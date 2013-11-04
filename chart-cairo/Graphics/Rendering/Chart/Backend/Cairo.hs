{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The backend to render charts with cairo.
module Graphics.Rendering.Chart.Backend.Cairo
  ( FileFormat(..)
  , FileOptions(..)
  , runBackend
  , renderableToFile
  , defaultEnv

  , fo_size
  , fo_format

  , cBackendToFile

  , renderableToPNGFile  -- deprecated
  , renderableToPDFFile  -- deprecated
  , renderableToPSFile   -- deprecated
  , renderableToSVGFile  -- deprecated
  
  , sparkLineToPDF       -- deprecated
  , sparkLineToPNG       -- deprecated
  ) where

import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.List (unfoldr)
import Data.Monoid

import Control.Lens(makeLenses)
import Control.Monad.Reader
import Control.Monad.Operational

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as CM

import Graphics.Rendering.Chart.Backend as G
import Graphics.Rendering.Chart.Backend.Impl
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry as G
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Simple
import Graphics.Rendering.Chart.SparkLine

-----------------------------------------------------------------------
-- Rendering Backend Environment
-----------------------------------------------------------------------

-- | The environment we need to track when rendering to cairo
data CEnv = CEnv
  { ceAlignmentFns :: AlignmentFns
  , ceFontColor :: AlphaColour Double
  , cePathColor :: AlphaColour Double
  , ceFillColor :: AlphaColour Double
}

-- | Produce a environment with no transformation and clipping. 
--   It will use the default styles.
defaultEnv :: AlignmentFns
           -> CEnv
defaultEnv alignFns = CEnv 
  { ceAlignmentFns = alignFns
  , ceFontColor = opaque black
  , cePathColor = opaque black
  , ceFillColor = opaque white
  }

-- -----------------------------------------------------------------------
-- Backend and Monad
-- -----------------------------------------------------------------------

-- | Run this backends renderer.
runBackend :: CEnv -- ^ Environment to start rendering with.
           -> ChartBackend a  -- ^ Chart render code.
           -> C.Render a      -- ^ Cairo render code.
runBackend env m = runBackend' env (withDefaultStyle m)

runBackend' :: CEnv -> ChartBackend a -> C.Render a
runBackend' env m = eval env (view m)
  where
    eval :: CEnv -> ProgramView ChartBackendInstr a -> C.Render a
    eval env (Return v)= return v
    eval env (StrokePath p :>>= f) = cStrokePath env p >>= step env f
    eval env (FillPath p :>>= f) = cFillPath env p >>= step env f
    eval env (GetTextSize s :>>= f) = cTextSize s >>= step env f
    eval env (DrawText p s :>>= f) = cDrawText env p s >>= step env f
    eval env (GetAlignments :>>= f) = return (ceAlignmentFns env) >>= step env f
    eval env (WithTransform m p :>>= f) = cWithTransform env m p >>= step env f
    eval env (WithFontStyle font p :>>= f) = cWithFontStyle env font p >>= step env f
    eval env (WithFillStyle fs p :>>= f) = cWithFillStyle env fs p >>= step env f
    eval env (WithLineStyle ls p :>>= f) = cWithLineStyle env ls p >>= step env f
    eval env (WithClipRegion r p :>>= f) = cWithClipRegion env r p >>= step env f

    step :: CEnv -> (v -> ChartBackend a) -> v -> C.Render a
    step env f =  \v -> runBackend' env (f v)
    
walkPath :: Path -> C.Render ()
walkPath (MoveTo p path) = C.moveTo (p_x p) (p_y p) >> walkPath path
walkPath (LineTo p path) = C.lineTo (p_x p) (p_y p) >> walkPath path
walkPath (Arc p r a1 a2 path) = C.arc (p_x p) (p_y p) r a1 a2 >> walkPath path
walkPath (ArcNeg p r a1 a2 path) = C.arcNegative (p_x p) (p_y p) r a1 a2 >> walkPath path
walkPath End = return ()
walkPath Close = C.closePath

cStrokePath :: CEnv -> Path -> C.Render ()
cStrokePath env p = preserveCState0 $ do
    setSourceColor (cePathColor env)
    C.newPath >> walkPath p >> C.stroke

cFillPath :: CEnv -> Path -> C.Render ()
cFillPath env p = preserveCState0 $ do
    setSourceColor (ceFillColor env)
    C.newPath >> walkPath p >> C.fill

cTextSize :: String -> C.Render TextSize
cTextSize text = do
  te <- C.textExtents text
  fe <- C.fontExtents
  return $ TextSize 
    { textSizeWidth    = C.textExtentsWidth te
    , textSizeAscent   = C.fontExtentsAscent fe
    , textSizeDescent  = C.fontExtentsDescent fe
    , textSizeYBearing = C.textExtentsYbearing te
    , textSizeHeight   = C.fontExtentsHeight fe
    }

cDrawText :: CEnv -> Point -> String -> C.Render ()
cDrawText env p text = preserveCState0 $ do
  setSourceColor $ (ceFontColor env)
  cTranslate p
  C.moveTo 0 0
  C.showText text

cWithTransform :: CEnv -> Matrix -> ChartBackend a -> C.Render a
cWithTransform env m p = preserveCState0 $ do
  C.transform (convertMatrix m)
  runBackend' env p

cWithFontStyle :: CEnv -> FontStyle -> ChartBackend a -> C.Render a
cWithFontStyle env font p = preserveCState0 $ do
  C.selectFontFace (G._font_name font) 
                   (convertFontSlant $ G._font_slant font) 
                   (convertFontWeight $ G._font_weight font)
  C.setFontSize (G._font_size font)
  runBackend' env{ceFontColor=G._font_color font} p

cWithFillStyle :: CEnv -> FillStyle -> ChartBackend a -> C.Render a
cWithFillStyle env fs p = do
  runBackend' env{ceFillColor=G._fill_colour fs} p

cWithLineStyle :: CEnv -> LineStyle -> ChartBackend a -> C.Render a
cWithLineStyle env ls p = preserveCState0 $ do
  C.setLineWidth (G._line_width ls)
  C.setLineCap (convertLineCap $ G._line_cap ls)
  C.setLineJoin (convertLineJoin $ G._line_join ls)
  C.setDash (G._line_dashes ls) 0
  runBackend' env{cePathColor=G._line_color ls} p

cWithClipRegion :: CEnv -> Rect -> ChartBackend a -> C.Render a
cWithClipRegion env r p = preserveCState0 $ do
  setClipRegion r
  runBackend' env p

-- -----------------------------------------------------------------------
-- Output rendering functions
-- -----------------------------------------------------------------------

data FileFormat = PNG
                | SVG
                | PS
                | PDF

data FileOptions = FileOptions {
  _fo_size :: (Int,Int),
  _fo_format :: FileFormat
}

instance Default FileOptions where
  def =  FileOptions (800,600) PNG

-- | Generate an image file for the given renderable, at the specified path. Size and
-- format are set through the `FileOptions` parameter.
renderableToFile :: FileOptions -> Renderable a -> FilePath -> IO (PickFn a)
renderableToFile fo r path = cBackendToFile fo cr path
  where
    cr = render r (fromIntegral width, fromIntegral height)
    (width,height) = _fo_size fo

-- | Generate an image file for the given drawing instructions, at the specified path. Size and
-- format are set through the `FileOptions` parameter.
cBackendToFile :: FileOptions -> ChartBackend a -> FilePath -> IO a
cBackendToFile fo cr path = do
    case (_fo_format fo) of
      PS -> write C.withPSSurface
      PDF -> write C.withPDFSurface
      SVG -> write C.withSVGSurface
      PNG -> writePNG
  where
    write withSurface = do
      withSurface path (fromIntegral width) (fromIntegral height) $ \result -> do
      pf <- C.renderWith result $ do
        pf <- runBackend (defaultEnv vectorAlignmentFns) cr
        C.showPage
        return pf
      C.surfaceFinish result
      return pf

    writePNG = C.withImageSurface C.FormatARGB32 width height $ \result -> do
      pf <- C.renderWith result $ runBackend (defaultEnv bitmapAlignmentFns) cr
      C.surfaceWriteToPNG result path
      return pf

    (width,height) = _fo_size fo

{-# DEPRECATED renderableToPNGFile "use renderableToFile" #-}
renderableToPNGFile :: Renderable a -> Int -> Int -> FilePath -> IO (PickFn a)
renderableToPNGFile r width height path = renderableToFile (FileOptions (width,height) PNG) r path

-- | Output the given renderable to a PDF file of the specifed size
--   (in points), to the specified file.
{-# DEPRECATED renderableToPDFFile "use renderableToFile" #-}
renderableToPDFFile :: Renderable a -> Int -> Int -> FilePath -> IO ()
renderableToPDFFile r width height path = void $ renderableToFile (FileOptions (width,height) PDF) r path

-- | Output the given renderable to a postscript file of the specifed size
--   (in points), to the specified file.
{-# DEPRECATED renderableToPSFile "use renderableToFile" #-}
renderableToPSFile  :: Renderable a -> Int -> Int -> FilePath -> IO ()
renderableToPSFile r width height path  = void $ renderableToFile (FileOptions (width,height) PS) r path

-- | Output the given renderable to an SVG file of the specifed size
--   (in points), to the specified file.
{-# DEPRECATED renderableToSVGFile "use renderableToFile" #-}
renderableToSVGFile :: Renderable a -> Int -> Int -> FilePath -> IO ()
renderableToSVGFile r width height path = void $ renderableToFile (FileOptions (width,height) SVG) r path

-- -----------------------------------------------------------------------
-- Type Conversions: Chart -> Cairo
-- -----------------------------------------------------------------------

-- | Convert a charts line join to a cairo line join.
convertLineJoin :: G.LineJoin -> C.LineJoin
convertLineJoin lj = case lj of
  G.LineJoinMiter -> C.LineJoinMiter
  G.LineJoinRound -> C.LineJoinRound
  G.LineJoinBevel -> C.LineJoinBevel

-- | Convert a charts line cap to a cairo line cap.
convertLineCap :: G.LineCap -> C.LineCap
convertLineCap lc = case lc of
  G.LineCapRound  -> C.LineCapRound
  G.LineCapButt   -> C.LineCapButt
  G.LineCapSquare -> C.LineCapSquare

convertFontSlant :: G.FontSlant -> C.FontSlant
convertFontSlant fs = case fs of
  G.FontSlantItalic  -> C.FontSlantItalic
  G.FontSlantNormal  -> C.FontSlantNormal
  G.FontSlantOblique -> C.FontSlantOblique

convertFontWeight :: G.FontWeight -> C.FontWeight
convertFontWeight fw = case fw of
  G.FontWeightBold   -> C.FontWeightBold
  G.FontWeightNormal -> C.FontWeightNormal

convertMatrix :: G.Matrix -> CM.Matrix
convertMatrix (G.Matrix a1 a2 b1 b2 c1 c2) = CM.Matrix a1 a2 b1 b2 c1 c2

-- -----------------------------------------------------------------------
-- Assorted helper functions in Cairo Usage
-- -----------------------------------------------------------------------

setClipRegion :: Rect -> C.Render ()
setClipRegion (Rect p2 p3) = do    
    C.moveTo (p_x p2) (p_y p2)
    C.lineTo (p_x p2) (p_y p3)
    C.lineTo (p_x p3) (p_y p3)
    C.lineTo (p_x p3) (p_y p2)
    C.lineTo (p_x p2) (p_y p2)
    C.clip

colourChannel :: (Floating a, Ord a) => AlphaColour a -> Colour a
colourChannel c = darken (recip (alphaChannel c)) (c `over` black)

setSourceColor :: AlphaColour Double -> C.Render ()
setSourceColor c = let (RGB r g b) = toSRGB $ colourChannel c
                   in C.setSourceRGBA r g b (alphaChannel c)

-- | Execute a rendering action in a saved context (ie bracketed
--   between C.save and C.restore).
preserveCState0 :: C.Render a -> C.Render a
preserveCState0 a = do 
  C.save
  v <- a
  C.restore
  return v

-- -- -----------------------------------------------------------------------
-- -- Cairo Operation Wrappers
-- -- -----------------------------------------------------------------------
  
cTranslate :: Point -> C.Render ()
cTranslate p = C.translate (p_x p) (p_y p)

cLineTo :: Point -> C.Render ()
cLineTo p = C.lineTo (p_x p) (p_y p)

cMoveTo :: Point -> C.Render ()
cMoveTo p = C.moveTo (p_x p) (p_y p)

cArc :: Point -> Double -> Double -> Double -> C.Render ()
cArc p r a1 a2 = C.arc (p_x p) (p_y p) r a1 a2

cArcNegative :: Point -> Double -> Double -> Double -> C.Render ()
cArcNegative p r a1 a2 = C.arcNegative (p_x p) (p_y p) r a1 a2

-- -----------------------------------------------------------------------
-- Simple Instances
-- -----------------------------------------------------------------------

instance PlotPDFType (IO a) where
    pld fn args = do
        renderableToPDFFile (layoutDddToRenderable $ uplot (reverse args)) 640 480 fn
        return undefined

instance PlotPSType (IO a) where
    pls fn args = do
        renderableToPSFile (layoutDddToRenderable $ uplot (reverse args)) 640 480 fn
        return undefined

instance PlotPNGType (IO a) where
    plp fn args = do
        renderableToPNGFile (layoutDddToRenderable $ uplot (reverse args)) 640 480 fn
        return undefined

-- -----------------------------------------------------------------------
-- SparkLine Functions
-- -----------------------------------------------------------------------

-- | Generate a PNG for the sparkline, using its natural size.
{-# DEPRECATED sparkLineToPNG "use renderableToFile" #-}
sparkLineToPNG :: FilePath -> SparkLine -> IO (PickFn ())
sparkLineToPNG fp sp = renderableToFile (FileOptions (sparkSize sp) PNG) (sparkLineToRenderable sp) fp

-- | Generate a PDF for the sparkline, using its natural size.
{-# DEPRECATED sparkLineToPDF "use renderableToFile" #-}
sparkLineToPDF :: FilePath -> SparkLine -> IO ()
sparkLineToPDF fp sp = void $ renderableToFile (FileOptions (sparkSize sp) PDF) (sparkLineToRenderable sp) fp

$( makeLenses ''FileOptions )
