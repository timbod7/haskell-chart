{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

-- | The backend to render charts with cairo.
module Graphics.Rendering.Chart.Backend.Cairo
  ( CRender
  , CairoBackend(..)
  , runBackend
  , renderToFile
  
  , bitmapEnv
  , vectorEnv

  , renderableToPNGFile
  , renderableToPDFFile
  , renderableToPSFile
  , renderableToSVGFile
  
  , sparkLineToPDF
  , sparkLineToPNG
  ) where

import Data.Default
import Data.Colour
import Data.Colour.SRGB
import Data.List (unfoldr)
import Data.Monoid

import Control.Monad.Reader

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as CM

import Graphics.Rendering.Chart.Backend as G
import Graphics.Rendering.Chart.Backend.Impl
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry as G
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Simple
import Graphics.Rendering.Chart.SparkLine

-- -----------------------------------------------------------------------
-- Backend and Monad
-- -----------------------------------------------------------------------

-- | The reader monad containing context information to control
--   the rendering process.
newtype CRender a = DR (C.Render a)
  deriving (Functor, Monad)

-- | Run this backends renderer.
runBackend :: ChartBackendEnv -- ^ Environment to start rendering with.
           -> ChartBackend a       -- ^ Chart render code.
           -> C.Render a      -- ^ Cairo render code.
runBackend env m = 
  let (DR c) = compileBackendM cStrokePath cFillPath cFillClip 
                               cTextSize cDrawText 
                               cWithTransform 
                               cWithLineStyle cWithFillStyle cWithFontStyle 
                               cWithClipRegion
                               env m
  in c

c :: C.Render a -> CRender a
c = DR

instance Monoid a => Monoid (CRender a) where
  mempty = return mempty
  mappend ma mb = do
    a <- ma
    b <- mb
    return $ a `mappend` b
    
cStrokePath :: ChartBackendEnv -> Path -> CRender ()
cStrokePath env p = preserveCState $ do
  cNewPath
  foldPath cMoveTo cLineTo cArc cArcNegative cClosePath p
  cSetSourceColor $ line_color_ $ cbeLineStyle env
  cStroke

cFillPath :: ChartBackendEnv -> Path -> CRender ()
cFillPath env p = preserveCState $ do
  cNewPath
  foldPath cMoveTo cLineTo cArc cArcNegative cClosePath p
  case cbeFillStyle env of
    FillStyleSolid cl -> cSetSourceColor cl
  cFill
  
cFillClip :: ChartBackendEnv -> CRender ()
cFillClip env = preserveCState $ do
  case cbeFillStyle env of
    FillStyleSolid cl -> cSetSourceColor cl
  cPaint

cTextSize :: ChartBackendEnv -> String -> CRender TextSize
cTextSize env text = do
  te <- c $ C.textExtents text
  fe <- c $ C.fontExtents
  return $ TextSize 
    { textSizeWidth    = C.textExtentsWidth te
    , textSizeAscent   = C.fontExtentsAscent fe
    , textSizeDescent  = C.fontExtentsDescent fe
    , textSizeYBearing = C.textExtentsYbearing te
    , textSizeHeight   = C.fontExtentsHeight fe
    }

cDrawText :: ChartBackendEnv -> Point -> String -> CRender ()
cDrawText env p text = preserveCState $ do
  cSetSourceColor $ font_color_ $ cbeFontStyle env
  cTranslate p
  cMoveTo $ Point 0 0
  cShowText text

cWithTransform :: ChartBackendEnv -> Change Matrix -> CRender a -> CRender a
cWithTransform env _ m = preserveCState $ cSetTransform (cbeTransform env) >> m

cWithLineStyle :: ChartBackendEnv -> Change LineStyle -> CRender a -> CRender a
cWithLineStyle env _ m = preserveCState $ setLineStyle (cbeLineStyle env) >> m

cWithFillStyle :: ChartBackendEnv -> Change FillStyle -> CRender a -> CRender a
cWithFillStyle env _ m = preserveCState $ setFillStyle (cbeFillStyle env) >> m

cWithFontStyle :: ChartBackendEnv -> Change FontStyle -> CRender a -> CRender a
cWithFontStyle env _ m = preserveCState $ setFontStyle (cbeFontStyle env) >> m

cWithClipRegion :: ChartBackendEnv -> Change (Limit Rect) -> CRender a -> CRender a
cWithClipRegion env _ m = preserveCState $ do
  case cbeClipRegion env of
    LMin -> setClipRegion (Rect (Point 0 0) (Point 0 0))
    LValue c -> setClipRegion c
    LMax -> c C.resetClip 
  m

-- -----------------------------------------------------------------------
-- Output rendering functions
-- -----------------------------------------------------------------------

data CairoBackend = CairoPNG | CairoSVG | CairoPS | CairoPDF

renderToFile :: ChartBackend a -> CairoBackend -> Int -> Int -> FilePath -> IO ()
renderToFile m b = case b of
  CairoPNG -> \w h f -> cRenderToPNGFile m w h f >> return ()
  CairoSVG -> cRenderToSVGFile m
  CairoPS  -> cRenderToPSFile  m
  CairoPDF -> cRenderToPDFFile m

-- | Output the given renderable to a PNG file of the specifed size
--   (in pixels), to the specified file.
renderableToPNGFile :: Renderable a -> Int -> Int -> FilePath -> IO (PickFn a)
renderableToPNGFile r width height path =
    cRenderToPNGFile cr width height path
  where
    cr = render r (fromIntegral width, fromIntegral height)

-- | Output the given renderable to a PDF file of the specifed size
--   (in points), to the specified file.
renderableToPDFFile :: Renderable a -> Int -> Int -> FilePath -> IO ()
renderableToPDFFile r width height path =
    cRenderToPDFFile cr width height path
  where
    cr = render r (fromIntegral width, fromIntegral height)

-- | Output the given renderable to a postscript file of the specifed size
--   (in points), to the specified file.
renderableToPSFile  :: Renderable a -> Int -> Int -> FilePath -> IO ()
renderableToPSFile r width height path  = 
    cRenderToPSFile cr width height path
  where
    cr = render r (fromIntegral width, fromIntegral height)

-- | Output the given renderable to an SVG file of the specifed size
--   (in points), to the specified file.
renderableToSVGFile :: Renderable a -> Int -> Int -> FilePath -> IO ()
renderableToSVGFile r width height path =
    cRenderToSVGFile cr width height path
  where
    cr = render r (fromIntegral width, fromIntegral height)

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

setClipRegion :: Rect -> CRender ()
setClipRegion (Rect p2 p3) = do    
    c $ C.moveTo (p_x p2) (p_y p2)
    c $ C.lineTo (p_x p2) (p_y p3)
    c $ C.lineTo (p_x p3) (p_y p3)
    c $ C.lineTo (p_x p3) (p_y p2)
    c $ C.lineTo (p_x p2) (p_y p2)
    c $ C.clip

setFontStyle :: FontStyle -> CRender ()
setFontStyle f = do
    c $ C.selectFontFace (G.font_name_ f) 
                         (convertFontSlant $ G.font_slant_ f) 
                         (convertFontWeight $ G.font_weight_ f)
    c $ C.setFontSize (G.font_size_ f)
    c $ setSourceColor (G.font_color_ f)

setLineStyle :: LineStyle -> CRender ()
setLineStyle ls = do
    c $ C.setLineWidth (G.line_width_ ls)
    c $ setSourceColor (G.line_color_ ls)
    c $ C.setLineCap (convertLineCap $ G.line_cap_ ls)
    c $ C.setLineJoin (convertLineJoin $ G.line_join_ ls)
    c $ C.setDash (G.line_dashes_ ls) 0

setFillStyle :: FillStyle -> CRender ()
setFillStyle (FillStyleSolid cl) = do
  c $ setSourceColor cl

colourChannel :: (Floating a, Ord a) => AlphaColour a -> Colour a
colourChannel c = darken (recip (alphaChannel c)) (c `over` black)

setSourceColor :: AlphaColour Double -> C.Render ()
setSourceColor c = let (RGB r g b) = toSRGB $ colourChannel c
                   in C.setSourceRGBA r g b (alphaChannel c)

-- | Execute a rendering action in a saved context (ie bracketed
--   between C.save and C.restore).
preserveCState :: CRender a -> CRender a
preserveCState a = do 
  c $ C.save
  v <- a
  c $ C.restore
  return v

-- -----------------------------------------------------------------------
-- Cairo Operation Wrappers
-- -----------------------------------------------------------------------
  
cSetTransform t = c $ C.setMatrix $ convertMatrix t

cTranslate :: Point -> CRender ()
cTranslate p = c $ C.translate (p_x p) (p_y p)

cRotate a = c $ C.rotate a
cNewPath = c $ C.newPath

cTransform :: Matrix -> CRender ()
cTransform t = c $ C.transform $ convertMatrix t

cLineTo :: Point -> CRender ()
cLineTo p = c $ C.lineTo (p_x p) (p_y p)

cMoveTo :: Point -> CRender ()
cMoveTo p = c $ C.moveTo (p_x p) (p_y p)

cRelLineTo x y = c $ C.relLineTo x y

cArc :: Point -> Double -> Double -> Double -> CRender ()
cArc p r a1 a2 = c $ C.arc (p_x p) (p_y p) r a1 a2

cArcNegative :: Point -> Double -> Double -> Double -> CRender ()
cArcNegative p r a1 a2 = c $ C.arcNegative (p_x p) (p_y p) r a1 a2

cClosePath = c $ C.closePath
cStroke = c $ C.stroke
cFill = c $ C.fill
cFillPreserve = c $ C.fillPreserve
cSetSourceColor color = c $ setSourceColor color
cPaint = c $ C.paint
cFontDescent = do
  fe <- c $ C.fontExtents
  return $ C.fontExtentsDescent fe
--cFontExtents = c $ C.fontExtents
--cFontExtentsDescent fe = C.fontExtentsDescent fe
cShowText s = c $ C.showText s

cRenderToPNGFile :: ChartBackend a -> Int -> Int -> FilePath -> IO a
cRenderToPNGFile cr width height path = 
    C.withImageSurface C.FormatARGB32 width height $ \result -> do
    a <- C.renderWith result $ runBackend bitmapEnv cr 
    C.surfaceWriteToPNG result path
    return a

cRenderToPDFFile :: ChartBackend a -> Int -> Int -> FilePath -> IO ()
cRenderToPDFFile = cRenderToFile C.withPDFSurface

cRenderToPSFile  ::  ChartBackend a -> Int -> Int -> FilePath -> IO ()
cRenderToPSFile  = cRenderToFile C.withPSSurface

cRenderToSVGFile  ::  ChartBackend a -> Int -> Int -> FilePath -> IO ()
cRenderToSVGFile  = cRenderToFile C.withSVGSurface

cRenderToFile withSurface cr width height path = 
    withSurface path (fromIntegral width) (fromIntegral height) $ \result -> do
    C.renderWith result $ do
      runBackend vectorEnv cr
      C.showPage
    C.surfaceFinish result

-- | Environment aligned to render good on bitmaps.
bitmapEnv :: ChartBackendEnv
bitmapEnv = defaultEnv (adjfn 0.5) (adjfn 0.0) 
  where
    adjfn offset (Point x y) = Point (adj x) (adj y)
      where
        adj v = (fromIntegral.round) v +offset

-- | Environment aligned to render good on vector based graphics.
vectorEnv :: ChartBackendEnv
vectorEnv = defaultEnv id id

-- -----------------------------------------------------------------------
-- Simple Instances
-- -----------------------------------------------------------------------

instance PlotPDFType (IO a) where
    pld fn args = do
        renderableToPDFFile (layout1DddToRenderable $ uplot (reverse args)) 640 480 fn
        return undefined

instance PlotPSType (IO a) where
    pls fn args = do
        renderableToPSFile (layout1DddToRenderable $ uplot (reverse args)) 640 480 fn
        return undefined

instance PlotPNGType (IO a) where
    plp fn args = do
        renderableToPNGFile (layout1DddToRenderable $ uplot (reverse args)) 640 480 fn
        return undefined

-- -----------------------------------------------------------------------
-- SparkLine Functions
-- -----------------------------------------------------------------------

-- | Generate a PNG for the sparkline, using its natural size.
sparkLineToPNG :: FilePath -> SparkLine -> IO (PickFn ())
sparkLineToPNG fp sp = renderableToPNGFile (sparkLineToRenderable sp)
                                           (sparkWidth sp)
                                           (so_height (sl_options sp))
                                           fp
-- | Generate a PDF for the sparkline, using its natural size.
sparkLineToPDF :: FilePath -> SparkLine -> IO ()
sparkLineToPDF fp sp = renderableToPDFFile (sparkLineToRenderable sp)
                                           (sparkWidth sp)
                                           (so_height (sl_options sp))
                                           fp