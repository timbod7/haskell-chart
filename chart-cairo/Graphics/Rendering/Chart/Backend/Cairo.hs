{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

-- | The backend to render charts with cairo.
module Graphics.Rendering.Chart.Backend.Cairo
  ( CRender
  , CairoBackend(..)
  , runBackend
  , renderToFile

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
newtype CRender a = DR (ReaderT ChartBackendEnv C.Render a)
  deriving (Functor, Monad, MonadReader ChartBackendEnv)

runCRender :: CRender a -> ChartBackendEnv -> C.Render a
runCRender (DR m) e = runReaderT m e

c :: C.Render a -> CRender a
c = DR . lift

instance Monoid a => Monoid (CRender a) where
  mempty = return mempty
  mappend ma mb = do
    a <- ma
    b <- mb
    return $ a `mappend` b

instance ChartBackend CRender where
  
  
  strokePath :: Path -> CRender ()
  strokePath p = preserveCState $ do
    cNewPath
    foldPath cMoveTo cLineTo cArc cArcNegative cClosePath p
    cl <- line_color_ `fmap` getLineStyle
    cSetSourceColor cl
    cStroke
  
  fillPath :: Path -> CRender ()
  fillPath p = preserveCState $ do
    cNewPath
    foldPath cMoveTo cLineTo cArc cArcNegative cClosePath p
    fs <- getFillStyle
    case fs of
      FillStyleSolid cl -> cSetSourceColor cl
    cFill
  
  fillClip :: CRender ()
  fillClip = do
    fs <- getFillStyle
    case fs of
      FillStyleSolid cl -> cSetSourceColor cl
    cPaint
  
  textSize :: String -> CRender TextSize
  textSize s = do
    te <- c $ C.textExtents s
    fe <- c $ C.fontExtents
    return $ TextSize 
      { textSizeWidth    = C.textExtentsWidth te
      , textSizeAscent   = C.fontExtentsAscent fe
      , textSizeDescent  = C.fontExtentsDescent fe
      , textSizeYBearing = C.textExtentsYbearing te
      , textSizeHeight   = C.fontExtentsHeight fe
      }
  
  drawText :: Point -> String -> CRender ()
  drawText p s = preserveCState $ do
    cTranslate p
    cMoveTo $ Point 0 0
    cShowText s
  
  withTransform :: Matrix -> CRender a -> CRender a
  withTransform t m = withTransform' t 
                    $ preserveCState 
                    $ cTransform t >> m
  
  withFontStyle :: FontStyle -> CRender a -> CRender a
  withFontStyle fs m = withFontStyle' fs
                     $ preserveCState 
                     $ setFontStyle fs >> m
  
  withFillStyle :: FillStyle -> CRender a -> CRender a
  withFillStyle fs m = withFillStyle' fs
                     $ preserveCState 
                     $ setFillStyle fs >> m
  
  withLineStyle :: LineStyle -> CRender a -> CRender a
  withLineStyle ls m = withLineStyle' ls
                     $ preserveCState 
                     $ setLineStyle ls >> m
  
  withClipRegion :: Rect -> CRender a -> CRender a
  withClipRegion clip m = withClipRegion' clip
                        $ preserveCState $ do
                            clip' <- getClipRegion
                            case clip' of
                              Just c -> setClipRegion c >> m
                              Nothing -> c C.resetClip >> m

-- -----------------------------------------------------------------------
-- Output rendering functions
-- -----------------------------------------------------------------------

-- | Run this backends renderer.
runBackend :: (Point -> Point) -- ^ Point alignment function.
           -> (Point -> Point) -- ^ Coordinate alignment function.
           -> CRender a        -- ^ Chart render code.
           -> C.Render a       -- ^ Cairo render code.
runBackend pAlignF cAlignF m = runCRender m $ defaultEnv pAlignF cAlignF

data CairoBackend = CairoPNG | CairoSVG | CairoPS | CairoPDF

renderToFile :: CRender a -> CairoBackend -> Int -> Int -> FilePath -> IO ()
renderToFile m b = case b of
  CairoPNG -> \w h f -> cRenderToPNGFile m w h f >> return ()
  CairoSVG -> cRenderToSVGFile m
  CairoPS  -> cRenderToPSFile  m
  CairoPDF -> cRenderToPDFFile m

-- | Output the given renderable to a PNG file of the specifed size
--   (in pixels), to the specified file.
renderableToPNGFile :: Renderable CRender a -> Int -> Int -> FilePath -> IO (PickFn a)
renderableToPNGFile r width height path =
    cRenderToPNGFile cr width height path
  where
    cr = render r (fromIntegral width, fromIntegral height)

-- | Output the given renderable to a PDF file of the specifed size
--   (in points), to the specified file.
renderableToPDFFile :: Renderable CRender a -> Int -> Int -> FilePath -> IO ()
renderableToPDFFile r width height path =
    cRenderToPDFFile cr width height path
  where
    cr = render r (fromIntegral width, fromIntegral height)

-- | Output the given renderable to a postscript file of the specifed size
--   (in points), to the specified file.
renderableToPSFile  :: Renderable CRender a -> Int -> Int -> FilePath -> IO ()
renderableToPSFile r width height path  = 
    cRenderToPSFile cr width height path
  where
    cr = render r (fromIntegral width, fromIntegral height)

-- | Output the given renderable to an SVG file of the specifed size
--   (in points), to the specified file.
renderableToSVGFile :: Renderable CRender a -> Int -> Int -> FilePath -> IO ()
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

-- | Function to draw a textual label anchored by one of its corners
--   or edges.
drawText :: HTextAnchor -> VTextAnchor -> Point -> String -> CRender ()
drawText hta vta p s = drawTextR hta vta 0 p s

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

cRenderToPNGFile :: CRender a -> Int -> Int -> FilePath -> IO a
cRenderToPNGFile cr width height path = 
    C.withImageSurface C.FormatARGB32 width height $ \result -> do
    a <- C.renderWith result $ runCRender cr bitmapEnv
    C.surfaceWriteToPNG result path
    return a

cRenderToPDFFile :: CRender a -> Int -> Int -> FilePath -> IO ()
cRenderToPDFFile = cRenderToFile C.withPDFSurface

cRenderToPSFile  ::  CRender a -> Int -> Int -> FilePath -> IO ()
cRenderToPSFile  = cRenderToFile C.withPSSurface

cRenderToSVGFile  ::  CRender a -> Int -> Int -> FilePath -> IO ()
cRenderToSVGFile  = cRenderToFile C.withSVGSurface

cRenderToFile withSurface cr width height path = 
    withSurface path (fromIntegral width) (fromIntegral height) $ \result -> do
    C.renderWith result $ runCRender rfn vectorEnv
    C.surfaceFinish result
  where
    rfn = do
        cr
        c $ C.showPage

bitmapEnv :: ChartBackendEnv
bitmapEnv = defaultEnv (adjfn 0.5) (adjfn 0.0) 
  where
    adjfn offset (Point x y) = Point (adj x) (adj y)
      where
        adj v = (fromIntegral.round) v +offset

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