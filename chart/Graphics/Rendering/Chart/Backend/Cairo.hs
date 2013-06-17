{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

-- | The backend to render charts with cairo.
module Graphics.Rendering.Chart.Backend.Cairo
  ( CRender
  , runCRender

  , renderableToPNGFile
  , renderableToPDFFile
  , renderableToPSFile
  , renderableToSVGFile
  ) where

import Data.Default
import Data.Colour
import Data.Colour.SRGB
import Data.List (unfoldr)
import Data.Monoid

import Control.Monad.Reader

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as CM

import qualified Graphics.Rendering.Chart.Types as G
import Graphics.Rendering.Chart.Types 
  ( PointShape(..)
  , FillStyle(..), PointStyle(..), FontStyle(..), LineStyle(..)
  , HTextAnchor(..), VTextAnchor(..)
  )
import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry as G
import Graphics.Rendering.Chart.Renderable

-- -----------------------------------------------------------------------
-- Backend and Monad
-- -----------------------------------------------------------------------

data CairoBackend = CairoPNG | CairoSVG | CairoPS | CairoPDF

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
  type ChartOutput a = CairoBackend -> Int -> Int -> FilePath -> IO ()
  bNewPath = cNewPath
  bClosePath = cClosePath
  bMoveTo = cMoveTo
  bLineTo = cLineTo
  bRelLineTo p = cRelLineTo (p_x p) (p_y p)
  
  bArc = cArc
  bArcNegative = cArcNegative
  
  bTranslate = cTranslate
  bRotate = cRotate
  
  bStroke = cStroke
  bFill = cFill
  bFillPreserve = cFillPreserve
  bPaint = cPaint
  
  bLocal = preserveCState
  
  bSetSourceColor = cSetSourceColor
  
  bSetFontStyle = setFontStyle
  bSetFillStyle = setFillStyle
  
  runBackend m b = case b of
    CairoPNG -> \w h f -> cRenderToPNGFile m w h f >> return ()
    CairoSVG -> cRenderToSVGFile m
    CairoPS  -> cRenderToPSFile  m
    CairoPDF -> cRenderToPDFFile m
  
  backendStrokePath :: Bool -> Path -> CRender ()
  backendStrokePath close p = preserveCState $ do
    cNewPath
    foldPath cMoveTo cLineTo cArc cArcNegative p
    when close cClosePath
    cl <- line_color_ `fmap` getLineStyle
    cSetSourceColor cl
    cStroke
  
  backendFillPath :: Bool -> Path -> CRender ()
  backendFillPath close p = preserveCState $ do
    cNewPath
    foldPath cMoveTo cLineTo cArc cArcNegative p
    when close cClosePath
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
  
  withClipRegion :: Maybe Rect -> CRender a -> CRender a
  withClipRegion clip m = withClipRegion' clip
                        $ preserveCState 
                        $ case clip of
                            Just (Rect tl br) -> setClipRegion tl br >> m
                            Nothing -> c C.resetClip >> m

-- -----------------------------------------------------------------------
-- Output rendering functions
-- -----------------------------------------------------------------------

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

setClipRegion :: Point -> Point -> CRender ()
setClipRegion p2 p3 = do    
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

{- TODO: Obsolete?
-- | Function to draw a textual label anchored by one of its corners
--   or edges, with rotation. Rotation angle is given in degrees,
--   rotation is performed around anchor point.
drawTextR :: HTextAnchor -> VTextAnchor -> Double -> Point -> String -> CRender ()
drawTextR hta vta angle (Point x y) s = preserveCState $ draw
    where
      draw =  c $ do te <- C.textExtents s
                     fe <- C.fontExtents
                     let lx = xadj hta (C.textExtentsWidth te)
                     let ly = yadj vta te fe
                     C.translate x y
                     C.rotate theta
                     C.moveTo lx ly
                     C.showText s
      theta = angle*pi/180.0
      xadj HTA_Left   w = 0
      xadj HTA_Centre w = (-w/2)
      xadj HTA_Right  w = (-w)
      yadj VTA_Top      te fe = C.fontExtentsAscent fe
      yadj VTA_Centre   te fe = - (C.textExtentsYbearing te) / 2
      yadj VTA_BaseLine te fe = 0
      yadj VTA_Bottom   te fe = -(C.fontExtentsDescent fe)
-}
{- TODO: Obsolete?
-- | Function to draw a multi-line textual label anchored by one of its corners
--   or edges, with rotation. Rotation angle is given in degrees,
--   rotation is performed around anchor point.
drawTextsR :: HTextAnchor -> VTextAnchor -> Double -> Point -> String -> CRender ()
drawTextsR hta vta angle p@(Point x y) s = case num of
      0 -> return ()
      1 -> drawTextR hta vta angle p s
      _ -> preserveCState $ drawAll
    where
      ss   = lines s
      num  = length ss
      drawAll =  c $ do tes <- mapM C.textExtents ss
                        fe  <- C.fontExtents
                        let widths = map C.textExtentsWidth tes
                            maxw   = maximum widths
                            maxh   = maximum (map C.textExtentsYbearing tes)
                            gap    = maxh / 2 -- half-line spacing
                            totalHeight = fromIntegral num*maxh +
                                          (fromIntegral num-1)*gap
                            ys = take num (unfoldr (\y-> Just (y, y-gap-maxh))
                                                   (yinit vta fe totalHeight))
                            xs = map (xadj hta) widths
                        C.translate x y
                        C.rotate theta
                        sequence_ (zipWith3 draw xs ys ss)

      draw lx ly s =  do C.moveTo lx ly
                         C.showText s
      theta = angle*pi/180.0

      xadj HTA_Left   w = 0
      xadj HTA_Centre w = (-w/2)
      xadj HTA_Right  w = (-w)

      yinit VTA_Top      fe height = C.fontExtentsAscent fe
      yinit VTA_BaseLine fe height = 0
      yinit VTA_Centre   fe height = height / 2 + C.fontExtentsAscent fe
      yinit VTA_Bottom   fe height = height + C.fontExtentsAscent fe
-}

-- | Execute a rendering action in a saved context (ie bracketed
--   between C.save and C.restore).
preserveCState :: CRender a -> CRender a
preserveCState a = do 
  c $ C.save
  v <- a
  c $ C.restore
  return v

-- -----------------------------------------------------------------------

{- TODO: Obsolete?
drawPoint :: PointStyle -> Point -> CRender ()
drawPoint (PointStyle cl bcl bw r shape) p = do
  (Point x y) <- alignp p
  case shape of
    PointShapeCircle -> do
      c $ setSourceColor cl
      c $ C.newPath
      c $ C.arc x y r 0 (2*pi)
      c $ C.fillPreserve
    PointShapePolygon sides isrot -> do
      c $ setSourceColor cl
      c $ C.newPath
      let intToAngle n =
            if isrot
            then       fromIntegral n * 2*pi/fromIntegral sides
            else (0.5 + fromIntegral n)*2*pi/fromIntegral sides
          angles = map intToAngle [0 .. sides-1]
          (p:ps) = map (\a -> Point (x + r * sin a)
                                    (y + r * cos a)) angles
      moveTo p
      mapM_ lineTo (ps++[p])
      c $ C.fillPreserve
    PointShapePlus -> do
      c $ C.newPath
      c $ C.moveTo (x+r) y
      c $ C.lineTo (x-r) y
      c $ C.moveTo x (y-r)
      c $ C.lineTo x (y+r)
    PointShapeCross -> do
      let rad = r / sqrt 2
      c $ C.newPath
      c $ C.moveTo (x+rad) (y+rad)
      c $ C.lineTo (x-rad) (y-rad)
      c $ C.moveTo (x+rad) (y-rad)
      c $ C.lineTo (x-rad) (y+rad)
    PointShapeStar -> do
      let rad = r / sqrt 2
      c $ C.newPath
      c $ C.moveTo (x+r) y
      c $ C.lineTo (x-r) y
      c $ C.moveTo x (y-r)
      c $ C.lineTo x (y+r)
      c $ C.moveTo (x+rad) (y+rad)
      c $ C.lineTo (x-rad) (y-rad)
      c $ C.moveTo (x+rad) (y-rad)
      c $ C.lineTo (x-rad) (y+rad)
  c $ C.setLineWidth bw
  c $ setSourceColor bcl
  c $ C.stroke
  -}
  
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




