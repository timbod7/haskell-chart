{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Drawing
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- This module contains basic types and functions used for drawing.
--
-- Note that template haskell is used to derive accessor functions
-- (see 'Data.Accessor') for each field of the following data types:
--
--    * 'CairoLineStyle'
--
--    * 'CairoFontStyle'
--
-- These accessors are not shown in this API documentation.  They have
-- the same name as the field, but with the trailing underscore
-- dropped. Hence for data field f_::F in type D, they have type
--
-- @
--   f :: Data.Accessor.Accessor D F
-- @
--

module Graphics.Rendering.Chart.Drawing(
    preserveCState,
    setClipRegion,
    moveTo,
    lineTo,
    strokePath,
    fillPath,

    defaultColorSeq,

    setSourceColor,
    
    LineCap(..),

    CairoLineStyle(..),
    solidLine,
    dashedLine,
    setLineStyle,

    CairoFillStyle(..),
    defaultPointStyle,
    solidFillStyle,
    setFillStyle,

    CairoFontStyle(..),
    defaultFontStyle,
    setFontStyle,

    CairoPointStyle(..),
    filledPolygon,
    hollowPolygon,
    filledCircles,
    hollowCircles,
    plusses,
    exes,
    stars,

    HTextAnchor(..),
    VTextAnchor(..),
    drawText,
    drawTextR,
    drawTextsR,
    textSize,
    textDrawRect,

    CRender,
    CEnv(..),
    runCRender,
    alignp,
    alignc,

    vectorEnv,
    bitmapEnv,

    cTranslate,
    cRotate,
    cNewPath,
    cMoveTo,
    cLineTo,
    cRelLineTo,
    cArc,
    cArcNegative,
    cClosePath,
    cStroke,
    cFill,
    cFillPreserve,
    cSetSourceColor,
    cPaint,
    cFontExtents,
    cFontExtentsDescent,
    cShowText,

    cRenderToPNGFile,
    cRenderToPSFile,
    cRenderToPDFFile,
    cRenderToSVGFile,
    
    line_width,
    line_color,
    line_dashes,
    line_cap,
    line_join,

    font_name,
    font_size,
    font_slant,
    font_weight,
    font_color,

    FontWeight(..),
    LineCap(..),
    LineJoin(..),
) where

import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo(FontWeight,LineCap,LineJoin)
                                
import Control.Monad.Reader
import Data.Accessor
import Data.Accessor.Template
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Data.List (unfoldr)

import Graphics.Rendering.Chart.Geometry

-- | The environment present in the CRender Monad.
data CEnv = CEnv {
    -- | An adjustment applied immediately prior to points
    --   being displayed in device coordinates.
    --
    --   When device coordinates correspond to pixels, a cleaner
    --   image is created if this transform rounds to the nearest
    --   pixel. With higher-resolution output, this transform can
    --   just be the identity function.
    cenv_point_alignfn :: Point -> Point,

    -- | A adjustment applied immediately prior to coordinates
    --   being transformed.
    cenv_coord_alignfn :: Point -> Point
}

-- | The reader monad containing context information to control
--   the rendering process.
newtype CRender a = DR (ReaderT CEnv C.Render a)
  deriving (Functor, Monad, MonadReader CEnv)

runCRender :: CRender a -> CEnv -> C.Render a
runCRender (DR m) e = runReaderT m e

c :: C.Render a -> CRender a
c = DR . lift
 
----------------------------------------------------------------------

-- | The different supported line ends.
data LineCap = LineCapButt   -- ^ Just cut the line straight.
             | LineCapRound  -- ^ Make a rounded line end.
             | LineCapSquare -- ^ Make a square that ends the line.

-- | The different supported ways to join line ends.
data LineJoin = LineJoinMiter -- ^ Extends the outline until they meet each other.
              | LineJoinRound -- ^ Draw a circle fragment to connet line end.
              | LineJoinBevel -- ^ Like miter, but cuts it off if a certain 
                              --   threshold is exceeded.

-- | Abstract data type for the style of a plotted point.
--
--   The contained Cairo action draws a point in the desired
--   style, at the supplied device coordinates.
newtype CairoPointStyle = CairoPointStyle (Point -> CRender ())

-- | Data type for the style of a line.
data CairoLineStyle = CairoLineStyle {
   line_width_  :: Double,
   line_color_  :: AlphaColour Double,
   line_dashes_ :: [Double],
   line_cap_    :: C.LineCap,
   line_join_   :: C.LineJoin
}

-- | Abstract data type for a fill style.
--
--   The contained Cairo action sets the required fill
--   style in the Cairo rendering state.
newtype CairoFillStyle = CairoFillStyle (CRender ())

-- | Data type for a font.
data CairoFontStyle = CairoFontStyle {
      font_name_   :: String,
      font_size_   :: Double,
      font_slant_  :: C.FontSlant,
      font_weight_ :: C.FontWeight,
      font_color_  :: AlphaColour Double
}

defaultColorSeq :: [AlphaColour Double]
defaultColorSeq = cycle $ map opaque [blue, red, green, yellow, cyan, magenta]

----------------------------------------------------------------------
-- Assorted helper functions in Cairo Usage

moveTo, lineTo :: Point -> CRender ()
moveTo p  = do
    p' <- alignp p
    c $ C.moveTo (p_x p') (p_y p')

alignp :: Point -> CRender Point
alignp p = do 
    alignfn <- fmap cenv_point_alignfn ask
    return (alignfn p)

alignc :: Point -> CRender Point
alignc p = do 
    alignfn <- fmap cenv_coord_alignfn ask
    return (alignfn p)

lineTo p = do
    p' <- alignp p
    c $ C.lineTo (p_x p') (p_y p')

setClipRegion :: Point -> Point -> CRender ()
setClipRegion p2 p3 = do    
    c $ C.moveTo (p_x p2) (p_y p2)
    c $ C.lineTo (p_x p2) (p_y p3)
    c $ C.lineTo (p_x p3) (p_y p3)
    c $ C.lineTo (p_x p3) (p_y p2)
    c $ C.lineTo (p_x p2) (p_y p2)
    c $ C.clip

stepPath :: [Point] -> CRender()
stepPath (p:ps) = c $ do
    C.newPath                    
    C.moveTo (p_x p) (p_y p)
    mapM_ (\p -> C.lineTo (p_x p) (p_y p)) ps
stepPath _  = return ()

-- | Draw lines between the specified points.
--
-- The points will be "corrected" by the cenv_point_alignfn, so that
-- when drawing bitmaps, 1 pixel wide lines will be centred on the
-- pixels.
strokePath :: [Point] -> CRender()
strokePath pts = do
    alignfn <- fmap cenv_point_alignfn ask
    stepPath (map alignfn pts)
    c $ C.stroke

-- | Fill the region with the given corners.
--
-- The points will be "corrected" by the cenv_coord_alignfn, so that
-- when drawing bitmaps, the edges of the region will fall between
-- pixels.
fillPath ::  [Point] -> CRender()
fillPath pts = do
    alignfn <- fmap cenv_coord_alignfn ask
    stepPath (map alignfn pts)
    c $ C.fill

setFontStyle :: CairoFontStyle -> CRender ()
setFontStyle f = do
    c $ C.selectFontFace (font_name_ f) (font_slant_ f) (font_weight_ f)
    c $ C.setFontSize (font_size_ f)
    c $ setSourceColor (font_color_ f)

setLineStyle :: CairoLineStyle -> CRender ()
setLineStyle ls = do
    c $ C.setLineWidth (line_width_ ls)
    c $ setSourceColor (line_color_ ls)
    c $ C.setLineCap (line_cap_ ls)
    c $ C.setLineJoin (line_join_ ls)
    c $ C.setDash (line_dashes_ ls) 0

setFillStyle :: CairoFillStyle -> CRender ()
setFillStyle (CairoFillStyle s) = s

colourChannel :: (Floating a, Ord a) => AlphaColour a -> Colour a
colourChannel c = darken (recip (alphaChannel c)) (c `over` black)

setSourceColor :: AlphaColour Double -> C.Render ()
setSourceColor c = let (RGB r g b) = toSRGB $ colourChannel c
                   in C.setSourceRGBA r g b (alphaChannel c)

-- | Return the bounding rectangle for a text string rendered
--   in the current context.
textSize :: String -> CRender RectSize
textSize s = c $ do
    te <- C.textExtents s
    fe <- C.fontExtents
    return (C.textExtentsWidth te, C.fontExtentsHeight fe)

data HTextAnchor = HTA_Left | HTA_Centre | HTA_Right
data VTextAnchor = VTA_Top | VTA_Centre | VTA_Bottom | VTA_BaseLine

-- | Recturn the bounding rectangle for a text string positioned
--   where it would be drawn by drawText
textDrawRect :: HTextAnchor -> VTextAnchor -> Point -> String -> CRender Rect
textDrawRect hta vta (Point x y) s = preserveCState $ textSize s >>= rect
    where
      rect (w,h) = c $ do te <- C.textExtents s
                          fe <- C.fontExtents
                          let lx = xadj hta (C.textExtentsWidth te)
                          let ly = yadj vta te fe
                          let (x',y') = (x + lx, y + ly)
                          let p1 = Point x' y'
                          let p2 = Point (x' + w) (y' + h)
                          return $ Rect p1 p2

      xadj HTA_Left   w = 0
      xadj HTA_Centre w = (-w/2)
      xadj HTA_Right  w = (-w)
      yadj VTA_Top      te fe = C.fontExtentsAscent fe
      yadj VTA_Centre   te fe = - (C.textExtentsYbearing te) / 2
      yadj VTA_BaseLine te fe = 0
      yadj VTA_Bottom   te fe = -(C.fontExtentsDescent fe)

-- | Function to draw a textual label anchored by one of its corners
--   or edges.
drawText :: HTextAnchor -> VTextAnchor -> Point -> String -> CRender ()
drawText hta vta p s = drawTextR hta vta 0 p s

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

-- | Function to draw a multi-line textual label anchored by one of its corners
--   or edges, with rotation. Rotation angle is given in degrees,
--   rotation is performed around anchor point.
drawTextsR :: HTextAnchor -> VTextAnchor -> Double -> Point -> String -> CRender ()
drawTextsR hta vta angle (Point x y) s = preserveCState $ drawAll
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


-- | Execute a rendering action in a saved context (ie bracketed
--   between C.save and C.restore).
preserveCState :: CRender a -> CRender a
preserveCState a = do 
  c $ C.save
  v <- a
  c $ C.restore
  return v

----------------------------------------------------------------------

filledCircles ::
     Double             -- ^ Radius of circle.
  -> AlphaColour Double -- ^ Colour.
  -> CairoPointStyle
filledCircles radius cl = CairoPointStyle rf
  where
    rf p = do
        (Point x y) <- alignp p
	c $ setSourceColor cl
        c $ C.newPath
	c $ C.arc x y radius 0 (2*pi)
	c $ C.fill

hollowCircles ::
     Double -- ^ Radius of circle.
  -> Double -- ^ Thickness of line.
  -> AlphaColour Double
  -> CairoPointStyle
hollowCircles radius w cl = CairoPointStyle rf
  where
    rf p = do
        (Point x y) <- alignp p
        c $ C.setLineWidth w
	c $ setSourceColor cl
        c $ C.newPath
	c $ C.arc x y radius 0 (2*pi)
	c $ C.stroke

hollowPolygon ::
     Double -- ^ Radius of circle.
  -> Double -- ^ Thickness of line.
  -> Int    -- ^ Number of vertices.
  -> Bool   -- ^ Is right-side-up?
  -> AlphaColour Double
  -> CairoPointStyle
hollowPolygon radius w sides isrot cl = CairoPointStyle rf
  where rf p =
            do (Point x y ) <- alignp p
               c $ C.setLineWidth w
	       c $ setSourceColor cl
               c $ C.newPath
               let intToAngle n =
                         if isrot
                         then       fromIntegral n * 2*pi / fromIntegral sides
                         else (0.5 + fromIntegral n)*2*pi / fromIntegral sides
                   angles = map intToAngle [0 .. sides-1]
                   (p:ps) = map (\a -> Point (x + radius * sin a)
                                             (y + radius * cos a))
                                angles
               moveTo p
               mapM_ lineTo (ps++[p])
	       c $ C.stroke

filledPolygon ::
     Double -- ^ Radius of circle.
  -> Int    -- ^ Number of vertices.
  -> Bool   -- ^ Is right-side-up?
  -> AlphaColour Double
  -> CairoPointStyle
filledPolygon radius sides isrot cl = CairoPointStyle rf
  where rf p =
            do (Point x y ) <- alignp p
               c $ setSourceColor cl
               c $ C.newPath
               let intToAngle n =
                         if isrot
                         then       fromIntegral n * 2*pi/fromIntegral sides
                         else (0.5 + fromIntegral n)*2*pi/fromIntegral sides
                   angles = map intToAngle [0 .. sides-1]
                   (p:ps) = map (\a -> Point (x + radius * sin a)
                                             (y + radius * cos a)) angles
               moveTo p
               mapM_ lineTo (ps++[p])
	       c $ C.fill

plusses ::
     Double -- ^ Radius of circle.
  -> Double -- ^ Thickness of line.
  -> AlphaColour Double
  -> CairoPointStyle
plusses radius w cl = CairoPointStyle rf
  where rf p = do (Point x y ) <- alignp p
                  c $ C.setLineWidth w
	          c $ setSourceColor cl
                  c $ C.newPath
                  c $ C.moveTo (x+radius) y
                  c $ C.lineTo (x-radius) y
                  c $ C.moveTo x (y-radius)
                  c $ C.lineTo x (y+radius)
	          c $ C.stroke

exes ::
     Double -- ^ Radius of circle.
  -> Double -- ^ Thickness of line.
  -> AlphaColour Double
  -> CairoPointStyle
exes radius w cl = CairoPointStyle rf
  where rad = radius / sqrt 2
        rf p = do (Point x y ) <- alignp p
                  c $ C.setLineWidth w
	          c $ setSourceColor cl
                  c $ C.newPath
                  c $ C.moveTo (x+rad) (y+rad)
                  c $ C.lineTo (x-rad) (y-rad)
                  c $ C.moveTo (x+rad) (y-rad)
                  c $ C.lineTo (x-rad) (y+rad)
	          c $ C.stroke

stars ::
     Double -- ^ Radius of circle.
  -> Double -- ^ Thickness of line.
  -> AlphaColour Double
  -> CairoPointStyle
stars radius w cl = CairoPointStyle rf
  where rad = radius / sqrt 2
        rf p = do (Point x y ) <- alignp p
                  c $ C.setLineWidth w
	          c $ setSourceColor cl
                  c $ C.newPath
                  c $ C.moveTo (x+radius) y
                  c $ C.lineTo (x-radius) y
                  c $ C.moveTo x (y-radius)
                  c $ C.lineTo x (y+radius)
                  c $ C.moveTo (x+rad) (y+rad)
                  c $ C.lineTo (x-rad) (y-rad)
                  c $ C.moveTo (x+rad) (y-rad)
                  c $ C.lineTo (x-rad) (y+rad)
	          c $ C.stroke

solidLine ::
     Double -- ^ Width of line.
  -> AlphaColour Double
  -> CairoLineStyle
solidLine w cl = CairoLineStyle w cl [] C.LineCapButt C.LineJoinMiter

dashedLine ::
     Double   -- ^ Width of line.
  -> [Double] -- ^ The dash pattern in device coordinates.
  -> AlphaColour Double
  -> CairoLineStyle
dashedLine w ds cl = CairoLineStyle w cl ds C.LineCapButt C.LineJoinMiter

solidFillStyle ::
     AlphaColour Double
  -> CairoFillStyle
solidFillStyle cl = CairoFillStyle fn
   where fn = c $ setSourceColor cl

defaultPointStyle :: CairoPointStyle
defaultPointStyle = filledCircles 1 $ opaque white

defaultFontStyle :: CairoFontStyle
defaultFontStyle = CairoFontStyle {
   font_name_   = "sans",
   font_size_   = 10,
   font_slant_  = C.FontSlantNormal,
   font_weight_ = C.FontWeightNormal,
   font_color_  = opaque black
}

cTranslate x y = c $ C.translate x y
cRotate a = c $ C.rotate a
cNewPath = c $ C.newPath
cLineTo x y = c $ C.lineTo x y
cMoveTo x y = c $ C.moveTo x y
cRelLineTo x y = c $ C.relLineTo x y
cArc x y r a1 a2 = c $ C.arc x y r a1 a2
cArcNegative x y r a1 a2 = c $ C.arcNegative x y r a1 a2
cClosePath = c $ C.closePath
cStroke = c $ C.stroke
cFill = c $ C.fill
cFillPreserve = c $ C.fillPreserve
cSetSourceColor color = c $ setSourceColor color
cPaint = c $ C.paint
cFontExtents = c $ C.fontExtents
cFontExtentsDescent fe = C.fontExtentsDescent fe
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

bitmapEnv :: CEnv
bitmapEnv = CEnv (adjfn 0.5) (adjfn 0.0)
  where
    adjfn offset (Point x y) = Point (adj x) (adj y)
      where
        adj v = (fromIntegral.round) v +offset

vectorEnv :: CEnv
vectorEnv = CEnv id id

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.
$( deriveAccessors ''CairoLineStyle )
$( deriveAccessors ''CairoFontStyle )

