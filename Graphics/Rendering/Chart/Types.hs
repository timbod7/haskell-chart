{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Types
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

module Graphics.Rendering.Chart.Types(
    Rect(..),
    Point(..),
    Vector(..),

    RectSize,
    Range,

    mkrect,
    pvadd,
    pvsub,
    psub,
    vscale,
    within,

    RectEdge(..),
    Limit(..),
    PointMapFn,

    preserveCState,
    setClipRegion,
    moveTo,
    lineTo,
    rectPath,
    strokePath,
    fillPath,

    isValidNumber,
    maybeM,

    defaultColorSeq,

    setSourceColor,

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

    CRender(..),
    CEnv(..),
    runCRender,
    c,
    alignp,
    alignc,
    
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

) where

import qualified Graphics.Rendering.Cairo as C
import Control.Monad.Reader
import Data.Accessor
import Data.Accessor.Template
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Data.List (unfoldr)

-- | A point in two dimensions.
data Point = Point {
    p_x :: Double,
    p_y :: Double
} deriving Show

data Vector = Vector {
    v_x :: Double,
    v_y :: Double
} deriving Show

-- | Scale a vector by a constant.
vscale :: Double -> Vector -> Vector
vscale c (Vector x y) = (Vector (x*c) (y*c))

-- | Add a point and a vector.
pvadd :: Point -> Vector -> Point
pvadd (Point x1 y1) (Vector x2 y2) = (Point (x1+x2) (y1+y2))

-- | Subtract a vector from a point.
pvsub :: Point -> Vector -> Point
pvsub (Point x1 y1) (Vector x2 y2) = (Point (x1-x2) (y1-y2))

-- | Subtract two points.
psub :: Point -> Point -> Vector
psub (Point x1 y1) (Point x2 y2) = (Vector (x1-x2) (y1-y2))

data Limit a = LMin | LValue a | LMax
   deriving Show

-- | A function mapping between points.
type PointMapFn x y = (Limit x, Limit y) -> Point

-- | A rectangle is defined by two points.
data Rect = Rect Point Point
   deriving Show

data RectEdge = E_Top | E_Bottom | E_Left | E_Right

-- | Create a rectangle based upon the coordinates of 4 points.
mkrect :: Point -> Point -> Point -> Point -> Rect
mkrect (Point x1 _) (Point _ y2) (Point x3 _) (Point _ y4) =
    Rect (Point x1 y2) (Point x3 y4)

-- | Test if a point is within a rectangle.
within :: Point -> Rect -> Bool
within (Point x y) (Rect (Point x1 y1) (Point x2 y2)) =
    x >= x1 && x <= x2 && y >= y1 && y <= y2


----------------------------------------------------------------------

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

type Range    = (Double,Double)
type RectSize = (Double,Double)

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

-- | Make a path from a rectangle.
rectPath :: Rect -> [Point]
rectPath (Rect p1@(Point x1 y1) p3@(Point x2 y2)) = [p1,p2,p3,p4,p1]
  where    
    p2 = (Point x1 y2)
    p4 = (Point x2 y1)

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
    case line_dashes_ ls of
      [] -> return ()
      ds -> c $ C.setDash ds 0

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

isValidNumber :: (RealFloat a) => a -> Bool
isValidNumber v = not (isNaN v) && not (isInfinite v)

maybeM :: (Monad m) => b -> (a -> m b) -> Maybe a -> m b
maybeM v = maybe (return v)

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.
$( deriveAccessors ''CairoLineStyle )
$( deriveAccessors ''CairoFontStyle )

