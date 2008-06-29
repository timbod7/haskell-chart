{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Types
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Types where

import qualified Graphics.Rendering.Cairo as C
import Control.Monad.Reader

-- | A point in two dimensions
data Point = Point {
    p_x :: Double,
    p_y :: Double
} deriving Show

data Vector = Vector {
    v_x :: Double,
    v_y :: Double
} deriving Show

data Color = Color {
    c_r :: Double,
    c_g :: Double,
    c_b :: Double
}

-- | scale a vector by a constant
vscale :: Double -> Vector -> Vector
vscale c (Vector x y) = (Vector (x*c) (y*c))

-- | add a point and a vector
pvadd :: Point -> Vector -> Point
pvadd (Point x1 y1) (Vector x2 y2) = (Point (x1+x2) (y1+y2))

-- | subtract a vector from a point
pvsub :: Point -> Vector -> Point
pvsub (Point x1 y1) (Vector x2 y2) = (Point (x1-x2) (y1-y2))

-- | subtract two points
psub :: Point -> Point -> Vector
psub (Point x1 y1) (Point x2 y2) = (Vector (x1-x2) (y1-y2))

-- | a function mapping between points
type PointMapFn = Point -> Point

-- | A rectangle is defined by two points
data Rect = Rect Point Point
   deriving Show

data RectEdge = E_Top | E_Bottom | E_Left | E_Right

-- | Create a rectangle based upon the coordinates of 4 points
mkrect (Point x1 _) (Point _ y2) (Point x3 _) (Point _ y4) =
    Rect (Point x1 y2) (Point x3 y4)

-- | A linear mapping of points in one range to another
vmap :: Range -> Range -> Double -> Double
vmap (v1,v2) (v3,v4) v = v3 + (v-v1) * (v4-v3) / (v2-v1)

----------------------------------------------------------------------

-- | The environment present in the CRender Monad.
data CEnv = CEnv {
    -- | A transform applied immediately prior to values
    -- being displayed in device coordinates
    --
    -- When device coordinates correspond to pixels, a cleaner
    -- image is created if this transform rounds to the nearest
    -- pixel. With higher-resolution output, this transform can
    -- just be the identity function.
    cenv_point_alignfn :: Point -> Point
}

newtype CRender a = DR (ReaderT CEnv C.Render a)
  deriving (Functor, Monad, MonadReader CEnv)

runCRender :: CRender a -> CEnv -> C.Render a
runCRender (DR m) e = runReaderT m e

c :: C.Render a -> CRender a
c = DR . lift
 
----------------------------------------------------------------------

-- | Abstract data type for the style of a plotted point
--
-- The contained Cairo action draws a point in the desired
-- style, at the supplied device coordinates.
newtype CairoPointStyle = CairoPointStyle (Point -> CRender ())

-- | Abstract data type for the style of a line
--
-- The contained Cairo action sets the required line
-- in the Cairo rendering state.
newtype CairoLineStyle = CairoLineStyle (CRender ())

-- | Abstract data type for a fill style
--
-- The contained Cairo action sets the required fill
-- style in the Cairo rendering state.
newtype CairoFillStyle = CairoFillStyle (CRender ())

-- | Data type for a font
data CairoFontStyle = CairoFontStyle {
      font_name :: String,
      font_size :: Double,
      font_slant :: C.FontSlant,
      font_weight :: C.FontWeight,
      font_color :: Color
}

type Range = (Double,Double)
type RectSize = (Double,Double)

black = Color 0 0 0
grey8 = Color 0.8 0.8 0.8
white = Color 1 1 1
red = Color 1 0 0
green = Color 0 1 0
blue = Color 0 0 1

defaultColorSeq = cycle [blue,red,green, Color 1 1 0,Color 0 1 1,Color 1 0 1 ]
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

lineTo p = do
    p' <- alignp p
    c $ C.lineTo (p_x p') (p_y p')

setClipRegion p2 p3 = do    
    c $ C.moveTo (p_x p2) (p_y p2)
    c $ C.lineTo (p_x p2) (p_y p3)
    c $ C.lineTo (p_x p3) (p_y p3)
    c $ C.lineTo (p_x p3) (p_y p2)
    c $ C.lineTo (p_x p2) (p_y p2)
    c $ C.clip

-- | stroke the lines between successive points
strokeLines :: [Point] -> CRender ()
strokeLines (p1:ps) = do
    c $ C.newPath
    moveTo p1
    mapM_ lineTo ps
    c $ C.stroke
strokeLines _ = return ()

-- | make a path from a rectable
rectPath :: Rect -> CRender ()
rectPath (Rect (Point x1 y1) (Point x2 y2)) = c $ do
   C.newPath
   C.moveTo x1 y1
   C.lineTo x2 y1
   C.lineTo x2 y2
   C.lineTo x1 y2
   C.lineTo x1 y1

setFontStyle f = do
    c $ C.selectFontFace (font_name f) (font_slant f) (font_weight f)
    c $ C.setFontSize (font_size f)
    c $ setSourceColor (font_color f)

setLineStyle (CairoLineStyle s) = s
setFillStyle (CairoFillStyle s) = s

setSourceColor (Color r g b) = C.setSourceRGB r g b

textSize :: String -> CRender RectSize
textSize s = c $ do
    te <- C.textExtents s
    fe <- C.fontExtents
    return (C.textExtentsWidth te, C.fontExtentsHeight fe)

data HTextAnchor = HTA_Left | HTA_Centre | HTA_Right
data VTextAnchor = VTA_Top | VTA_Centre | VTA_Bottom | VTA_BaseLine

-- | Function to draw a textual label anchored by one of it's corners
-- or edges.
drawText :: HTextAnchor -> VTextAnchor -> Point -> String -> CRender ()
drawText hta vta (Point x y) s = c $ do
    te <- C.textExtents s
    fe <- C.fontExtents
    let lx = xadj hta (C.textExtentsWidth te)
    let ly = yadj vta te fe
    C.moveTo (x+lx) (y+ly)
    C.showText s
  where
    xadj HTA_Left   w = 0
    xadj HTA_Centre w = (-w/2)
    xadj HTA_Right  w = (-w)
    yadj VTA_Top    te fe = C.fontExtentsAscent fe
    yadj VTA_Centre te fe = - (C.textExtentsYbearing te) / 2
    yadj VTA_BaseLine te fe = 0
    yadj VTA_Bottom te fe = -(C.fontExtentsDescent fe)

-- | Execute a rendering action in a saved context (ie bracketed
-- between C.save and C.restore)
preserveCState :: CRender a -> CRender a
preserveCState a = do 
  c $ C.save
  v <- a
  c $ C.restore
  return v

----------------------------------------------------------------------

filledCircles ::
     Double -- ^ radius of circle
  -> Color -- ^ colour
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
     Double -- ^ radius of circle
  -> Double -- ^ thickness of line
  -> Color
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
     Double -- ^ radius of circle
  -> Double -- ^ thickness of line
  -> Int    -- ^ Number of vertices
  -> Bool   -- ^ Is right-side-up?
  -> Color
  -> CairoPointStyle
hollowPolygon radius w sides isrot cl = CairoPointStyle rf
  where rf p =
            do (Point x y ) <- alignp p
               c $ C.setLineWidth w
	       c $ setSourceColor cl
               c $ C.newPath
               let intToAngle n = if isrot
                                  then fromIntegral n * 2*pi / fromIntegral sides
                                  else (0.5 + fromIntegral n)*2*pi/fromIntegral sides
                   angles = map intToAngle [0 .. sides-1]
                   (p:ps) = map (\a -> Point (x + radius * sin a) (y + radius * cos a)) angles
               moveTo p
               mapM_ lineTo (ps++[p])
	       c $ C.stroke

filledPolygon ::
     Double -- ^ radius of circle
  -> Int    -- ^ Number of vertices
  -> Bool   -- ^ Is right-side-up?
  -> Color
  -> CairoPointStyle
filledPolygon radius sides isrot cl = CairoPointStyle rf
  where rf p =
            do (Point x y ) <- alignp p
               c $ setSourceColor cl
               c $ C.newPath
               let intToAngle n = if isrot
                                  then fromIntegral n * 2*pi / fromIntegral sides
                                  else (0.5 + fromIntegral n)*2*pi/fromIntegral sides
                   angles = map intToAngle [0 .. sides-1]
                   (p:ps) = map (\a -> Point (x + radius * sin a) (y + radius * cos a)) angles
               moveTo p
               mapM_ lineTo (ps++[p])
	       c $ C.fill

plusses ::
     Double -- ^ radius of circle
  -> Double -- ^ thickness of line
  -> Color
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
     Double -- ^ radius of circle
  -> Double -- ^ thickness of line
  -> Color
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
     Double -- ^ radius of circle
  -> Double -- ^ thickness of line
  -> Color 
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
     Double -- ^ width of line
  -> Color
  -> CairoLineStyle
solidLine w cl = CairoLineStyle (do
    c $ C.setLineWidth w
    c $ setSourceColor cl
    )

dashedLine ::
     Double   -- ^ width of line
  -> [Double] -- ^ the dash pattern in device coordinates
  -> Color
  -> CairoLineStyle
dashedLine w dashes cl = CairoLineStyle (do
    c $ C.setDash dashes 0
    c $ C.setLineWidth w
    c $ setSourceColor cl
    )

solidFillStyle ::
     Color
  -> CairoFillStyle
solidFillStyle cl = CairoFillStyle fn
   where fn = c $ setSourceColor cl

defaultPointStyle = filledCircles 1 white

defaultFontStyle = CairoFontStyle {
   font_name = "sans",
   font_size = 10,
   font_slant = C.FontSlantNormal,
   font_weight = C.FontWeightNormal,
   font_color = black
}

isValidNumber v = not (isNaN v) && not (isInfinite v)
