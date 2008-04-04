-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Types
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Types where

import qualified Graphics.Rendering.Cairo as C

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
 
-- | Abstract data type for the style of a plotted point
--
-- The contained Cairo action draws a point in the desired
-- style, at the supplied device coordinates.
newtype CairoPointStyle = CairoPointStyle (Point -> C.Render ())

-- | Abstract data type for the style of a line
--
-- The contained Cairo action sets the required line
-- in the Cairo rendering state.
newtype CairoLineStyle = CairoLineStyle (C.Render ())

-- | Abstract data type for a fill style
--
-- The contained Cairo action sets the required fill
-- style in the Cairo rendering state.
newtype CairoFillStyle = CairoFillStyle (C.Render ())

-- | Abstract data type for a font.
--
-- The contained Cairo action sets the required font
-- in the Cairo rendering state.
newtype CairoFontStyle = CairoFontStyle (C.Render ())

type Range = (Double,Double)
type RectSize = (Double,Double)

black = Color 0 0 0
grey8 = Color 0.8 0.8 0.8
white = Color 1 1 1
red = Color 1 0 0
green = Color 0 1 0
blue = Color 0 0 1

----------------------------------------------------------------------
-- Assorted helper functions in Cairo Usage

moveTo, lineTo :: Point -> C.Render ()
moveTo (Point px py) = C.moveTo px py
lineTo (Point px py) = C.lineTo px py

setClipRegion p2 p3 = do    
    C.moveTo (p_x p2) (p_y p2)
    C.lineTo (p_x p2) (p_y p3)
    C.lineTo (p_x p3) (p_y p3)
    C.lineTo (p_x p3) (p_y p2)
    C.lineTo (p_x p2) (p_y p2)
    C.clip

-- | stroke the lines between successive points
strokeLines (p1:ps) = do
    C.newPath
    moveTo p1
    mapM_ lineTo ps
    C.stroke
strokeLines _ = return ()

-- | make a path from a rectable
rectPath :: Rect -> C.Render ()
rectPath (Rect (Point x1 y1) (Point x2 y2)) = do
   C.newPath
   C.moveTo x1 y1
   C.lineTo x2 y1
   C.lineTo x2 y2
   C.lineTo x1 y2
   C.lineTo x1 y1

setFontStyle (CairoFontStyle s) = s
setLineStyle (CairoLineStyle s) = s
setFillStyle (CairoFillStyle s) = s

setSourceColor (Color r g b) = C.setSourceRGB r g b

textSize :: String -> C.Render RectSize
textSize s = do
    te <- C.textExtents s
    fe <- C.fontExtents
    return (C.textExtentsWidth te, C.fontExtentsHeight fe)

data HTextAnchor = HTA_Left | HTA_Centre | HTA_Right
data VTextAnchor = VTA_Top | VTA_Centre | VTA_Bottom | VTA_BaseLine

-- | Function to draw a textual label anchored by one of it's corners
-- or edges.
drawText :: HTextAnchor -> VTextAnchor -> Point -> String -> C.Render ()
drawText hta vta (Point x y) s = do
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

----------------------------------------------------------------------

filledCircles ::
     Double -- ^ radius of circle
  -> Color -- ^ colour
  -> CairoPointStyle
filledCircles radius c = CairoPointStyle rf
  where
    rf (Point x y) = do
	setSourceColor c
        C.newPath
	C.arc x y radius 0 360
	C.fill

hollowCircles ::
     Double -- ^ radius of circle
  -> Double -- ^ thickness of line
  -> Color
  -> CairoPointStyle
hollowCircles radius w c = CairoPointStyle rf
  where
    rf (Point x y) = do
        C.setLineWidth w
	setSourceColor c
        C.newPath
	C.arc x y radius 0 360
	C.stroke

hollowPolygon ::
     Double -- ^ radius of circle
  -> Double -- ^ thickness of line
  -> Int    -- ^ Number of vertices
  -> Bool   -- ^ Is right-side-up?
  -> Color
  -> CairoPointStyle
hollowPolygon radius w sides isrot c = CairoPointStyle rf
  where rf (Point x y) =
            do C.setLineWidth w
	       setSourceColor c
               C.newPath
               let intToAngle n = if isrot
                                  then fromIntegral n * 2*pi / fromIntegral sides
                                  else (0.5 + fromIntegral n)*2*pi/fromIntegral sides
                   angles = map intToAngle [0 .. sides-1]
                   (p:ps) = map (\a -> Point (x + radius * sin a) (y + radius * cos a)) angles
               moveTo p
               mapM_ lineTo (ps++[p])
	       C.stroke

filledPolygon ::
     Double -- ^ radius of circle
  -> Int    -- ^ Number of vertices
  -> Bool   -- ^ Is right-side-up?
  -> Color
  -> CairoPointStyle
filledPolygon radius sides isrot c = CairoPointStyle rf
  where rf (Point x y) =
            do setSourceColor c
               C.newPath
               let intToAngle n = if isrot
                                  then fromIntegral n * 2*pi / fromIntegral sides
                                  else (0.5 + fromIntegral n)*2*pi/fromIntegral sides
                   angles = map intToAngle [0 .. sides-1]
                   (p:ps) = map (\a -> Point (x + radius * sin a) (y + radius * cos a)) angles
               moveTo p
               mapM_ lineTo (ps++[p])
	       C.fill

plusses ::
     Double -- ^ radius of circle
  -> Double -- ^ thickness of line
  -> Color
  -> CairoPointStyle
plusses radius w c = CairoPointStyle rf
  where rf (Point x y) = do C.setLineWidth w
	                    setSourceColor c
                            C.newPath
                            C.moveTo (x+radius) y
                            C.lineTo (x-radius) y
                            C.moveTo x (y-radius)
                            C.lineTo x (y+radius)
	                    C.stroke

exes ::
     Double -- ^ radius of circle
  -> Double -- ^ thickness of line
  -> Color
  -> CairoPointStyle
exes radius w c = CairoPointStyle rf
  where rad = radius / sqrt 2
        rf (Point x y) = do C.setLineWidth w
	                    setSourceColor c
                            C.newPath
                            C.moveTo (x+rad) (y+rad)
                            C.lineTo (x-rad) (y-rad)
                            C.moveTo (x+rad) (y-rad)
                            C.lineTo (x-rad) (y+rad)
	                    C.stroke

stars ::
     Double -- ^ radius of circle
  -> Double -- ^ thickness of line
  -> Color 
  -> CairoPointStyle
stars radius w c = CairoPointStyle rf
  where rad = radius / sqrt 2
        rf (Point x y) = do C.setLineWidth w
	                    setSourceColor c
                            C.newPath
                            C.moveTo (x+radius) y
                            C.lineTo (x-radius) y
                            C.moveTo x (y-radius)
                            C.lineTo x (y+radius)
                            C.moveTo (x+rad) (y+rad)
                            C.lineTo (x-rad) (y-rad)
                            C.moveTo (x+rad) (y-rad)
                            C.lineTo (x-rad) (y+rad)
	                    C.stroke

solidLine ::
     Double -- ^ width of line
  -> Color
  -> CairoLineStyle
solidLine w c = CairoLineStyle (do
    C.setLineWidth w
    setSourceColor c
    )

dashedLine ::
     Double   -- ^ width of line
  -> [Double] -- ^ the dash pattern in device coordinates
  -> Color
  -> CairoLineStyle
dashedLine w dashes c = CairoLineStyle (do
    C.setDash dashes 0
    C.setLineWidth w
    setSourceColor c
    )

fontStyle ::
     String         -- ^ the font name
  -> Double         -- ^ the font size
  -> C.FontSlant    -- ^ the font slant
  -> C.FontWeight   -- ^ the font weight
  -> CairoFontStyle
fontStyle name size slant weight = CairoFontStyle fn
  where
    fn = do
	 C.selectFontFace name slant weight
	 C.setFontSize size

solidFillStyle ::
     Color
  -> CairoFillStyle
solidFillStyle c = CairoFillStyle fn
   where fn = setSourceColor c

defaultPointStyle = filledCircles 1 white
defaultFontStyle = CairoFontStyle (return ())

isValidNumber v = not (isNaN v) && not (isInfinite v)
