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

-- | scale a point by a constant
pscale :: Double -> Point -> Point
pscale c (Point x y) = (Point (x*c) (y*c))

-- | add two points
padd :: Point -> Point -> Point
padd (Point x1 y1) (Point x2 y2) = (Point (x1+x2) (y1+y2))

-- | subtract two points
psub :: Point -> Point -> Point
psub (Point x1 y1) (Point x2 y2) = (Point (x1-x2) (y1-y2))

-- | A rectangle is defined by two points
data Rect = Rect Point Point
   deriving Show

data RectEdge = E_Top | E_Bottom | E_Left | E_Right

-- | Create a rectangle based upon the coordinates of 4 points
mkrect (Point x1 _) (Point _ y2) (Point x3 _) (Point _ y4) =
    Rect (Point x1 y2) (Point x3 y4)

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
  -> Double -- ^ red component of colour
  -> Double -- ^ green component of colour
  -> Double -- ^ blue component of colour
  -> CairoPointStyle
filledCircles radius r g b = CairoPointStyle rf
  where
    rf (Point x y) = do
	C.setSourceRGB r g b
        C.newPath
	C.arc x y radius 0 360
	C.fill

solidLine ::
     Double -- ^ width of line
  -> Double -- ^ red component of colour
  -> Double -- ^ green component of colour
  -> Double -- ^ blue component of colour
  -> CairoLineStyle
solidLine w r g b = CairoLineStyle (do
    C.setLineWidth w
    C.setSourceRGB r g b
    )

dashedLine ::
     Double   -- ^ width of line
  -> [Double] -- ^ the dash pattern in device coordinates
  -> Double   -- ^ red component of colour
  -> Double   -- ^ green component of colour
  -> Double   -- ^ blue component of colour
  -> CairoLineStyle
dashedLine w dashes r g b = CairoLineStyle (do
    C.setDash dashes 0
    C.setLineWidth w
    C.setSourceRGB r g b
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
     Double         -- ^ red component of colour
  -> Double         -- ^ green component of colour
  -> Double         -- ^ blue component of colour
  -> CairoFillStyle
solidFillStyle r g b = CairoFillStyle fn
   where fn = C.setSourceRGB r g b

defaultPointStyle = filledCircles 1 1 1 1
defaultFontStyle = CairoFontStyle (return ())

