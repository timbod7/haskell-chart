-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Drawing
-- Copyright   :  (c) Tim Docker 2006, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- This module contains basic types and functions used for drawing.
--
-- Note that Template Haskell is used to derive accessor functions
-- (see 'Control.Lens') for each field of the following data types:
--
--    * 'PointStyle'
--
-- These accessors are not shown in this API documentation.  They have
-- the same name as the field, but with the trailing underscore
-- dropped. Hence for data field @f_::F@ in type @D@, they have type
--
-- @
--   f :: Control.Lens.Lens' D F
-- @
--

{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Drawing
  ( -- * Point Types and Drawing
    PointShape(..)
  , PointStyle(..)
  , drawPoint

  -- * Alignments and Paths
  , alignPath
  , alignFillPath
  , alignStrokePath
  , alignFillPoints
  , alignStrokePoints

  , alignFillPoint
  , alignStrokePoint

  , strokePointPath
  , fillPointPath

  -- * Transformation and Style Helpers
  , withRotation
  , withTranslation
  , withScale
  , withScaleX, withScaleY
  , withPointStyle
  , withDefaultStyle

  -- * Text Drawing
  , drawTextA
  , drawTextR
  , drawTextsR
  , textDrawRect
  , textDimension

  -- * Style Helpers
  , defaultColorSeq

  , solidLine
  , dashedLine

  , filledCircles
  , hollowCircles
  , filledPolygon
  , hollowPolygon
  , plusses
  , exes
  , stars
  , arrows

  , solidFillStyle

  -- * Backend and general Types
  , module Graphics.Rendering.Chart.Backend

  -- * Accessors
  , point_color
  , point_border_color
  , point_border_width
  , point_radius
  , point_shape
) where

import Data.Default.Class
-- lens < 4 includes Control.Lens.Zipper.moveTo which clashes
-- with Graphics.Rendering.Chart.Geometry.moveTo (so you get
-- -Wall notices). This would suggest a 'hiding (moveTo)' in
-- the import, but it's been removed in lens-4.0 and I don't
-- feel it's worth the use of conditional compilation. This does
-- lead to the qualified Geometry import below.
import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.List (unfoldr)

import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Geometry hiding (moveTo)
import qualified Graphics.Rendering.Chart.Geometry as G

-- -----------------------------------------------------------------------
-- Transformation helpers
-- -----------------------------------------------------------------------

-- | Apply a local rotation. The angle is given in radians.
withRotation :: Double -> BackendProgram a -> BackendProgram a
withRotation angle = withTransform (rotate angle 1)

-- | Apply a local translation.
withTranslation :: Point -> BackendProgram a -> BackendProgram a
withTranslation p = withTransform (translate (pointToVec p) 1)

-- | Apply a local scale.
withScale :: Vector -> BackendProgram a -> BackendProgram a
withScale v = withTransform (scale v 1)

-- | Apply a local scale on the x-axis.
withScaleX :: Double -> BackendProgram a -> BackendProgram a
withScaleX x = withScale (Vector x 1)

-- | Apply a local scale on the y-axis.
withScaleY :: Double -> BackendProgram a -> BackendProgram a
withScaleY y = withScale (Vector 1 y)

-- | Changes the 'LineStyle' and 'FillStyle' to comply with
--   the given 'PointStyle'.
withPointStyle :: PointStyle -> BackendProgram a -> BackendProgram a
withPointStyle (PointStyle cl bcl bw _ _) m =
  withLineStyle (def { _line_color = bcl, _line_width = bw, _line_join = LineJoinMiter }) $
    withFillStyle (solidFillStyle cl) m

withDefaultStyle :: BackendProgram a -> BackendProgram a
withDefaultStyle = withLineStyle def . withFillStyle def . withFontStyle def

-- -----------------------------------------------------------------------
-- Alignment Helpers
-- -----------------------------------------------------------------------

-- | Align the path by applying the given function on all points.
alignPath :: (Point -> Point) -> Path -> Path
alignPath f = foldPath (G.moveTo . f)
                       (lineTo . f)
                       (arc . f)
                       (arcNeg . f)
                       close

-- | Align the path using the environment's alignment function for points.
--   This is generally useful when stroking.
--   See 'alignPath' and 'getPointAlignFn'.
alignStrokePath :: Path -> BackendProgram Path
alignStrokePath p = do
  f <- getPointAlignFn
  return $ alignPath f p

-- | Align the path using the environment's alignment function for coordinates.
--   This is generally useful when filling.
--   See 'alignPath' and 'getCoordAlignFn'.
alignFillPath :: Path -> BackendProgram Path
alignFillPath p = do
  f <- getCoordAlignFn
  return $ alignPath f p

-- | The points will be aligned by the 'getPointAlignFn', so that
--   when drawing bitmaps, 1 pixel wide lines will be centred on the
--   pixels.
alignStrokePoints :: [Point] -> BackendProgram [Point]
alignStrokePoints p = do
  f <- getPointAlignFn
  return $ fmap f p

-- | The points will be aligned by the 'getCoordAlignFn', so that
--   when drawing bitmaps, the edges of the region will fall between
--   pixels.
alignFillPoints :: [Point] -> BackendProgram [Point]
alignFillPoints p = do
  f <- getCoordAlignFn
  return $ fmap f p

-- | Align the point using the environment's alignment function for points.
--   See 'getPointAlignFn'.
alignStrokePoint :: Point -> BackendProgram Point
alignStrokePoint p = do
    alignfn <- getPointAlignFn
    return (alignfn p)

-- | Align the point using the environment's alignment function for coordinates.
--   See 'getCoordAlignFn'.
alignFillPoint :: Point -> BackendProgram Point
alignFillPoint p = do
    alignfn <- getCoordAlignFn
    return (alignfn p)

-- | Create a path by connecting all points with a line.
--   The path is not closed.
stepPath :: [Point] -> Path
stepPath (p:ps) = G.moveTo p
               <> mconcat (map lineTo ps)
stepPath [] = mempty

-- | Draw lines between the specified points.
strokePointPath :: [Point] -> BackendProgram ()
strokePointPath pts = strokePath $ stepPath pts

-- | Fill the region with the given corners.
fillPointPath :: [Point] -> BackendProgram ()
fillPointPath pts = fillPath $ stepPath pts

-- -----------------------------------------------------------------------
-- Text Drawing
-- -----------------------------------------------------------------------

-- | Draw a line of text that is aligned at a different anchor point.
--   See 'drawText'.
drawTextA :: HTextAnchor -> VTextAnchor -> Point -> String -> BackendProgram ()
drawTextA hta vta = drawTextR hta vta 0

{-
   The following is useful for checking out the bounding-box
   calculation. At present it looks okay for PNG/Cairo but
   is a bit off for SVG/Diagrams; this may well be down to
   differences in how fonts are rendered in the two backends

drawTextA hta vta p txt =
  drawTextR hta vta 0 p txt
  >> withLineStyle (solidLine 1 (opaque red))
     (textDrawRect hta vta p txt
       >>= \rect -> alignStrokePath (rectPath rect) >>= strokePath)
-}

-- | Draw a textual label anchored by one of its corners
--   or edges, with rotation. Rotation angle is given in degrees,
--   rotation is performed around anchor point.
--   See 'drawText'.
drawTextR :: HTextAnchor -> VTextAnchor -> Double -> Point -> String -> BackendProgram ()
drawTextR hta vta angle p s =
  withTranslation p $
    withRotation theta $ do
      ts <- textSize s
      drawText (adjustText hta vta ts) s
  where
    theta = angle*pi/180.0

-- | Draw a multi-line textual label anchored by one of its corners
--   or edges, with rotation. Rotation angle is given in degrees,
--   rotation is performed around anchor point.
--   See 'drawText'.
drawTextsR :: HTextAnchor -> VTextAnchor -> Double -> Point -> String -> BackendProgram ()
drawTextsR hta vta angle p s = case num of
      0 -> return ()
      1 -> drawTextR hta vta angle p s
      _ ->
        withTranslation p $
          withRotation theta $ do
            tss <- mapM textSize ss
            let ts = head tss
            let -- widths = map textSizeWidth tss
                -- maxw   = maximum widths
                maxh   = maximum (map textSizeYBearing tss)
                gap    = maxh / 2 -- half-line spacing
                totalHeight = fromIntegral num*maxh +
                              (fromIntegral num-1)*gap
                ys = take num (unfoldr (\y-> Just (y, y-gap-maxh))
                                       (yinit vta ts totalHeight))
                xs = map (adjustTextX hta) tss
            sequence_ (zipWith3 drawT xs ys ss)
    where
      ss   = lines s
      num  = length ss

      drawT x y = drawText (Point x y)
      theta = angle*pi/180.0

      yinit VTA_Top      ts _      = textSizeAscent ts
      yinit VTA_BaseLine _  _      = 0
      yinit VTA_Centre   ts height = height / 2 + textSizeAscent ts
      yinit VTA_Bottom   ts height = height + textSizeAscent ts

-- | Calculate the correct offset to align the text anchor.
adjustText :: HTextAnchor -> VTextAnchor -> TextSize -> Point
adjustText hta vta ts = Point (adjustTextX hta ts) (adjustTextY vta ts)

-- | Calculate the correct offset to align the horizontal anchor.
adjustTextX :: HTextAnchor -> TextSize -> Double
adjustTextX HTA_Left   _  = 0
adjustTextX HTA_Centre ts = - (textSizeWidth ts / 2)
adjustTextX HTA_Right  ts = - textSizeWidth ts

-- | Calculate the correct offset to align the vertical anchor.
adjustTextY :: VTextAnchor -> TextSize -> Double
adjustTextY VTA_Top      ts = textSizeAscent ts
adjustTextY VTA_Centre   ts = - textSizeYBearing ts / 2
adjustTextY VTA_BaseLine _  = 0
adjustTextY VTA_Bottom   ts = - textSizeDescent ts

-- | Return the bounding rectangle for a text string positioned
--   where it would be drawn by 'drawText'.
--   See 'textSize'.
textDrawRect :: HTextAnchor -> VTextAnchor -> Point -> String -> BackendProgram Rect
textDrawRect hta vta (Point x y) s = do
  ts <- textSize s
  -- This does not account for the pixel width of the label; e.g.
  -- with a label "bread" and a large-enough foint size (e.g. 36)
  -- I have seen the right-hand edge of the bounding box go through
  -- the vertical part of the 'd' character (see chart-tests/tests/Test8.hs
  -- and bump up the label size).
  let (w,h,dh) = (textSizeWidth ts, textSizeHeight ts, textSizeDescent ts)
      lx = adjustTextX hta ts
      ly = adjustTextY vta ts
      (x',y') = (x + lx, y + ly + dh)
      p1 = Point x' (y' - h)
      p2 = Point (x' + w) y'
  return $ Rect p1 p2

-- | Get the width and height of the string when rendered.
--   See 'textSize'.
textDimension :: String -> BackendProgram RectSize
textDimension s = do
  ts <- textSize s
  return (textSizeWidth ts, textSizeHeight ts)

-- -----------------------------------------------------------------------
-- Point Types and Drawing
-- -----------------------------------------------------------------------

-- | The different shapes a point can have.
data PointShape = PointShapeCircle           -- ^ A circle.
                | PointShapePolygon Int Bool -- ^ Number of vertices and is right-side-up?
                | PointShapePlus  -- ^ A plus sign.
                | PointShapeCross -- ^ A cross.
                | PointShapeStar  -- ^ Combination of a cross and a plus.
                | PointShapeArrowHead Double
                | PointShapeEllipse Double Double -- ^ Ratio of minor to major axis and rotation

-- | Abstract data type for the style of a plotted point.
data PointStyle = PointStyle
  { _point_color :: AlphaColour Double
  -- ^ The color to fill the point with.
  , _point_border_color :: AlphaColour Double
  -- ^ The color to stroke the outline with.
  , _point_border_width :: Double
  -- ^ The width of the outline.
  , _point_radius :: Double
  -- ^ The radius of the tightest surrounding circle of the point.
  , _point_shape :: PointShape
  -- ^ The shape.
  }

-- | Default style to use for points.
instance Default PointStyle where
  def = PointStyle
    { _point_color        = opaque black
    , _point_border_color = transparent
    , _point_border_width = 0
    , _point_radius       = 1
    , _point_shape        = PointShapeCircle
    }

-- | Draw a single point at the given location.
drawPoint :: PointStyle  -- ^ Style to use when rendering the point.
          -> Point       -- ^ Position of the point to render.
          -> BackendProgram ()
drawPoint ps@(PointStyle cl _ _ r shape) p = withPointStyle ps $ do
  p'@(Point x y) <- alignStrokePoint p
  case shape of
    PointShapeCircle -> do
      let path = arc p' r 0 (2*pi)
      fillPath path
      strokePath path
    PointShapePolygon sides isrot -> do
      let intToAngle n =
            if isrot
            then       fromIntegral n * 2*pi/fromIntegral sides
            else (0.5 + fromIntegral n)*2*pi/fromIntegral sides
          angles = map intToAngle [0 .. sides-1]
          (p1:p1':p1s) = map (\a -> Point (x + r * sin a)
                                      (y + r * cos a)) angles
      let path = G.moveTo p1 <> mconcat (map lineTo $ p1':p1s) <> lineTo p1 <> lineTo p1'
      fillPath path
      strokePath path
    PointShapeArrowHead theta ->
      withTranslation p $ withRotation (theta - pi/2) $
          drawPoint (filledPolygon r 3 True cl) (Point 0 0)
    PointShapePlus ->
      strokePath $ moveTo' (x+r) y
                <> lineTo' (x-r) y
                <> moveTo' x (y-r)
                <> lineTo' x (y+r)
    PointShapeCross -> do
      let rad = r / sqrt 2
      strokePath $ moveTo' (x+rad) (y+rad)
                <> lineTo' (x-rad) (y-rad)
                <> moveTo' (x+rad) (y-rad)
                <> lineTo' (x-rad) (y+rad)
    PointShapeStar -> do
      let rad = r / sqrt 2
      strokePath $ moveTo' (x+r) y
                <> lineTo' (x-r) y
                <> moveTo' x (y-r)
                <> lineTo' x (y+r)
                <> moveTo' (x+rad) (y+rad)
                <> lineTo' (x-rad) (y-rad)
                <> moveTo' (x+rad) (y-rad)
                <> lineTo' (x-rad) (y+rad)
    PointShapeEllipse b theta ->
      withTranslation p $ withRotation theta $ withScaleX b $ do
        let path = arc (Point 0 0) r 0 (2*pi)
        fillPath path
        strokePath path

-- -----------------------------------------------------------------------
-- Style Helpers
-- -----------------------------------------------------------------------

-- | The default sequence of colours to use when plotings different data sets
--   in a graph.
defaultColorSeq :: [AlphaColour Double]
defaultColorSeq = cycle $ map opaque [blue, red, green, yellow, cyan, magenta]

-- | Create a solid line style (not dashed).
solidLine :: Double             -- ^ Width of line.
          -> AlphaColour Double -- ^ Colour of line.
          -> LineStyle
solidLine w cl = LineStyle w cl [] LineCapButt LineJoinMiter

-- | Create a dashed line style.
dashedLine :: Double   -- ^ Width of line.
           -> [Double] -- ^ The dash pattern in device coordinates.
           -> AlphaColour Double -- ^ Colour of line.
           -> LineStyle
dashedLine w ds cl = LineStyle w cl ds LineCapButt LineJoinMiter

-- | Style for filled circle points.
filledCircles :: Double             -- ^ Radius of circle.
              -> AlphaColour Double -- ^ Fill colour.
              -> PointStyle
filledCircles radius cl =
  PointStyle cl transparent 0 radius PointShapeCircle

-- | Style for stroked circle points.
hollowCircles :: Double -- ^ Radius of circle.
              -> Double -- ^ Thickness of line.
              -> AlphaColour Double -- Colour of line.
              -> PointStyle
hollowCircles radius w cl =
  PointStyle transparent cl w radius PointShapeCircle

-- | Style for stroked polygon points.
hollowPolygon :: Double -- ^ Radius of circle.
              -> Double -- ^ Thickness of line.
              -> Int    -- ^ Number of vertices.
              -> Bool   -- ^ Is right-side-up?
              -> AlphaColour Double -- ^ Colour of line.
              -> PointStyle
hollowPolygon radius w sides isrot cl =
  PointStyle transparent cl w radius (PointShapePolygon sides isrot)

-- | Style for filled polygon points.
filledPolygon :: Double -- ^ Radius of circle.
              -> Int    -- ^ Number of vertices.
              -> Bool   -- ^ Is right-side-up?
              -> AlphaColour Double -- ^ Fill color.
              -> PointStyle
filledPolygon radius sides isrot cl =
  PointStyle cl transparent 0 radius (PointShapePolygon sides isrot)

-- | Plus sign point style.
plusses :: Double -- ^ Radius of tightest surrounding circle.
        -> Double -- ^ Thickness of line.
        -> AlphaColour Double -- ^ Color of line.
        -> PointStyle
plusses radius w cl =
  PointStyle transparent cl w radius PointShapePlus

-- | Cross point style.
exes :: Double -- ^ Radius of circle.
     -> Double -- ^ Thickness of line.
     -> AlphaColour Double -- ^ Color of line.
     -> PointStyle
exes radius w cl =
  PointStyle transparent cl w radius PointShapeCross

-- | Combination of plus and cross point style.
stars :: Double -- ^ Radius of circle.
      -> Double -- ^ Thickness of line.
      -> AlphaColour Double -- ^ Color of line.
      -> PointStyle
stars radius w cl =
  PointStyle transparent cl w radius PointShapeStar

arrows :: Double -- ^ Radius of circle.
       -> Double -- ^ Rotation (Tau)
       -> Double -- ^ Thickness of line.
       -> AlphaColour Double -- ^ Color of line.
       -> PointStyle
arrows radius angle w cl =
  PointStyle transparent cl w radius (PointShapeArrowHead angle)

-- | Fill style that fill everything this the given colour.
solidFillStyle :: AlphaColour Double -> FillStyle
solidFillStyle = FillStyleSolid

$( makeLenses ''PointStyle )
