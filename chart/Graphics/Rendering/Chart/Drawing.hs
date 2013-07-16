{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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
--    * 'LineStyle'
--
--    * 'FontStyle'
--
-- These accessors are not shown in this API documentation.  They have
-- the same name as the field, but with the trailing underscore
-- dropped. Hence for data field f_::F in type D, they have type
--
-- @
--   f :: Data.Accessor.Accessor D F
-- @
--

module Graphics.Rendering.Chart.Drawing
  ( -- * Point Types and Drawing
    PointShape(..)
  , PointStyle(..)
  , drawPoint
  , defaultPointStyle
  
  -- * Alignments and Paths
  , alignPath
  , alignFillPath
  , alignStrokePath
  
  , alignp
  , alignc
  
  , strokePointPath
  , fillPointPath
  
  -- * Transformation Helpers
  , withRotation
  , withTranslation
  , withScale
  , withScaleX, withScaleY
  , withPointStyle
  
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
    
  , solidFillStyle
  
  -- * Backend and general Types
  , module Graphics.Rendering.Chart.Backend
) where

import Data.Default
import Data.Accessor
import Data.Accessor.Template
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Data.List (unfoldr)
import Data.Monoid

import Control.Monad.Reader

import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Geometry

-- -----------------------------------------------------------------------
-- Transformation helpers
-- -----------------------------------------------------------------------

-- | Apply a local rotation. The angle is given in radians.
withRotation :: Double -> ChartBackend a -> ChartBackend a
withRotation angle = withTransform (rotate angle 1)

-- | Apply a local translation.
withTranslation :: Point -> ChartBackend a -> ChartBackend a
withTranslation p = withTransform (translate (pointToVec p) 1)

-- | Apply a local scale.
withScale :: Vector -> ChartBackend a -> ChartBackend a
withScale v = withTransform (scale v 1)

-- | Apply a local scale on the x-axis.
withScaleX :: Double -> ChartBackend a -> ChartBackend a
withScaleX x = withScale (Vector x 1)

-- | Apply a local scale on the y-axis.
withScaleY :: Double -> ChartBackend a -> ChartBackend a
withScaleY y = withScale (Vector 1 y)

-- | Changes the 'LineStyle' and 'FillStyle' to comply with
--   the given 'PointStyle'.
withPointStyle :: PointStyle -> ChartBackend a -> ChartBackend a
withPointStyle (PointStyle cl bcl bw _ _) m = do
  ls <- getLineStyle
  withLineStyle (ls { line_color_ = bcl, line_width_ = bw }) $ do
    fs <- getFillStyle
    withFillStyle (solidFillStyle cl) m

-- -----------------------------------------------------------------------
-- Alignment Helpers
-- -----------------------------------------------------------------------

-- | Align the path by applying the given function on all points.
alignPath :: (Point -> Point) -> Path -> Path
alignPath f = foldPath (\p -> moveTo $ f p)
                       (\p -> lineTo $ f p)
                       (\p -> arc $ f p)
                       (\p -> arcNeg $ f p)
                       (close)

-- | Align the path using the environments alignment function for points.
--   This is generally useful when stroking. 
--   See 'alignPath' and 'cbePointAlignFn'.
alignStrokePath :: Path -> ChartBackend Path
alignStrokePath p = do
  f <- getPointAlignFn
  return $ alignPath f p

-- | Align the path using the environments alignment function for coordinates.
--   This is generally useful when filling. 
--   See 'alignPath' and 'cbeCoordAlignFn'.
alignFillPath :: Path -> ChartBackend Path
alignFillPath p = do
  f <- getCoordAlignFn
  return $ alignPath f p

-- | Align the point using the environments alignment function for points.
--   See 'cbePointAlignFn'.
alignp :: Point -> ChartBackend Point
alignp p = do 
    alignfn <- getPointAlignFn
    return (alignfn p)

-- | Align the point using the environments alignment function for coordinates.
--   See 'cbeCoordAlignFn'.
alignc :: Point -> ChartBackend Point
alignc p = do 
    alignfn <- getCoordAlignFn
    return (alignfn p)

-- | Create a path by connecting all points with a line.
--   The path is not closed.
stepPath :: [Point] -> Path
stepPath (p:ps) = moveTo p
               <> mconcat (map lineTo ps)
stepPath [] = mempty

-- | Draw lines between the specified points.
--
--   The points will be aligned by the 'cbePointAlignFn', so that
--   when drawing bitmaps, 1 pixel wide lines will be centred on the
--   pixels.
strokePointPath :: [Point] -> ChartBackend ()
strokePointPath pts = do
    path <- alignStrokePath $ stepPath pts
    strokePath path

-- | Fill the region with the given corners.
--
--   The points will be aligned by the 'cbeCoordAlignFn', so that
--   when drawing bitmaps, the edges of the region will fall between
--   pixels.
fillPointPath :: [Point] -> ChartBackend ()
fillPointPath pts = do
    path <- alignFillPath $ stepPath pts
    fillPath path

-- -----------------------------------------------------------------------
-- Text Drawing
-- -----------------------------------------------------------------------

-- | Draw a line of text that is aligned at a different anchor point.
--   See 'drawText'.
drawTextA :: HTextAnchor -> VTextAnchor -> Point -> String -> ChartBackend ()
drawTextA hta vta p s = drawTextR hta vta 0 p s

-- | Draw a textual label anchored by one of its corners
--   or edges, with rotation. Rotation angle is given in degrees,
--   rotation is performed around anchor point.
--   See 'drawText'.
drawTextR :: HTextAnchor -> VTextAnchor -> Double -> Point -> String -> ChartBackend ()
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
drawTextsR :: HTextAnchor -> VTextAnchor -> Double -> Point -> String -> ChartBackend ()
drawTextsR hta vta angle p s = case num of
      0 -> return ()
      1 -> drawTextR hta vta angle p s
      _ -> do
        withTranslation p $
          withRotation theta $ do
            tss <- mapM textSize ss
            let ts = head tss
            let widths = map textSizeWidth tss
                maxw   = maximum widths
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

      drawT x y s = drawText (Point x y) s
      theta = angle*pi/180.0

      yinit VTA_Top      ts height = textSizeAscent ts
      yinit VTA_BaseLine ts height = 0
      yinit VTA_Centre   ts height = height / 2 + textSizeAscent ts
      yinit VTA_Bottom   ts height = height + textSizeAscent ts

-- | Calculate the correct offset to align the text anchor.
adjustText :: HTextAnchor -> VTextAnchor -> TextSize -> Point
adjustText hta vta ts = Point (adjustTextX hta ts) (adjustTextY vta ts)

-- | Calculate the correct offset to align the horizontal anchor.
adjustTextX :: HTextAnchor -> TextSize -> Double
adjustTextX HTA_Left   _  = 0
adjustTextX HTA_Centre ts = (- (textSizeWidth ts / 2))
adjustTextX HTA_Right  ts = (- textSizeWidth ts)

-- | Calculate the correct offset to align the vertical anchor.
adjustTextY :: VTextAnchor -> TextSize -> Double
adjustTextY VTA_Top      ts = textSizeAscent ts
adjustTextY VTA_Centre   ts = - (textSizeYBearing ts) / 2
adjustTextY VTA_BaseLine _  = 0
adjustTextY VTA_Bottom   ts = -(textSizeDescent ts)

-- | Return the bounding rectangle for a text string positioned
--   where it would be drawn by 'drawText'.
--   See 'textSize'.
textDrawRect :: HTextAnchor -> VTextAnchor -> Point -> String -> ChartBackend Rect
textDrawRect hta vta (Point x y) s = do
  ts <- textSize s
  let (w,h) = (textSizeWidth ts, textSizeHeight ts)
  let lx = adjustTextX hta ts
  let ly = adjustTextY vta ts
  let (x',y') = (x + lx, y + ly)
  let p1 = Point x' y'
  let p2 = Point (x' + w) (y' + h)
  return $ Rect p1 p2

-- | Get the width and height of the string when rendered.
--   See 'textSize'.
textDimension :: String -> ChartBackend RectSize
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

-- | Abstract data type for the style of a plotted point.
--
--   The contained Cairo action draws a point in the desired
--   style, at the supplied device coordinates.
data PointStyle = PointStyle 
  { point_color_ :: AlphaColour Double
  -- ^ The color to fill the point with.
  , point_border_color_ :: AlphaColour Double
  -- ^ The color to stroke the outline with.
  , point_border_width_ :: Double
  -- ^ The width of the outline.
  , point_radius_ :: Double
  -- ^ The radius of the tightest surrounding circle of the point.
  , point_shape_ :: PointShape
  -- ^ The shape.
  }

-- | Default style to use for points.
instance Default PointStyle where
  def = PointStyle 
    { point_color_        = opaque black
    , point_border_color_ = transparent
    , point_border_width_ = 0
    , point_radius_       = 1
    , point_shape_        = PointShapeCircle
    }

{-# DEPRECATED defaultPointStyle "Use the according Data.Default instance!" #-}
-- | Default style for points.
defaultPointStyle :: PointStyle
defaultPointStyle = def

-- | Draw a single point at the given location.
drawPoint :: PointStyle  -- ^ Style to use when rendering the point.
          -> Point       -- ^ Position of the point to render.
          -> ChartBackend ()
drawPoint ps@(PointStyle cl bcl bw r shape) p = withPointStyle ps $ do
  p'@(Point x y) <- alignp p
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
          (p:ps) = map (\a -> Point (x + r * sin a)
                                    (y + r * cos a)) angles
      let path = moveTo p <> mconcat (map lineTo ps) <> lineTo p
      fillPath path
      strokePath path
    PointShapePlus -> do
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

-- | Fill style that fill everything this the given colour.
solidFillStyle :: AlphaColour Double -> FillStyle
solidFillStyle cl = FillStyleSolid cl

