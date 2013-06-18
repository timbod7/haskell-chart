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
  ( bStrokePath
  , bFillPath
  , defaultColorSeq
  
  , alignFillPath
  , alignStrokePath
  
  , alignp
  , alignc
  
  , withRotation
  , withTranslation
  , withPointStyle
  
  , bDrawText
  , bDrawTextR
  , bDrawTextsR
  , drawTextR
  , drawTextsR
  , textDrawRect
  , textDimension
  
  , bDrawPoint
  , drawPoint
    
  , solidLine
  , dashedLine

  , defaultPointStyle
  , filledCircles
  , hollowCircles
  , filledPolygon
  , hollowPolygon
  , plusses
  , exes
  , stars
    
  , solidFillStyle

  , defaultFontStyle
  
  , module Graphics.Rendering.Chart.Types
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

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Geometry

-- -----------------------------------------------------------------------
-- Transformation helpers
-- -----------------------------------------------------------------------

-- | Apply a local rotation. The angle is given in radians.
withRotation :: (ChartBackend m) => Double -> m a -> m a
withRotation angle = withTransform (rotate angle 1)

-- | Apply a local translation.
withTranslation :: (ChartBackend m) => Point -> m a -> m a
withTranslation p = withTransform (translate (pointToVec p) 1)


-- | Changes the 'LineStyle' and 'FillStyle' to comply with
--   the given 'PointStyle'.
withPointStyle :: (ChartBackend m) => PointStyle -> m a -> m a
withPointStyle (PointStyle cl bcl bw _ _) m = do
  ls <- getLineStyle
  withLineStyle (ls { line_color_ = bcl, line_width_ = bw }) $ do
    fs <- getFillStyle
    withFillStyle (solidFillStyle cl) m

-- -----------------------------------------------------------------------
-- Alignment Helpers
-- -----------------------------------------------------------------------

alignPath :: (Point -> Point) -> Path -> Path
alignPath f = foldPath (\p -> moveTo $ f p)
                          (\p -> lineTo $ f p)
                          (\p -> arc $ f p)
                          (\p -> arcNeg $ f p)
                          (close)

alignStrokePath :: (ChartBackend m) => Path -> m Path
alignStrokePath p = do
  f <- liftM cbePointAlignFn ask
  return $ alignPath f p

alignFillPath :: (ChartBackend m) => Path -> m Path
alignFillPath p = do
  f <- liftM cbeCoordAlignFn ask
  return $ alignPath f p

alignp :: (ChartBackend m) => Point -> m Point
alignp p = do 
    alignfn <- liftM cbePointAlignFn ask
    return (alignfn p)

alignc :: (ChartBackend m) => Point -> m Point
alignc p = do 
    alignfn <- liftM cbeCoordAlignFn ask
    return (alignfn p)

-- -----------------------------------------------------------------------
-- Abstract Drawing Methods
-- -----------------------------------------------------------------------

stepPath :: [Point] -> Path
stepPath (p:ps) = moveTo p
               <> mconcat (map lineTo ps)
stepPath [] = mempty

-- | Draw lines between the specified points.
--
-- The points will be "corrected" by the cenv_point_alignfn, so that
-- when drawing bitmaps, 1 pixel wide lines will be centred on the
-- pixels.
bStrokePath :: (ChartBackend m) => [Point] -> m ()
bStrokePath pts = do
    path <- alignStrokePath $ stepPath pts
    strokePath path

-- | Fill the region with the given corners.
--
-- The points will be "corrected" by the cenv_coord_alignfn, so that
-- when drawing bitmaps, the edges of the region will fall between
-- pixels.
bFillPath :: (ChartBackend m) => [Point] -> m ()
bFillPath pts = do
    path <- alignFillPath $ stepPath pts
    fillPath path

{- TODO: Obsolete?
moveTo :: (ChartBackend m) => Point -> m ()
moveTo p  = do
    p' <- alignp p
    bMoveTo p'
    
lineTo :: (ChartBackend m) => Point -> m ()
lineTo p = do
    p' <- alignp p
    bLineTo p'
      -}

-- | Draw a single point at the given location.
bDrawPoint :: (ChartBackend m) 
           => PointStyle -- ^ Style to use when rendering the point.
           -> Point      -- ^ Position of the point to render.
           -> m ()
bDrawPoint = drawPoint

drawPoint :: (ChartBackend m) => PointStyle -> Point -> m ()
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

bDrawText :: (ChartBackend m) => HTextAnchor -> VTextAnchor -> Point -> String -> m ()
bDrawText hta vta p s = drawTextR hta vta 0 p s

bDrawTextR :: (ChartBackend m) => HTextAnchor -> VTextAnchor -> Double -> Point -> String -> m ()
bDrawTextR = drawTextR

-- | Draw a multiline text anchored by one of its corners
--   or edges, with rotation.
bDrawTextsR :: (ChartBackend m)
            => HTextAnchor -- ^ Horizontal text anchor.
            -> VTextAnchor -- ^ Vertical text anchor.
            -> Double      -- ^ Rotation angle in degrees.
            -> Point       -- ^ Anchor point to rotate around.
            -> String      -- ^ Text to render.
            -> m ()
bDrawTextsR = drawTextsR

-- | Function to draw a textual label anchored by one of its corners
--   or edges, with rotation. Rotation angle is given in degrees,
--   rotation is performed around anchor point.
drawTextR :: (ChartBackend m) => HTextAnchor -> VTextAnchor -> Double -> Point -> String -> m ()
drawTextR hta vta angle p s =
  withTranslation p $
    withRotation theta $ do
      ts <- textSize s
      drawText (adjustText hta vta ts) s
  where
    theta = angle*pi/180.0

-- | Function to draw a multi-line textual label anchored by one of its corners
--   or edges, with rotation. Rotation angle is given in degrees,
--   rotation is performed around anchor point.
drawTextsR :: (ChartBackend m) => HTextAnchor -> VTextAnchor -> Double -> Point -> String -> m ()
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

adjustText :: HTextAnchor -> VTextAnchor -> TextSize -> Point
adjustText hta vta ts = Point (adjustTextX hta ts) (adjustTextY vta ts)

adjustTextX :: HTextAnchor -> TextSize -> Double
adjustTextX HTA_Left   _  = 0
adjustTextX HTA_Centre ts = (- (textSizeWidth ts / 2))
adjustTextX HTA_Right  ts = (- textSizeWidth ts)

adjustTextY :: VTextAnchor -> TextSize -> Double
adjustTextY VTA_Top      ts = textSizeAscent ts
adjustTextY VTA_Centre   ts = - (textSizeYBearing ts) / 2
adjustTextY VTA_BaseLine _  = 0
adjustTextY VTA_Bottom   ts = -(textSizeDescent ts)

-- | Recturn the bounding rectangle for a text string positioned
--   where it would be drawn by drawText
textDrawRect :: (ChartBackend m) => HTextAnchor -> VTextAnchor -> Point -> String -> m Rect
textDrawRect hta vta (Point x y) s = do
  ts <- textSize s
  let (w,h) = (textSizeWidth ts, textSizeHeight ts)
  let lx = adjustTextX hta ts
  let ly = adjustTextY vta ts
  let (x',y') = (x + lx, y + ly)
  let p1 = Point x' y'
  let p2 = Point (x' + w) (y' + h)
  return $ Rect p1 p2

textDimension :: (ChartBackend m) => String -> m RectSize
textDimension s = do
  ts <- textSize s
  return (textSizeWidth ts, textSizeHeight ts)

-- -----------------------------------------------------------------------

defaultColorSeq :: [AlphaColour Double]
defaultColorSeq = cycle $ map opaque [blue, red, green, yellow, cyan, magenta]

----------------------------------------------------------------------

solidLine ::
     Double -- ^ Width of line.
  -> AlphaColour Double
  -> LineStyle
solidLine w cl = LineStyle w cl [] LineCapButt LineJoinMiter

dashedLine ::
     Double   -- ^ Width of line.
  -> [Double] -- ^ The dash pattern in device coordinates.
  -> AlphaColour Double
  -> LineStyle
dashedLine w ds cl = LineStyle w cl ds LineCapButt LineJoinMiter

defaultPointStyle :: PointStyle
defaultPointStyle = def

filledCircles :: Double             -- ^ Radius of circle.
              -> AlphaColour Double -- ^ Colour.
              -> PointStyle
filledCircles radius cl = 
  PointStyle cl transparent 0 radius PointShapeCircle

hollowCircles :: Double -- ^ Radius of circle.
              -> Double -- ^ Thickness of line.
              -> AlphaColour Double
              -> PointStyle
hollowCircles radius w cl = 
  PointStyle transparent cl w radius PointShapeCircle

hollowPolygon :: Double -- ^ Radius of circle.
              -> Double -- ^ Thickness of line.
              -> Int    -- ^ Number of vertices.
              -> Bool   -- ^ Is right-side-up?
              -> AlphaColour Double
              -> PointStyle
hollowPolygon radius w sides isrot cl = 
  PointStyle transparent cl w radius (PointShapePolygon sides isrot)

filledPolygon :: Double -- ^ Radius of circle.
              -> Int    -- ^ Number of vertices.
              -> Bool   -- ^ Is right-side-up?
              -> AlphaColour Double
              -> PointStyle
filledPolygon radius sides isrot cl = 
  PointStyle cl transparent 0 radius (PointShapePolygon sides isrot)

plusses :: Double -- ^ Radius of circle.
        -> Double -- ^ Thickness of line.
        -> AlphaColour Double
        -> PointStyle
plusses radius w cl = 
  PointStyle transparent cl w radius PointShapePlus

exes :: Double -- ^ Radius of circle.
     -> Double -- ^ Thickness of line.
     -> AlphaColour Double
     -> PointStyle
exes radius w cl =
  PointStyle transparent cl w radius PointShapeCross

stars :: Double -- ^ Radius of circle.
      -> Double -- ^ Thickness of line.
      -> AlphaColour Double
      -> PointStyle
stars radius w cl =
  PointStyle transparent cl w radius PointShapeStar

solidFillStyle :: AlphaColour Double -> FillStyle
solidFillStyle cl = FillStyleSolid cl

defaultFontStyle :: FontStyle
defaultFontStyle = def

