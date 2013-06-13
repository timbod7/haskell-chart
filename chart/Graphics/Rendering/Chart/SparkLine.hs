---------------------------------------------------------------
-- |
-- Module      : Graphics.Rendering.Chart.Sparkline
-- Copyright   : (c) Hitesh Jasani, 2008, Malcolm Wallace 2011
-- License     : BSD3
--
-- Created     : 2008-02-26
-- Modified    : 2011-02-11
-- Version     : 0.2
--
-- Sparklines implementation in Haskell.  Sparklines are
-- mini graphs inspired by Edward Tufte.
--
-- The original implementation (by Hitesh Jasani) used the gd
-- package as a backend renderer, and is still available at
--     http://hackage.haskell.org/package/hsparklines
-- The present version uses Cairo as its renderer, and integrates with
-- the Chart package, in the sense that Sparklines are just another
-- kind of (ToRenderable a => a), so can be composed into grids etc.
--
-- > dp :: [Double]
-- > dp = [24,21,32.3,24,15,34,43,55,57,72,74,75,73,72,55,44]
-- >
-- > sparkLineToPNG "bar_spark.png" (SparkLine barSpark dp)
-- >
---------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}

module Graphics.Rendering.Chart.SparkLine
  ( -- * SparkLine type
    SparkLine(..)
    -- * Drawing options
  , SparkOptions(..)
  , smoothSpark
  , barSpark
    -- * Size calculation
  , sparkSize
    -- * Rendering function
  , renderSparkLine
  , sparkLineToRenderable
  , sparkLineToPNG
  , sparkLineToPDF
  ) where

import Control.Monad
import Data.List
import Data.Ord
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Renderable
import Data.Colour
import Data.Colour.Names

import Graphics.Rendering.Chart.Backend.Cairo

-- | A sparkline is a single sequence of data values, treated as y-values.
--   The x-values are anonymous and implicit in the sequence.
data SparkLine = SparkLine { sl_options :: SparkOptions
                           , sl_data    :: [Double]
                           }

-- | Options to render the sparklines in different ways.
data SparkOptions = SparkOptions
  { so_smooth     :: Bool            -- ^ smooth or bars
  , so_step       :: Int             -- ^ step size
  , so_height     :: Int             -- ^ graph height (pixels)
  , so_limits     :: (Double,Double) -- ^ data point limits
  , so_bgColor    :: Colour Double   -- ^ background color
  , so_minColor   :: Colour Double   -- ^ color of minimum datapoint
  , so_maxColor   :: Colour Double   -- ^ color of maximum datapoint
  , so_lastColor  :: Colour Double   -- ^ color of last datapoint
  , so_minMarker  :: Bool            -- ^ display minimum marker
  , so_maxMarker  :: Bool            -- ^ display maximum marker
  , so_lastMarker :: Bool            -- ^ display last marker
  } deriving (Show)

-- | Default options for a smooth sparkline.
smoothSpark :: SparkOptions
smoothSpark  = SparkOptions
  { so_smooth     = True
  , so_step       = 2
  , so_height     = 20
  , so_limits     = (0,100)
  , so_bgColor    = white
  , so_minColor   = red
  , so_maxColor   = green
  , so_lastColor  = blue
  , so_minMarker  = True
  , so_maxMarker  = True
  , so_lastMarker = True
  }

-- | Default options for a barchart sparkline.
barSpark :: SparkOptions
barSpark  = smoothSpark { so_smooth=False }
{- TODO: See class definition
instance ToRenderable SparkLine where
    type RenderableT m SparkLine = SparkLine
    toRenderable = sparkLineToRenderable
-}
sparkLineToRenderable :: (ChartBackend m) => SparkLine -> Renderable m ()
sparkLineToRenderable sp = Renderable
            { minsize = return (0, fromIntegral (so_height (sl_options sp)))
            , render  = \_rect-> renderSparkLine sp
            }

-- | Compute the width of a SparkLine, for rendering purposes.
sparkWidth :: SparkLine -> Int
sparkWidth SparkLine{sl_options=opt, sl_data=ds} =
  let w = 4 + (so_step opt) * (length ds - 1) + extrawidth
      extrawidth | so_smooth opt = 0
                 | otherwise  = bw * length ds
      bw | so_smooth opt = 0
         | otherwise  = 2
  in w

sparkSize :: SparkLine -> (Int,Int)
sparkSize s = (sparkWidth s, so_height (sl_options s))

-- | Render a SparkLine to a drawing surface using cairo.
renderSparkLine :: (ChartBackend m) => SparkLine -> m (PickFn ())
renderSparkLine SparkLine{sl_options=opt, sl_data=ds} =
  let w = 4 + (so_step opt) * (length ds - 1) + extrawidth
      extrawidth | so_smooth opt = 0
                 | otherwise  = bw * length ds
      bw | so_smooth opt = 0
         | otherwise  = 2
      h = so_height opt 
      dmin = fst (so_limits opt)
      dmax = snd (so_limits opt)
      coords = zipWith (\x y-> Point (fi x) y)
                       [1,(1+bw+so_step opt)..(1+(so_step opt+bw)*(length ds))]
                       [ fi h - ( (y-dmin) /
                                  ((dmax-dmin+1) / fi (h-4)) )
                         | y <- ds ]
      -- remember y increases as we go down the page
      minpt = maximumBy (comparing p_y) coords
      maxpt = minimumBy (comparing p_y) coords
      endpt = last coords
      boxpt :: Point -> Rect
      boxpt (Point x y) = Rect (Point (x-1)(y-1)) (Point (x+1)(y+1))
      fi    :: (Num b, Integral a) => a -> b
      fi    = fromIntegral
  in bLocal $ do

  bSetFillStyle (solidFillStyle (opaque (so_bgColor opt)))
  bFillPath (rectPath (Rect (Point 0 0) (Point (fi w) (fi h))))
  if so_smooth opt
    then do
      bSetLineStyle (solidLine 1 (opaque grey))
      bStrokePath coords
    else do
      bSetFillStyle (solidFillStyle (opaque grey))
      forM_ coords $ \ (Point x y) ->
          bFillPath (rectPath (Rect (Point (x-1) y) (Point (x+1) (fi h))))
  when (so_minMarker opt) $ do
      bSetFillStyle (solidFillStyle (opaque (so_minColor opt)))
      bFillPath (rectPath (boxpt minpt))
  when (so_maxMarker opt) $ do
      bSetFillStyle (solidFillStyle (opaque (so_maxColor opt)))
      bFillPath (rectPath (boxpt maxpt))
  when (so_lastMarker opt) $ do
      bSetFillStyle (solidFillStyle (opaque (so_lastColor opt)))
      bFillPath (rectPath (boxpt endpt))
  return nullPickFn

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
