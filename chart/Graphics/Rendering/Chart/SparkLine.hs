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

module Graphics.Rendering.Chart.SparkLine
  ( -- * SparkLine type
    SparkLine(..)
    -- * Drawing options
  , SparkOptions(..)
  , smoothSpark
  , barSpark
    -- * Rendering function
  , renderSparkLine
  , sparkLineToPNG
  , sparkLineToPDF
  ) where

import Control.Monad
import Data.List
import Data.Ord
import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Renderable
import Data.Colour
import Data.Colour.Names

-- | A sparkline is a single sequence of data values, treated as y-values.
--   The x-values are anonymous and implicit in the sequence.
data SparkLine = SparkLine { sparkOptions :: SparkOptions
                           , sparkData    :: [Double]
                           }

-- | Options to render the sparklines in different ways.
data SparkOptions = SparkOptions
  { smooth     :: Bool            -- ^ smooth or bars
  , step       :: Int             -- ^ step size
  , height     :: Int             -- ^ graph height (pixels)
  , limits     :: (Double,Double) -- ^ data point limits
  , bgColor    :: Colour Double   -- ^ background color
  , minColor   :: Colour Double   -- ^ color of minimum datapoint
  , maxColor   :: Colour Double   -- ^ color of maximum datapoint
  , lastColor  :: Colour Double   -- ^ color of last datapoint
  , minMarker  :: Bool            -- ^ display minimum marker
  , maxMarker  :: Bool            -- ^ display maximum marker
  , lastMarker :: Bool            -- ^ display last marker
  } deriving (Show)

-- | Default options for a smooth sparkline.
smoothSpark :: SparkOptions
smoothSpark  = SparkOptions
  { smooth     = True
  , step       = 2
  , height     = 20
  , limits     = (0,100)
  , bgColor    = white
  , minColor   = red
  , maxColor   = green
  , lastColor  = blue
  , minMarker  = True
  , maxMarker  = True
  , lastMarker = True
  }

-- | Default options for a barchart sparkline.
barSpark :: SparkOptions
barSpark  = smoothSpark { smooth=False }

instance ToRenderable SparkLine where
    toRenderable sp = Renderable
            { minsize = return (0, fromIntegral (height (sparkOptions sp)))
            , render  = \_rect-> renderSparkLine sp
            }

-- | Compute the width of a SparkLine, for rendering purposes.
sparkWidth :: SparkLine -> Int
sparkWidth SparkLine{sparkOptions=opt, sparkData=ds} =
  let w = 4 + (step opt) * (length ds - 1) + extrawidth
      extrawidth | smooth opt = 0
                 | otherwise  = bw * length ds
      bw | smooth opt = 0
         | otherwise  = 2
  in w

-- | Render a SparkLine to a drawing surface using cairo.
renderSparkLine :: SparkLine -> CRender (PickFn ())
renderSparkLine SparkLine{sparkOptions=opt, sparkData=ds} =
  let w = 4 + (step opt) * (length ds - 1) + extrawidth
      extrawidth | smooth opt = 0
                 | otherwise  = bw * length ds
      bw | smooth opt = 0
         | otherwise  = 2
      h = height opt 
      dmin = fst (limits opt)
      dmax = snd (limits opt)
      coords = zipWith (\x y-> Point (fi x) y)
                       [1,(1+bw+step opt)..(1+(step opt+bw)*(length ds))]
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
  in preserveCState $ do

  setFillStyle (solidFillStyle (opaque (bgColor opt)))
  fillPath (rectPath (Rect (Point 0 0) (Point (fi w) (fi h))))
  if smooth opt
    then do
      setLineStyle (solidLine 1 (opaque grey))
      strokePath coords
    else do
      setFillStyle (solidFillStyle (opaque grey))
      forM_ coords $ \ (Point x y) ->
          fillPath (rectPath (Rect (Point (x-1) y) (Point (x+1) (fi h))))
  when (minMarker opt) $ do
      setFillStyle (solidFillStyle (opaque (minColor opt)))
      fillPath (rectPath (boxpt minpt))
  when (maxMarker opt) $ do
      setFillStyle (solidFillStyle (opaque (maxColor opt)))
      fillPath (rectPath (boxpt maxpt))
  when (lastMarker opt) $ do
      setFillStyle (solidFillStyle (opaque (lastColor opt)))
      fillPath (rectPath (boxpt endpt))
  return nullPickFn

-- | Generate a PNG for the sparkline, using its natural size.
sparkLineToPNG :: FilePath -> SparkLine -> IO (PickFn ())
sparkLineToPNG fp sp = renderableToPNGFile (toRenderable sp)
                                           (sparkWidth sp)
                                           (height (sparkOptions sp))
                                           fp
-- | Generate a PDF for the sparkline, using its natural size.
sparkLineToPDF :: FilePath -> SparkLine -> IO ()
sparkLineToPDF fp sp = renderableToPDFFile (toRenderable sp)
                                           (sparkWidth sp)
                                           (height (sparkOptions sp))
                                           fp
