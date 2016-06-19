-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.Floating
-- Copyright   :  (c) Tim Docker 2010, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Calculate and render floating value axes
-- including doubles with linear, log, and percentage scaling.
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Rendering.Chart.Axis.Floating(
    Percent(..),
    LinearAxisParams(..),
    LogValue(..),
    LogAxisParams(..),
    scaledAxis,
    autoScaledAxis,
    autoScaledLogAxis,
    autoSteps,

    la_labelf,
    la_nLabels,
    la_nTicks,

    loga_labelf
) where

import Data.List(minimumBy)
import Data.Ord (comparing)
import Data.Default.Class
import Numeric (showEFloat, showFFloat)

import Control.Lens
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Utils
import Graphics.Rendering.Chart.Axis.Types

-- Note: the following code uses explicit Integer types
-- to avoid -Wall 'defaulting to Integer' messages.

instance PlotValue Double where
    toValue  = id
    fromValue= id
    autoAxis = autoScaledAxis def

instance PlotValue Float where
    toValue  = realToFrac
    fromValue= realToFrac
    autoAxis = autoScaledAxis def

-- | A wrapper class for doubles used to indicate they are to
-- be plotted against a percentage axis.
newtype Percent = Percent {unPercent :: Double}
    deriving (Eq,Ord,Num,Real,Fractional,RealFrac,Floating,RealFloat)

instance Show Percent where
    show (Percent d) = showD (d*100) ++ "%"

instance PlotValue Percent where
    toValue  = unPercent
    fromValue= Percent
    autoAxis = autoScaledAxis def {-_la_labelf=-}

-- | A wrapper class for doubles used to indicate they are to
-- be plotted against a log axis.
newtype LogValue = LogValue Double
    deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)

instance Show LogValue where
    show (LogValue x) = show x

instance PlotValue LogValue where
    toValue (LogValue x) = log x
    fromValue d          = LogValue (exp d)
    autoAxis             = autoScaledLogAxis def

-- | Show a list of axis labels.
-- If some are too big or all are too small, switch to scientific notation for all.
-- If the range is much smaller than the mean, use an offset.
-- TODO: show this offset only once, not on every label.
-- When thinking about improving this function,
-- https://github.com/matplotlib/matplotlib/blob/master/lib/matplotlib/ticker.py
-- is a good read.
--
-- >>> showDs [0, 1, 2 :: Double]
-- ["0","1","2"]
--
-- >>> showDs [0, 1000000, 2000000 :: Double]
-- ["0.0e0","1.0e6","2.0e6"]
--
-- >>> showDs [0, 0.001, 0.002 :: Double]
-- ["0","0.001","0.002"]
--
-- >>> showDs [-10000000, -1000000, 9000000 :: Double]
-- ["-1.0e7","-1.0e6","9.0e6"]
--
-- >>> showDs [10, 11, 12 :: Double]
-- ["10","11","12"]
--
-- >>> showDs [100, 101, 102 :: Double]
-- ["100","101","102"]
--
-- >>> showDs [100000, 100001, 100002 :: Double]
-- ["100000","100001","100002"]
--
-- >>> showDs [1000000, 1000001, 1000002 :: Double]
-- ["1.0e6 + 0","1.0e6 + 1","1.0e6 + 2"]
--
-- >>> showDs [10000000, 10000001, 10000002 :: Double]
-- ["1.0e7 + 0","1.0e7 + 1","1.0e7 + 2"]
--
-- >>> showDs [-10000000, -10000001, -10000002 :: Double]
-- ["-1.0e7 + 2","-1.0e7 + 1","-1.0e7 + 0"]
--
-- prop> let [s0, s1] = showDs [x, x + 1.0 :: Double] in s0 /= s1
showDs :: forall d . (RealFloat d) => [d] -> [String]
showDs xs
  | useOffset = map addShownOffset $ showWithoutOffset (map (\x -> x - offset) xs)
  | otherwise = showWithoutOffset xs
  where
    -- if the range is much smaller than the mean
    -- AND the data is either all positive or all negative, apply an offset
    useOffset
      -- if some data is positive and some negative, we don't need an offset
      | min' <= 0 && max' >= 0 = False
      -- if the range is significantly smaller than the average, we need an offset
      | 1e6 * abs (max' - min') < abs mean' = True
      | otherwise = False

    mean' :: d
    mean'
      | n == 0 = 0
      | otherwise = sum xs / fromIntegral n
      where
        n = length xs
    min' = minimum xs
    max' = maximum xs

    -- Use the min for offset. Another good choice could be the mean.
    offset :: d
    offset = min'
    shownOffset = case showWithoutOffset [offset] of
      [r] -> r
      rs -> error $ "showDs: shownOffset expected 1 element, got " ++ show (length rs)

    addShownOffset :: String -> String
    addShownOffset ('-':x) = shownOffset ++ " - " ++ x
    addShownOffset x = shownOffset ++ " + " ++ x

showWithoutOffset :: RealFloat d => [d] -> [String]
showWithoutOffset xs
  | useScientificNotation = map (\x -> showEFloat' (Just 1) x) xs
  | otherwise = map showD xs
  where
    -- use scientific notation if max value is too big or too small
    useScientificNotation = maxAbs >= 1e6 || maxAbs <= 1e-6
    maxAbs = maximum (map abs xs)


-- | Changes the behavior of showEFloat to drop more than one trailings 0.
-- Instead of 1.000e4 you get 1.0e4
showEFloat' :: forall d . RealFloat d => Maybe Int -> d -> String
showEFloat' mdigits x = reverse $ cleanup0 (reverse shown0)
  where
    shown0 = showEFloat mdigits x ""

    -- wait until we get the "e"
    cleanup0 :: String -> String
    cleanup0 (e@'e':ys) = e:cleanup1 ys
    cleanup0 (y:ys) = y : cleanup0 ys
    cleanup0 [] = reverse shown0 -- something went wrong, just return the original

    -- get rid of redundant 0s before the '.'
    cleanup1 :: String -> String
    cleanup1 ('0':ys@('0':_)) = cleanup1 ys
    cleanup1 y = y


showD :: (RealFloat d) => d -> String
showD x = case reverse $ showFFloat Nothing x "" of
            '0':'.':r -> reverse r
            r         -> reverse r

data LinearAxisParams a = LinearAxisParams {
    -- | The function used to show the axes labels.
    _la_labelf  :: [a] -> [String],

    -- | The target number of labels to be shown.
    _la_nLabels :: Int,

    -- | The target number of ticks to be shown.
    _la_nTicks  :: Int
}

instance (Show a, RealFloat a) => Default (LinearAxisParams a) where
  def = LinearAxisParams 
    { _la_labelf    = showDs
    , _la_nLabels   = 5
    , _la_nTicks    = 50
    }

-- | Generate a linear axis with the specified bounds
scaledAxis :: RealFloat a => LinearAxisParams a -> (a,a) -> AxisFn a
scaledAxis lap rs@(minV,maxV) ps0 = makeAxis' realToFrac realToFrac
                                         (_la_labelf lap) (labelvs,tickvs,gridvs)
  where
    ps        = filter isValidNumber ps0
    range []  = (0,1)
    range _   | minV == maxV = if minV==0 then (-1,1) else
                               let d = abs (minV * 0.01) in (minV-d,maxV+d)
              | otherwise    = rs
    labelvs   = map fromRational $ steps (fromIntegral (_la_nLabels lap)) r
    tickvs    = map fromRational $ steps (fromIntegral (_la_nTicks lap))
                                         (minimum labelvs,maximum labelvs)
    gridvs    = labelvs
    r         = range ps

-- | Generate a linear axis automatically, scaled appropriately for the
-- input data.
autoScaledAxis :: RealFloat a => LinearAxisParams a -> AxisFn a
autoScaledAxis lap ps0 = scaledAxis lap rs ps
  where
    ps = filter isValidNumber ps0
    rs = (minimum ps,maximum ps)

steps :: RealFloat a => a -> (a,a) -> [Rational]
steps nSteps rs@(minV,maxV) = map ((s*) . fromIntegral) [min' .. max']
  where
    s    = chooseStep nSteps rs
    min' :: Integer
    min' = floor   $ realToFrac minV / s
    max' = ceiling $ realToFrac maxV / s

chooseStep :: RealFloat a => a -> (a,a) -> Rational
chooseStep nsteps (x1,x2) = minimumBy (comparing proximity) stepVals
  where
    delta = x2 - x1
    mult  = 10 ^^ ((floor $ log10 $ delta / nsteps)::Integer)
    stepVals = map (mult*) [0.1,0.2,0.25,0.5,1.0,2.0,2.5,5.0,10,20,25,50]
    proximity x = abs $ delta / realToFrac x - nsteps

-- | Given a target number of values, and a list of input points,
--   find evenly spaced values from the set {1*X, 2*X, 2.5*X, 5*X} (where
--   X is some power of ten) that evenly cover the input points.
autoSteps :: Int -> [Double] -> [Double]
autoSteps nSteps vs = map fromRational $ steps (fromIntegral nSteps) r
  where
    range []  = (0,1)
    range _   | minV == maxV = (minV-0.5,minV+0.5)
              | otherwise    = rs
    rs@(minV,maxV) = (minimum ps,maximum ps)
    ps        = filter isValidNumber vs
    r         = range ps

----------------------------------------------------------------------

instance (Show a, RealFloat a) => Default (LogAxisParams a) where
  def = LogAxisParams 
    { _loga_labelf = showDs
    }

-- | Generate a log axis automatically, scaled appropriate for the
-- input data.
autoScaledLogAxis :: RealFloat a => LogAxisParams a -> AxisFn a
autoScaledLogAxis lap ps0 =
    makeAxis' (realToFrac . log) (realToFrac . exp)
              (_loga_labelf lap) (wrap rlabelvs, wrap rtickvs, wrap rgridvs)
        where
          ps        = filter (\x -> isValidNumber x && 0 < x) ps0
          (minV,maxV) = (minimum ps,maximum ps)
          wrap      = map fromRational
          range []  = (3,30)
          range _   | minV == maxV = (realToFrac $ minV/3, realToFrac $ maxV*3)
                    | otherwise    = (realToFrac $ minV,   realToFrac $ maxV)
          (rlabelvs, rtickvs, rgridvs) = logTicks (range ps)


data LogAxisParams a = LogAxisParams {
    -- | The function used to show the axes labels.
    _loga_labelf :: [a] -> [String]
}

{-
 Rules: Do not subdivide between powers of 10 until all powers of 10
          get a major ticks.
        Do not subdivide between powers of ten as [1,2,4,6,8,10] when
          5 gets a major ticks
          (ie the major ticks need to be a subset of the minor tick)
-}
logTicks :: Range -> ([Rational],[Rational],[Rational])
logTicks (low,high) = (major,minor,major)
 where
  pf :: RealFrac a => a -> (Integer, a)
  pf = properFraction

  -- frac :: (RealFrac a, Integral b) => a -> (b, a)
  frac :: (RealFrac a) => a -> (Integer, a)
  frac x | 0 <= b    = (a,b)
         | otherwise = (a-1,b+1)
    where
      (a,b) = properFraction x

  ratio      = high/low
  lower a l  = let (i,r) = frac (log10 a) in
               maximum (1:filter (\x -> log10 (fromRational x) <= r) l)*10^^i
  upper a l  = let (i,r) = pf (log10 a) in
               minimum (10:filter (\x -> r <= log10 (fromRational x)) l)*10^^i
               
  powers           :: (Double,Double) -> [Rational] -> [Rational]
  powers (x,y) l    = [ a*10^^p | p <- [(floor (log10 x))..(ceiling (log10 y))] :: [Integer]
                                , a <- l ]
  midselection r l  = filter (inRange r l) (powers r l)
  inRange (a,b) l x = (lower a l <= x) && (x <= upper b l)
  
  logRange = (log10 low, log10 high)
  
  roundPow x = 10^^(round x :: Integer)
  
  major | 17.5 < log10 ratio = map roundPow $
                               steps (min 5 (log10 ratio)) logRange
        | 12 < log10 ratio   = map roundPow $
                               steps (log10 ratio / 5) logRange
        | 6 < log10 ratio    = map roundPow $
                               steps (log10 ratio / 2) logRange
        | 3 < log10 ratio    = midselection (low,high) [1,10]
        | 20 < ratio         = midselection (low,high) [1,5,10]
        | 6 < ratio          = midselection (low,high) [1,2,4,6,8,10]
        | 3 < ratio          = midselection (low,high) [1..10]
        | otherwise          = steps 5 (low,high)

  (l',h')   = (minimum major, maximum major)
  (dl',dh') = (fromRational l', fromRational h')
  ratio' :: Double
  ratio' = fromRational (h'/l')
  filterX = filter (\x -> l'<=x && x <=h') . powers (dl',dh') 
  
  minor | 50 < log10 ratio' = map roundPow $
                              steps 50 (log10 dl', log10 dh')
        | 6 < log10 ratio'  = filterX [1,10]
        | 3 < log10 ratio'  = filterX [1,5,10]
        | 6 < ratio'        = filterX [1..10]
        | 3 < ratio'        = filterX [1,1.2..10]
        | otherwise         = steps 50 (dl', dh')


log10 :: (Floating a) => a -> a
log10 = logBase 10

$( makeLenses ''LinearAxisParams )
$( makeLenses ''LogAxisParams )
