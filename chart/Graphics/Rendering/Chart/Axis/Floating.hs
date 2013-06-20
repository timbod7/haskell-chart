-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.Floating
-- Copyright   :  (c) Tim Docker 2010
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Calculate and render floating value axes
-- including doubles with linear, log, and percentage scaling.
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}

module Graphics.Rendering.Chart.Axis.Floating(
    Percent(..),
    LinearAxisParams(..),
    LogValue(..),
    LogAxisParams(..),
    defaultLinearAxis,
    defaultLogAxis,
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
import Data.Default
import Numeric (showFFloat)

import Data.Accessor.Template
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Utils
import Graphics.Rendering.Chart.Axis.Types

instance PlotValue Double where
    toValue  = id
    fromValue= id
    autoAxis = autoScaledAxis defaultLinearAxis

-- | A wrapper class for doubles used to indicate they are to
-- be plotted against a percentage axis.
newtype Percent = Percent {unPercent :: Double}
    deriving (Eq,Ord,Num,Real,Fractional,RealFrac,Floating,RealFloat)

instance Show Percent where
    show (Percent d) = showD (d*100) ++ "%"

instance PlotValue Percent where
    toValue  = unPercent
    fromValue= Percent
    autoAxis = autoScaledAxis defaultLinearAxis{-la_labelf_=-}

-- | A wrapper class for doubles used to indicate they are to
-- be plotted against a log axis.
newtype LogValue = LogValue Double
    deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)

instance Show LogValue where
    show (LogValue x) = show x

instance PlotValue LogValue where
    toValue (LogValue x) = log x
    fromValue d          = LogValue (exp d)
    autoAxis             = autoScaledLogAxis defaultLogAxis

showD :: (RealFloat d) => d -> String
showD x = case reverse $ showFFloat Nothing x "" of
            '0':'.':r -> reverse r
            r         -> reverse r

data LinearAxisParams a = LinearAxisParams {
    -- | The function used to show the axes labels.
    la_labelf_  :: a -> String,

    -- | The target number of labels to be shown.
    la_nLabels_ :: Int,

    -- | The target number of ticks to be shown.
    la_nTicks_  :: Int
}

{-# DEPRECATED defaultLinearAxis "Use the according Data.Default instance!" #-}
defaultLinearAxis :: (Show a, RealFloat a) => LinearAxisParams a
defaultLinearAxis = def

instance (Show a, RealFloat a) => Default (LinearAxisParams a) where
  def = LinearAxisParams 
    { la_labelf_    = showD
    , la_nLabels_   = 5
    , la_nTicks_    = 50
    }

-- | Generate a linear axis with the specified bounds
scaledAxis :: RealFloat a => LinearAxisParams a -> (a,a) -> AxisFn a
scaledAxis lap (min,max) ps0 = makeAxis' realToFrac realToFrac
                                         (la_labelf_ lap) (labelvs,tickvs,gridvs)
  where
    ps        = filter isValidNumber ps0
    range []  = (0,1)
    range _   | min == max = if min==0 then (-1,1) else
                             let d = abs (min * 0.01) in (min-d,max+d)
              | otherwise  = (min,max)
    labelvs   = map fromRational $ steps (fromIntegral (la_nLabels_ lap)) r
    tickvs    = map fromRational $ steps (fromIntegral (la_nTicks_ lap))
                                         (minimum labelvs,maximum labelvs)
    gridvs    = labelvs
    r         = range ps

-- | Generate a linear axis automatically, scaled appropriately for the
-- input data.
autoScaledAxis :: RealFloat a => LinearAxisParams a -> AxisFn a
autoScaledAxis lap ps0 = scaledAxis lap (min,max) ps0
  where
    (min,max) = (minimum ps0,maximum ps0)

steps :: RealFloat a => a -> (a,a) -> [Rational]
steps nSteps (min,max) = map ((s*) . fromIntegral) [min' .. max']
  where
    s    = chooseStep nSteps (min,max)
    min' = floor   $ realToFrac min / s
    max' = ceiling $ realToFrac max / s
    n    = (max' - min')


chooseStep :: RealFloat a => a -> (a,a) -> Rational
chooseStep nsteps (x1,x2) = minimumBy (comparing proximity) steps
  where
    delta = x2 - x1
    mult  = 10 ^^ (floor $ log10 $ delta / nsteps)
    steps = map (mult*) [0.1,0.2,0.25,0.5,1.0,2.0,2.5,5.0,10,20,25,50]
    proximity x = abs $ delta / realToFrac x - nsteps

-- | Given a target number of values, and a list of input points,
--   find evenly spaced values from the set {1*X, 2*X, 2.5*X, 5*X} (where
--   X is some power of ten) that evenly cover the input points.
autoSteps :: Int -> [Double] -> [Double]
autoSteps nSteps vs = map fromRational $ steps (fromIntegral nSteps) r
  where
    range []  = (0,1)
    range _   | min == max = (min-0.5,min+0.5)
              | otherwise  = (min,max)
    (min,max) = (minimum ps,maximum ps)
    ps        = filter isValidNumber vs
    r         = range ps

----------------------------------------------------------------------

{-# DEPRECATED defaultLogAxis "Use the according Data.Default instance!" #-}
defaultLogAxis :: (Show a, RealFloat a) => LogAxisParams a
defaultLogAxis = def

instance (Show a, RealFloat a) => Default (LogAxisParams a) where
  def = LogAxisParams 
    { loga_labelf_ = showD
    }

-- | Generate a log axis automatically, scaled appropriate for the
-- input data.
autoScaledLogAxis :: RealFloat a => LogAxisParams a -> AxisFn a
autoScaledLogAxis lap ps0 =
    makeAxis' (realToFrac . log) (realToFrac . exp)
              (loga_labelf_ lap) (wrap rlabelvs, wrap rtickvs, wrap rgridvs)
        where
          ps        = filter (\x -> isValidNumber x && 0 < x) ps0
          (min,max) = (minimum ps,maximum ps)
          wrap      = map fromRational
          range []  = (3,30)
          range _   | min == max = (realToFrac $ min/3, realToFrac $ max*3)
                    | otherwise  = (realToFrac $ min,   realToFrac $ max)
          (rlabelvs, rtickvs, rgridvs) = logTicks (range ps)


data LogAxisParams a = LogAxisParams {
    -- | The function used to show the axes labels.
    loga_labelf_ :: a -> String
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
  ratio      = high/low
  lower a l  = let (i,r) = frac (log10 a) in
               (maximum (1:filter (\x -> log10 (fromRational x) <= r) l))*10^^i
  upper a l  = let (i,r) = properFraction (log10 a) in
               (minimum (10:filter (\x -> r <= log10 (fromRational x)) l))*10^^i
  powers           :: (Double,Double) -> [Rational] -> [Rational]
  powers (x,y) l    = [ a*10^^p | p <- [(floor (log10 x))..(ceiling (log10 y))]
                                , a <- l ]
  midselection r l  = filter (inRange r l) (powers r l)
  inRange (a,b) l x = (lower a l <= x) && (x <= upper b l)
  major | 17.5 < log10 ratio = map (\x -> 10^^(round x)) $
                               steps (min 5 (log10 ratio))
                                                       (log10 low, log10 high)
        | 12 < log10 ratio   = map (\x -> 10^^(round x)) $
                               steps ((log10 ratio)/5) (log10 low, log10 high)
        | 6 < log10 ratio    = map (\x -> 10^^(round x)) $
                               steps ((log10 ratio)/2) (log10 low, log10 high)
        | 3 < log10 ratio    = midselection (low,high) [1,10]
        | 20 < ratio         = midselection (low,high) [1,5,10]
        | 6 < ratio          = midselection (low,high) [1,2,4,6,8,10]
        | 3 < ratio          = midselection (low,high) [1..10]
        | otherwise          = steps 5 (low,high)
  (l',h')   = (minimum major, maximum major)
  (dl',dh') = (fromRational l', fromRational h')
  ratio'    = fromRational (h'/l')
  minor | 50 < log10 ratio' = map (\x -> 10^^(round x)) $
                              steps 50 (log10 $ dl', log10 $ dh')
        | 6 < log10 ratio'  = filter (\x -> l'<=x && x <=h') $
                              powers (dl', dh') [1,10]
        | 3 < log10 ratio'  = filter (\x -> l'<=x && x <=h') $
                              powers (dl',dh') [1,5,10]
        | 6 < ratio'        = filter (\x -> l'<=x && x <=h') $
                              powers (dl',dh') [1..10]
        | 3 < ratio'        = filter (\x -> l'<=x && x <=h') $
                              powers (dl',dh') [1,1.2..10]
        | otherwise         = steps 50 (dl', dh')

log10 :: (Floating a) => a -> a
log10 = logBase 10

frac x | 0 <= b    = (a,b)
       | otherwise = (a-1,b+1)
 where
  (a,b) = properFraction x

$( deriveAccessors ''LinearAxisParams )
$( deriveAccessors ''LogAxisParams )

