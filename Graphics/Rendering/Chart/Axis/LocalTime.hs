-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.LocalTime
-- Copyright   :  (c) Tim Docker 2010
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Calculate and render time axes

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}

module Graphics.Rendering.Chart.Axis.LocalTime(
    timeAxis,
    autoTimeAxis,
    days, months, years
) where

import qualified Graphics.Rendering.Cairo as C
import Data.Time
import Data.Fixed
import System.Locale (defaultTimeLocale)
import Control.Monad
import Data.List
import Data.Accessor.Template
import Data.Colour (opaque)
import Data.Colour.Names (black, lightgrey)
import Data.Ord (comparing)

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Axis.Types

instance PlotValue LocalTime where
    toValue    = doubleFromLocalTime
    fromValue  = localTimeFromDouble
    autoAxis   = autoTimeAxis

----------------------------------------------------------------------

-- | Map a LocalTime value to a plot coordinate.
doubleFromLocalTime :: LocalTime -> Double
doubleFromLocalTime lt = fromIntegral (toModifiedJulianDay (localDay lt))
              + fromRational (timeOfDayToDayFraction (localTimeOfDay lt))

-- | Map a plot coordinate to a LocalTime.
localTimeFromDouble :: Double -> LocalTime
localTimeFromDouble v =
  LocalTime (ModifiedJulianDay i) (dayFractionToTimeOfDay (toRational d))
 where
   (i,d) = properFraction v

-- | TimeSeq is a (potentially infinite) set of times. When passed
--   a reference time, the function returns a a pair of lists. The first
--   contains all times in the set less than the reference time in
--   decreasing order. The second contains all times in the set greater
--   than or equal to the reference time, in increasing order.
type TimeSeq = LocalTime-> ([LocalTime],[LocalTime])

coverTS :: TimeSeq -> LocalTime -> LocalTime -> [LocalTime]
coverTS tseq min max = min' ++ enumerateTS tseq min max ++ max'
  where
    min' =  if elemTS min tseq then [] else take 1 (fst (tseq min))
    max' =  if elemTS max tseq then [] else take 1 (snd (tseq max))

enumerateTS :: TimeSeq -> LocalTime -> LocalTime -> [LocalTime]
enumerateTS tseq min max =
    reverse (takeWhile (>=min) ts1)  ++ takeWhile (<=max) ts2
  where
    (ts1,ts2) = tseq min

elemTS :: LocalTime -> TimeSeq -> Bool
elemTS t tseq = case tseq t of
    (_,(t0:_)) | t == t0 -> True
    _                    -> False

-- | How to display a time
type TimeLabelFn = LocalTime -> String

-- | Create an 'AxisFn' to for a time axis.  The first 'TimeSeq' sets the
--   minor ticks, and the ultimate range will be aligned to its elements.
--   The second 'TimeSeq' sets the labels and grid.  The third 'TimeSeq'
--   sets the second line of labels.  The 'TimeLabelFn' is
--   used to format LocalTimes for labels.  The values to be plotted
--   against this axis can be created with 'doubleFromLocalTime'.
timeAxis :: TimeSeq -> TimeSeq -> TimeLabelFn -> TimeSeq -> TimeLabelFn
            -> AxisFn LocalTime
timeAxis tseq lseq labelf cseq contextf pts = AxisData {
    axis_viewport_ = vmap(min', max'),
    axis_tropweiv_ = invmap(min', max'),
    axis_ticks_    = [ (t,2) | t <- times] ++ [ (t,5) | t <- ltimes, visible t],
    axis_labels_   = [ [ (t,l) | (t,l) <- labels labelf   ltimes, visible t]
                     , [ (t,l) | (t,l) <- labels contextf ctimes, visible t]
                     ], 
    axis_grid_     = [ t     | t <- ltimes, visible t]
    }
  where
    (min,max)    = case pts of
                       [] -> (refLocalTime,refLocalTime)
                       ps -> (minimum ps, maximum ps)
    refLocalTime = LocalTime (ModifiedJulianDay 0) midnight
    times        = coverTS tseq min max
    ltimes       = coverTS lseq min max
    ctimes       = coverTS cseq min max
    min'         = minimum times
    max'         = maximum times
    visible t    = min' <= t && t <= max'
    labels f ts  = [ (avg m3 m4, f m1)
                   | (m1,m2) <- zip ts (tail ts)
                   , let m3 = if m1<min' then min' else m1
                   , let m4 = if m2>max' then max' else m2 ]
    avg m1 m2    = localTimeFromDouble $ m1' + (m2' - m1')/2
     where
      m1' = doubleFromLocalTime m1
      m2' = doubleFromLocalTime m2

normalizeTimeOfDay :: LocalTime -> LocalTime
normalizeTimeOfDay t@(LocalTime day (TimeOfDay h m s))
  | s >= 60   = normalizeTimeOfDay (LocalTime day (TimeOfDay h (m+s`div'`60)
                                                               (s`mod'`60)))
  | m >= 60   = normalizeTimeOfDay (LocalTime day (TimeOfDay (h+m`div`60)
                                                             (m`mod`60) s))
  | h >= 24   = LocalTime (addDays (fromIntegral (h`div`24)) day)
                          (TimeOfDay (h`mod`24) m s)
  | otherwise = t

addTod :: Int -> Int -> Int -> LocalTime -> LocalTime
addTod dh dm ds (LocalTime day (TimeOfDay h m s)) = normalizeTimeOfDay t'
  where t' = LocalTime day (TimeOfDay (h+dh) (m+dm) (s+fromIntegral ds))


-- | A 'TimeSeq' for seconds.
seconds :: TimeSeq
seconds t = (iterate rev t1, tail (iterate fwd t1))
  where h0       = todHour (localTimeOfDay t)
        m0       = todMin  (localTimeOfDay t)
        s0       = todSec  (localTimeOfDay t)
        t0       = LocalTime (localDay t) (TimeOfDay h0 m0 s0)
        t1       = if t0 < t then t0 else (rev t0)
        rev      = addTod 0 0 (-1)
        fwd      = addTod 0 0 1
        toTime h = LocalTime

-- | Another 'TimeSeq' for seconds.
fiveSeconds :: TimeSeq
fiveSeconds t = (iterate rev t1, tail (iterate fwd t1))
  where h0       = todHour (localTimeOfDay t)
        m0       = todMin  (localTimeOfDay t)
        s0       = todSec  (localTimeOfDay t)
        t0       = LocalTime (localDay t) (TimeOfDay h0 m0 s0)
        t1       = if t0 < t then t0 else (rev t0)
        rev      = addTod 0 0 (-5)
        fwd      = addTod 0 0 5
        toTime h = LocalTime

-- | A 'TimeSeq' for minutes.
minutes :: TimeSeq
minutes t = (iterate rev t1, tail (iterate fwd t1))
  where h0       = todHour (localTimeOfDay t)
        m0       = todMin  (localTimeOfDay t)
        t0       = LocalTime (localDay t) (TimeOfDay h0 m0 0)
        t1       = if t0 < t then t0 else (rev t0)
        rev      = addTod 0 (-1)0
        fwd      = addTod 0 1   0
        toTime h = LocalTime

-- | Another 'TimeSeq' for minutes.
fiveMinutes :: TimeSeq
fiveMinutes t = (iterate rev t1, tail (iterate fwd t1))
  where h0       = todHour (localTimeOfDay t)
        m0       = todMin  (localTimeOfDay t)
        t0       = LocalTime (localDay t) (TimeOfDay h0 m0 0)
        t1       = if t0 < t then t0 else (rev t0)
        rev      = addTod 0 (-5)0
        fwd      = addTod 0 5   0
        toTime h = LocalTime

-- | A 'TimeSeq' for hours.
hours :: TimeSeq
hours t = (iterate rev t1, tail (iterate fwd t1))
  where h0       = todHour (localTimeOfDay t)
        t0       = LocalTime (localDay t) (TimeOfDay h0 0 0)
        t1       = if t0 < t then t0 else (rev t0)
        rev      = addTod (-1) 0 0
        fwd      = addTod 1    0 0
        toTime h = LocalTime

-- | A 'TimeSeq' for calendar days.
days :: TimeSeq
days t = (map toTime $ iterate rev t1, map toTime $ tail (iterate fwd t1))
  where t0       = localDay t
        t1       = if (toTime t0) < t then t0 else (rev t0)
        rev      = pred
        fwd      = succ
        toTime d = LocalTime d midnight

-- | A 'TimeSeq' for calendar months.
months :: TimeSeq
months t = (map toTime $ iterate rev t1, map toTime $ tail (iterate fwd t1))
  where t0       = let (y,m,d) = toGregorian $ localDay t in fromGregorian y m 1
        t1       = if toTime t0 < t then t0 else (rev t0)
        rev      = addGregorianMonthsClip (-1)
        fwd      = addGregorianMonthsClip 1
        toTime d = LocalTime d midnight

-- | A 'TimeSeq' for calendar years.
years :: TimeSeq
years t = (map toTime $ iterate rev t1, map toTime $ tail (iterate fwd t1))
  where t0       = let (y,m,d) = toGregorian $ localDay t in y
        t1       = if toTime t0 < t then t0 else (rev t0)
        rev      = pred
        fwd      = succ
        toTime y = LocalTime (fromGregorian y 1 1) midnight

-- | A 'TimeSeq' for no sequence at all.
noTime :: TimeSeq
noTime t = ([],[])

-- | Automatically choose a suitable time axis, based upon the time range
--   of data.  The values to be plotted against this axis can be created
--   with 'doubleFromLocalTime'.
autoTimeAxis :: AxisFn LocalTime
autoTimeAxis pts
    | null pts              = timeAxis days    days    (ft "%d-%b-%y")
                                               noTime  (ft "") []
    | tdiff==0 && dsec<32   = timeAxis seconds seconds (ft "%Ss")
                                               minutes (ft "%d-%b-%y %H:%M") pts
    | tdiff==0 && dsec<120  = timeAxis seconds fiveSeconds (ft "%Ss")
                                               minutes (ft "%d-%b-%y %H:%M") pts
    | tdiff==0 && dmin<7    = timeAxis fiveSeconds minutes (ft "%H:%M")
                                               hours   (ft "%d-%b-%y") pts
    | tdiff==0 && dmin<18   = timeAxis minutes minutes (ft "%H:%M")
                                               hours   (ft "%d-%b-%y") pts
    | tdiff==0 && dmin<32   = timeAxis minutes minutes (ft "%Mm")
                                               hours   (ft "%d-%b-%y %H:00") pts
    | tdiff==0 && dmin<90   = timeAxis minutes fiveMinutes (ft "%Mm")
                                               hours   (ft "%d-%b-%y %H:00") pts
    | tdiff < 2 && dhour<4  = timeAxis fiveMinutes hours (ft "%H:%M")
                                                   days  (ft "%d-%b-%y") pts
    | tdiff < 2 && dhour<16 = timeAxis hours  hours  (ft "%H:%M")
                                              days   (ft "%d-%b-%y") pts
    | tdiff < 2 && dhour<32 = timeAxis hours  hours  (ft "%Hh")
                                              days   (ft "%d-%b-%y") pts
    | tdiff < 4             = timeAxis hours  days   (ft "%d-%b-%y")
                                              noTime (ft "") pts
    | tdiff < 12            = timeAxis days   days   (ft "%d-%b")
                                              years  (ft "%Y")      pts
    | tdiff < 45            = timeAxis days   days   (ft "%d")
                                              months (ft "%b-%y") pts
    | tdiff < 95            = timeAxis days   months (ft "%b-%y")
                                              noTime (ft "") pts
    | tdiff < 450           = timeAxis months months (ft "%b-%y")
                                              noTime (ft "") pts
    | tdiff < 735           = timeAxis months months (ft "%b")
                                              years  (ft "%Y") pts
    | tdiff < 1800          = timeAxis months years (ft "%Y") noTime (ft "") pts
    | otherwise             = timeAxis years  years (ft "%Y") noTime (ft "") pts
  where
    tdiff = diffDays (localDay t1) (localDay t0)
    dhour = if tdiff==0 then h1-h0 else 24*fromIntegral tdiff +h1-h0
    dmin  = 60*dhour+(m1-m0)
    dsec  = fromIntegral (60*dmin) + (s1-s0)
    (TimeOfDay h0 m0 s0) = localTimeOfDay t0
    (TimeOfDay h1 m1 s1) = localTimeOfDay t1
    t1    = maximum pts
    t0    = minimum pts
    ft    = formatTime defaultTimeLocale

