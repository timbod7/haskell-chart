{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.Time
-- Copyright   :  (c) Tim Docker 2010, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Calculate and render time axes

module Graphics.Rendering.Chart.Axis.Time(
    TimeSeq,
    TimeLabelFn,
    TimeLabelAlignment(..),

    TimeValue (..),

    timeValueAxis,
    autoTimeValueAxis,

    days, months, years,

    ) where

import Data.Default.Class
#if MIN_VERSION_time(1,5,0)
import Data.Time hiding (months)
#else
import Data.Time
import System.Locale (defaultTimeLocale)
#endif
import Data.Fixed
import Control.Lens

import Graphics.Rendering.Chart.Axis.Types
import Graphics.Rendering.Chart.Geometry (Range)

-- | A typeclass abstracting the functions we need
-- to be able to plot against an axis of time type @d@.
class TimeValue t where
    utctimeFromTV :: t -> UTCTime
    tvFromUTCTime :: UTCTime -> t

    {-# MINIMAL utctimeFromTV, tvFromUTCTime #-}

    doubleFromTimeValue  :: t -> Double
    doubleFromTimeValue = doubleFromTimeValue . utctimeFromTV

    timeValueFromDouble  :: Double -> t
    timeValueFromDouble = tvFromUTCTime . timeValueFromDouble

instance TimeValue UTCTime where
    utctimeFromTV       = id
    tvFromUTCTime       = id
    doubleFromTimeValue = doubleFromUTCTime
    timeValueFromDouble = utcTimeFromDouble

instance TimeValue Day where
    utctimeFromTV d     = UTCTime d 0
    tvFromUTCTime       = utctDay
    doubleFromTimeValue = doubleFromDay
    timeValueFromDouble = dayFromDouble

instance TimeValue LocalTime where
    utctimeFromTV (LocalTime d tod) = UTCTime d (timeOfDayToTime tod)
    tvFromUTCTime (UTCTime d dt)    = LocalTime d (timeToTimeOfDay dt)

----------------------------------------------------------------------

instance PlotValue LocalTime where
    toValue    = doubleFromTimeValue
    fromValue  = timeValueFromDouble
    autoAxis   = autoTimeValueAxis

instance PlotValue UTCTime where
    toValue    = doubleFromTimeValue
    fromValue  = timeValueFromDouble
    autoAxis   = autoTimeValueAxis

instance PlotValue Day where
    toValue    = doubleFromTimeValue
    fromValue  = timeValueFromDouble
    autoAxis   = autoTimeValueAxis

----------------------------------------------------------------------

-- | Map a UTCTime value to a plot coordinate.
doubleFromUTCTime :: UTCTime -> Double
doubleFromUTCTime ut = fromIntegral (toModifiedJulianDay (utctDay ut))
              + fromRational (timeOfDayToDayFraction (timeToTimeOfDay (utctDayTime ut)))

-- | Map a plot coordinate to a UTCTime.
utcTimeFromDouble :: Double -> UTCTime
utcTimeFromDouble v =
  UTCTime (ModifiedJulianDay i) (timeOfDayToTime (dayFractionToTimeOfDay (toRational d)))
 where
   (i,d) = properFraction v

-- | Map a Day value to a plot coordinate.
doubleFromDay :: Day -> Double
doubleFromDay d = fromIntegral (toModifiedJulianDay d)

-- | Map a plot coordinate to a Day.
dayFromDouble :: Double -> Day
dayFromDouble v = ModifiedJulianDay (truncate v)

----------------------------------------------------------------------

-- | TimeSeq is a (potentially infinite) set of times. When passed
--   a reference time, the function returns a a pair of lists. The first
--   contains all times in the set less than the reference time in
--   decreasing order. The second contains all times in the set greater
--   than or equal to the reference time, in increasing order.
type TimeSeq = UTCTime -> ([UTCTime],[UTCTime])

coverTS :: TimeSeq -> UTCTime -> UTCTime -> [UTCTime]
coverTS tseq minT maxT = min' ++ enumerateTS tseq minT maxT ++ max'
  where
    min' =  if elemTS minT tseq then [] else take 1 (fst (tseq minT))
    max' =  if elemTS maxT tseq then [] else take 1 (snd (tseq maxT))

enumerateTS :: TimeSeq -> UTCTime -> UTCTime -> [UTCTime]
enumerateTS tseq minT maxT =
    reverse (takeWhile (>=minT) ts1)  ++ takeWhile (<=maxT) ts2
  where
    (ts1,ts2) = tseq minT

elemTS :: UTCTime -> TimeSeq -> Bool
elemTS t tseq = case tseq t of
    (_,t0:_) | t == t0 -> True
    _                  -> False

-- | How to display a time
type TimeLabelFn = UTCTime -> String

data TimeLabelAlignment = UnderTicks
                        | BetweenTicks
                        deriving (Show)

-- | Create an 'AxisFn' to for a time axis.
--
--   The values to be plotted against this axis can be created with
--   'doubleFromLocalTime'.
--
--   Implementation detail: 'PlotValue' constraint is needed to use `vmap`.
timeValueAxis ::
  TimeValue t
  => TimeSeq
  -- ^ Set the minor ticks, and the final range will be aligned to its
  --   elements.
  -> TimeSeq
  -- ^ Set the labels and grid.
  -> TimeLabelFn
  -> TimeLabelAlignment
  -> TimeSeq
  -- ^ Set the second line of labels.
  -> TimeLabelFn
  -- ^ Format @t@ for labels.
  -> TimeLabelAlignment
  -> AxisFn t
timeValueAxis tseq lseq labelf lal cseq contextf clal pts = AxisData {
    _axis_visibility = def,
    _axis_viewport = vmap' (min', max'),
    _axis_tropweiv = invmap' (min', max'),
    _axis_ticks    = [ (tvFromUTCTime t,2) | t <- times] ++ [ (tvFromUTCTime t,5) | t <- ltimes, visible t],
    _axis_labels   = [ [ (tvFromUTCTime t,l) | (t,l) <- labels labelf   ltimes lal, visible t]
                     , [ (tvFromUTCTime t,l) | (t,l) <- labels contextf ctimes clal, visible t]
                     ],
    _axis_grid     = [ tvFromUTCTime t     | t <- ltimes, visible t]
    }
  where
    (minT,maxT)  = case pts of
                       [] -> (refTimeValue,refTimeValue)
                       ps -> (minimum (map utctimeFromTV ps), maximum (map utctimeFromTV ps))
    refTimeValue = timeValueFromDouble 0

    times, ltimes, ctimes :: [UTCTime]
    times        = coverTS tseq minT maxT
    ltimes       = coverTS lseq minT maxT
    ctimes       = coverTS cseq minT maxT
    min'         = minimum times
    max'         = maximum times
    visible t    = min' <= t && t <= max'
    labels f ts lal' =
        [ (align lal' m1' m2', f m1)
          | (m1,m2) <- zip ts (tail ts)
          , let m1' = if m1<min' then min' else m1
          , let m2' = if m2>max' then max' else m2 ]

    align BetweenTicks m1 m2 = avg m1 m2
    align UnderTicks   m1 _  = m1

    avg m1 m2    = timeValueFromDouble $ m1' + (m2' - m1')/2
     where
      m1' = doubleFromTimeValue m1
      m2' = doubleFromTimeValue m2

vmap' :: TimeValue x => (UTCTime,UTCTime) -> Range -> x -> Double
vmap' (v1,v2) (v3,v4) v = v3 + (doubleFromTimeValue v - doubleFromTimeValue v1) * (v4-v3)
                              / (doubleFromTimeValue v2 - doubleFromTimeValue v1)

invmap' :: TimeValue x => (UTCTime,UTCTime) -> Range -> Double -> x
invmap' (v3,v4) (d1,d2) d = timeValueFromDouble (doubleFromTimeValue v3 + ( (d-d1) * doubleRange
                                                   / (d2-d1) ))
    where doubleRange = doubleFromTimeValue v4 - doubleFromTimeValue v3

truncateTo :: Real a => a -> a -> a
truncateTo t step = t - t `mod'` step

secondSeq :: NominalDiffTime -> TimeSeq
secondSeq step t@(UTCTime day dt) = (iterate rev t1, tail (iterate fwd t1))
  where t0       = UTCTime day (truncateTo dt step')
        t1       = if t0 < t then t0 else rev t0
        rev      = addUTCTime (negate step)
        fwd      = addUTCTime step
        step'    = realToFrac step

millis1, millis10, millis100, seconds, fiveSeconds :: TimeSeq
millis1 = secondSeq (1 / 1000)
millis10 = secondSeq (1 / 100)
millis100 = secondSeq (1 / 10)
seconds = secondSeq 1
fiveSeconds = secondSeq 5

minutes, fiveMinutes :: TimeSeq
minutes = secondSeq 60
fiveMinutes = secondSeq (5 * 60)

-- | A 'TimeSeq' for hours.
hours :: TimeSeq
hours = secondSeq (60 * 60)

-- | A 'TimeSeq' for calendar days.
days :: TimeSeq
days t = (map toTime $ iterate rev t1, map toTime $ tail (iterate fwd t1))
  where t0       = utctDay t
        t1       = if toTime t0 < t then t0 else rev t0
        rev      = pred
        fwd      = succ
        toTime d = UTCTime d 0

-- | A 'TimeSeq' for calendar months.
months :: TimeSeq
months t = (map toTime $ iterate rev t1, map toTime $ tail (iterate fwd t1))
  where t0       = let (y,m,_) = toGregorian $ utctDay t in fromGregorian y m 1
        t1       = if toTime t0 < t then t0 else rev t0
        rev      = addGregorianMonthsClip (-1)
        fwd      = addGregorianMonthsClip 1
        toTime d = UTCTime d 0

-- | A 'TimeSeq' for calendar years.
years :: TimeSeq
years t = (map toTime $ iterate rev t1, map toTime $ tail (iterate fwd t1))
  where t0       = toGregorian (utctDay t) ^. _1
        t1       = if toTime t0 < t then t0 else rev t0
        rev      = pred
        fwd      = succ
        toTime y = UTCTime (fromGregorian y 1 1) 0

-- | A 'TimeSeq' for no sequence at all.
noTime :: TimeSeq
noTime _ = ([],[])

-- | Automatically choose a suitable time axis, based upon the time range
--   of data.  The values to be plotted against this axis can be created
--   with 'doubleFromTimeValue'.
autoTimeValueAxis :: TimeValue t => AxisFn t
autoTimeValueAxis pts
    | null pts     = timeValueAxis days    days    (ft "%d-%b-%y") UnderTicks
                                           noTime  (ft "") UnderTicks []
    | 100*dsec<1   = timeValueAxis millis1   millis1  (ft "%S%Q") UnderTicks
                                             noTime (ft "%S%Q") UnderTicks pts
    | 10*dsec<1    = timeValueAxis millis10  millis10  (ft "%S%Q") UnderTicks
                                             noTime (ft "%S%Q") UnderTicks pts
    | dsec<1       = timeValueAxis millis10  millis100 (ft "%S%Q") UnderTicks
                                             seconds (ft "%M:%S") BetweenTicks pts
    | dsec<5       = timeValueAxis millis100 seconds (ft "%M:%S%Q") UnderTicks
                                             seconds (ft "%M:%S") BetweenTicks pts
    | dsec<32      = timeValueAxis seconds seconds (ft "%Ss") UnderTicks
                                           minutes (ft "%d-%b-%y %H:%M") BetweenTicks pts
    | dsec<120     = timeValueAxis seconds fiveSeconds (ft "%Ss") UnderTicks
                                           minutes (ft "%d-%b-%y %H:%M") BetweenTicks pts
    | dsec<7*60    = timeValueAxis fiveSeconds minutes (ft "%Mm") UnderTicks
                                           hours   (ft "%d-%b-%y %H:00") BetweenTicks pts
    | dsec<32*60   = timeValueAxis minutes minutes (ft "%Mm") UnderTicks
                                           hours   (ft "%d-%b-%y %H:00") BetweenTicks pts
    | dsec<90*60   = timeValueAxis minutes fiveMinutes (ft "%Mm") UnderTicks
                                           hours   (ft "%d-%b-%y %H:00") BetweenTicks pts
    | dsec<4*3600  = timeValueAxis fiveMinutes hours (ft "%H:%M") UnderTicks
                                               days  (ft "%d-%b-%y") BetweenTicks pts
    | dsec<32*3600 = timeValueAxis hours  hours  (ft "%H:%M") UnderTicks
                                          days   (ft "%d-%b-%y") BetweenTicks pts
    | dday<4       = timeValueAxis hours  days   (ft "%d-%b-%y") BetweenTicks
                                          noTime (ft "") BetweenTicks pts
    | dday<12      = timeValueAxis days   days   (ft "%d-%b") BetweenTicks
                                          years  (ft "%Y") BetweenTicks pts
    | dday<45      = timeValueAxis days   days   (ft "%d") BetweenTicks
                                          months (ft "%b-%y") BetweenTicks pts
    | dday<95      = timeValueAxis days   months (ft "%b-%y") BetweenTicks
                                          noTime (ft "") BetweenTicks pts
    | dday<450     = timeValueAxis months months (ft "%b-%y") BetweenTicks
                                          noTime (ft "") BetweenTicks pts
    | dday<735     = timeValueAxis months months (ft "%b") BetweenTicks
                                          years  (ft "%Y") BetweenTicks pts
    | dday<1800    = timeValueAxis months years (ft "%Y") BetweenTicks
                                          noTime (ft "") BetweenTicks pts
    | otherwise    = timeValueAxis years  years (ft "%Y") BetweenTicks
                                          noTime (ft "") BetweenTicks pts
  where
    upts  = map utctimeFromTV pts
    dsec  = diffUTCTime t1 t0  -- seconds
    dday  = dsec / 86400       -- days
    t1    = maximum upts
    t0    = minimum upts
    ft    = formatTime defaultTimeLocale
