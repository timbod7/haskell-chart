-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.LocalTime
-- Copyright   :  (c) Tim Docker 2010
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Calculate and render time axes

module Graphics.Rendering.Chart.Axis.LocalTime(
    timeAxis,
    autoTimeAxis,
    days, months, years
) where
 
import Data.Default.Class
import Data.Time
import Data.Fixed
import System.Locale (defaultTimeLocale)
import Control.Monad
import Data.List
import Control.Lens
import Data.Colour (opaque)
import Data.Colour.Names (black, lightgrey)
import Data.Ord (comparing)

import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
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

data TimeLabelAlignment = UnderTicks
                        | BetweenTicks
                        deriving (Show)

-- | Create an 'AxisFn' to for a time axis.  The first 'TimeSeq' sets the
--   minor ticks, and the ultimate range will be aligned to its elements.
--   The second 'TimeSeq' sets the labels and grid.  The third 'TimeSeq'
--   sets the second line of labels.  The 'TimeLabelFn' is
--   used to format LocalTimes for labels.  The values to be plotted
--   against this axis can be created with 'doubleFromLocalTime'.
timeAxis :: TimeSeq -> TimeSeq -> TimeLabelFn -> TimeLabelAlignment -> 
                       TimeSeq -> TimeLabelFn -> TimeLabelAlignment -> 
            AxisFn LocalTime
timeAxis tseq lseq labelf lal cseq contextf clal pts = AxisData {
    _axis_visibility = def,
    _axis_viewport = vmap(min', max'),
    _axis_tropweiv = invmap(min', max'),
    _axis_ticks    = [ (t,2) | t <- times] ++ [ (t,5) | t <- ltimes, visible t],
    _axis_labels   = [ [ (t,l) | (t,l) <- labels labelf   ltimes lal, visible t]
                     , [ (t,l) | (t,l) <- labels contextf ctimes clal, visible t]
                     ], 
    _axis_grid     = [ t     | t <- ltimes, visible t]
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
    labels f ts lal =
        [ (align lal m1' m2', f m1)
          | (m1,m2) <- zip ts (tail ts)
          , let m1' = if m1<min' then min' else m1
          , let m2' = if m2>max' then max' else m2 ]

    align BetweenTicks m1 m2 = avg m1 m2
    align UnderTicks m1 m2 = m1

    avg m1 m2    = localTimeFromDouble $ m1' + (m2' - m1')/2
     where
      m1' = doubleFromLocalTime m1
      m2' = doubleFromLocalTime m2

normalizeTimeOfDay :: LocalTime -> LocalTime
normalizeTimeOfDay t@(LocalTime day (TimeOfDay h m s))
  | s <  0    = normalizeTimeOfDay (LocalTime day (TimeOfDay h (m-1) (s+60)))
  | m <  0    = normalizeTimeOfDay (LocalTime day (TimeOfDay (h-1) (m+60) s))
  | h <  0    = normalizeTimeOfDay (LocalTime (addDays (-1) day) (TimeOfDay (h+24) m s))
  | s >= 60   = normalizeTimeOfDay (LocalTime day (TimeOfDay h (m+s`div'`60)
                                                               (s`mod'`60)))
  | m >= 60   = normalizeTimeOfDay (LocalTime day (TimeOfDay (h+m`div`60)
                                                             (m`mod`60) s))
  | h >= 24   = LocalTime (addDays (fromIntegral (h`div`24)) day)
                          (TimeOfDay (h`mod`24) m s)
  | otherwise = t

addTod :: Int -> Int -> Pico -> LocalTime -> LocalTime
addTod dh dm ds (LocalTime day (TimeOfDay h m s)) = normalizeTimeOfDay t'
  where t' = LocalTime day (TimeOfDay (h+dh) (m+dm) (s+ds))

truncateTo :: (HasResolution a) => Fixed a -> Fixed a -> Fixed a
truncateTo t step = t - t `mod'` step

secondSeq :: Pico -> TimeSeq
secondSeq step t = (iterate rev t1, tail (iterate fwd t1))
  where h0       = todHour (localTimeOfDay t)
        m0       = todMin  (localTimeOfDay t)
        s0       = todSec  (localTimeOfDay t) `truncateTo` (fromIntegral 1 / 1000)
        t0       = LocalTime (localDay t) (TimeOfDay h0 m0 s0)
        t1       = if t0 < t then t0 else (rev t0)
        rev      = addTod 0 0 (negate step)
        fwd      = addTod 0 0 (step)

millis1, millis10, millis100, seconds, fiveSeconds  :: TimeSeq
millis1 = secondSeq (fromIntegral 1 / 1000)
millis10 = secondSeq (fromIntegral 1 / 100)
millis100 = secondSeq (fromIntegral 1 / 10)
seconds = secondSeq (fromIntegral 1)
fiveSeconds = secondSeq (fromIntegral 5)

minuteSeq :: Int -> TimeSeq
minuteSeq step t = (iterate rev t1, tail (iterate fwd t1))
  where h0       = todHour (localTimeOfDay t)
        m0       = todMin  (localTimeOfDay t)
        t0       = LocalTime (localDay t) (TimeOfDay h0 m0 0)
        t1       = if t0 < t then t0 else (rev t0)
        rev      = addTod 0 (negate step) (fromIntegral 0)
        fwd      = addTod 0 step    (fromIntegral 0)


minutes, fiveMinutes :: TimeSeq
minutes = minuteSeq 1
fiveMinutes = minuteSeq 5

-- | A 'TimeSeq' for hours.
hours :: TimeSeq
hours t = (iterate rev t1, tail (iterate fwd t1))
  where h0       = todHour (localTimeOfDay t)
        t0       = LocalTime (localDay t) (TimeOfDay h0 0 0)
        t1       = if t0 < t then t0 else (rev t0)
        rev      = addTod (-1) 0 (fromIntegral 0)
        fwd      = addTod 1    0 (fromIntegral 0)

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
    | null pts              = timeAxis days    days    (ft "%d-%b-%y") UnderTicks
                                               noTime  (ft "") UnderTicks []
    | tdiff==0 && 100*dsec<1= timeAxis millis1   millis1  (ft "%S%Q") UnderTicks 
                                                 noTime (ft "%S%Q") UnderTicks pts
    | tdiff==0 && 10*dsec<1 = timeAxis millis10  millis10  (ft "%S%Q") UnderTicks 
                                                 noTime (ft "%S%Q") UnderTicks pts
    | tdiff==0 && dsec<1    = timeAxis millis10  millis100 (ft "%S%Q") UnderTicks
                                                 seconds (ft "%M:%S") BetweenTicks pts
    | tdiff==0 && dsec<5    = timeAxis millis100 seconds (ft "%M:%S%Q") UnderTicks
                                                 seconds (ft "%M:%S") BetweenTicks pts
    | tdiff==0 && dsec<32   = timeAxis seconds seconds (ft "%Ss") UnderTicks
                                               minutes (ft "%d-%b-%y %H:%M") BetweenTicks pts
    | tdiff==0 && dsec<120  = timeAxis seconds fiveSeconds (ft "%Ss") UnderTicks
                                               minutes (ft "%d-%b-%y %H:%M") BetweenTicks pts
    | tdiff==0 && dmin<7    = timeAxis fiveSeconds minutes (ft "%Mm") UnderTicks
                                               hours   (ft "%d-%b-%y %H:00") BetweenTicks pts
    | tdiff==0 && dmin<32   = timeAxis minutes minutes (ft "%Mm") UnderTicks
                                               hours   (ft "%d-%b-%y %H:00") BetweenTicks pts
    | tdiff==0 && dmin<90   = timeAxis minutes fiveMinutes (ft "%Mm") UnderTicks
                                               hours   (ft "%d-%b-%y %H:00") BetweenTicks pts
    | tdiff < 2 && dhour<4  = timeAxis fiveMinutes hours (ft "%H:%M") UnderTicks
                                                   days  (ft "%d-%b-%y") BetweenTicks pts
    | tdiff < 2 && dhour<32 = timeAxis hours  hours  (ft "%H:%M") UnderTicks
                                              days   (ft "%d-%b-%y") BetweenTicks pts
    | tdiff < 4             = timeAxis hours  days   (ft "%d-%b-%y") BetweenTicks
                                              noTime (ft "") BetweenTicks pts
    | tdiff < 12            = timeAxis days   days   (ft "%d-%b") BetweenTicks
                                              years  (ft "%Y") BetweenTicks pts
    | tdiff < 45            = timeAxis days   days   (ft "%d") BetweenTicks
                                              months (ft "%b-%y") BetweenTicks pts
    | tdiff < 95            = timeAxis days   months (ft "%b-%y") BetweenTicks
                                              noTime (ft "") BetweenTicks pts
    | tdiff < 450           = timeAxis months months (ft "%b-%y") BetweenTicks
                                              noTime (ft "") BetweenTicks pts
    | tdiff < 735           = timeAxis months months (ft "%b") BetweenTicks
                                              years  (ft "%Y") BetweenTicks pts
    | tdiff < 1800          = timeAxis months years (ft "%Y") BetweenTicks
                                              noTime (ft "") BetweenTicks pts
    | otherwise             = timeAxis years  years (ft "%Y") BetweenTicks
                                              noTime (ft "") BetweenTicks pts
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


