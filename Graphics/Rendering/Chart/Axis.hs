-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Axis where

import qualified Graphics.Rendering.Cairo as C
import Data.Time
import System.Locale (defaultTimeLocale)
import Control.Monad
import Data.List

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Renderable

-- | The concrete data type for an axis
data Axis x =  Axis {
		   
    -- | The axis_viewport function maps values into device
    -- cordinates.
    axis_viewport :: Range -> x -> Double,

    -- | The title string to be displayed on the axis. An
    -- empty string means no title.
    axis_title :: String,

    -- | The tick marks on the axis as pairs.
    -- The first element is the position on the axis
    -- (in viewport units) and the second element is the
    -- length of the tick in output coordinates.
    -- The tick starts on the axis, and positive number are drawn
    -- towards the plot area.
    axis_ticks :: [(x,Double)],
    
    -- | The labels on an axis as pairs. The first element 
    -- is the position on the axis (in viewport units) and
    -- the second is the label text string.
    axis_labels :: [ (x, String) ],

    -- | The positions on the axis (in viewport units) where
    -- we want to show grid lines.
    axis_grid :: [ x ],

    -- | How far the labels are to be drawn from the axis.
    axis_label_gap :: Double,

    axis_title_style :: CairoFontStyle,
    axis_line_style :: CairoLineStyle,
    axis_label_style :: CairoFontStyle,
    axis_grid_style :: CairoLineStyle

}

-- | Function type to generate an optional axis given a set
-- of points to be plotted against that axis.
type AxisFn x = [x] -> Maybe (Axis x)

-- | Function type to generate a pair of axes (either top 
-- and bottom, or left and right), given the set of points to
-- be plotted against each of them.
type AxesFn x x' = [x] -> [x'] -> (Maybe (Axis x), Maybe (Axis x'))

data AxisT x = AxisT RectEdge (Axis x)

instance ToRenderable (AxisT x) where
  toRenderable at = Renderable {
     minsize=minsizeAxis at,
     render=renderAxis at
  }

minsizeAxis :: AxisT x -> CRender RectSize
minsizeAxis (AxisT at a) = do
    let labels = map snd (axis_labels a)
    labelSizes <- preserveCState $ do
        setFontStyle (axis_label_style a)
        mapM textSize labels
    let (lw,lh) = foldl maxsz (0,0) labelSizes
    let ag = axis_label_gap a
    let tsize = maximum [ max 0 (-l) | (v,l) <- axis_ticks a ]
    let sz = case at of
		     E_Top    -> (lw,max (addIfNZ lh ag) tsize)
		     E_Bottom -> (lw,max (addIfNZ lh ag) tsize)
		     E_Left   -> (max (addIfNZ lw ag) tsize, lh)
		     E_Right  -> (max (addIfNZ lw ag) tsize, lh)
    return sz

  where
    maxsz (w1,h1) (w2,h2) = (max w1 w2, max h1 h2)
    addIfNZ a b | a == 0 = 0
                | otherwise = a+b


-- | Calculate the amount by which the labels extend beyond
-- the ends of the axis
axisOverhang :: Ord x => AxisT x -> CRender (Double,Double)
axisOverhang (AxisT at a) = do
    let labels = map snd (sort (axis_labels a))
    labelSizes <- preserveCState $ do
        setFontStyle (axis_label_style a)
        mapM textSize labels
    case labelSizes of
        [] -> return (0,0)
	ls  -> let l1 = head ls
		   l2 = last ls
		   ohangv = return (snd l1 / 2, snd l2 / 2)
		   ohangh = return (fst l1 / 2, fst l2 / 2)
		   in
		   case at of
		       E_Top -> ohangh
		       E_Bottom -> ohangh
		       E_Left -> ohangv
		       E_Right -> ohangh

renderAxis :: AxisT x -> Rect -> CRender ()
renderAxis at@(AxisT et a) rect = do
   let ls = axis_line_style a
   preserveCState $ do
       setLineStyle ls{line_cap=C.LineCapSquare}
       strokeLines [Point sx sy,Point ex ey]
   preserveCState $ do
       setLineStyle ls{line_cap=C.LineCapButt}
       mapM_ drawTick (axis_ticks a)
   preserveCState $ do
       setFontStyle (axis_label_style a)
       mapM_ drawLabel (axis_labels a)
 where
   (sx,sy,ex,ey,tp,axisPoint) = axisMapping at rect

   drawTick (value,length) = 
       let t1 = axisPoint value
	   t2 = t1 `pvadd` (vscale length tp)
       in strokeLines [t1,t2]

   (hta,vta,lp) = 
       let g = axis_label_gap a
       in case et of
		  E_Top    -> (HTA_Centre,VTA_Bottom,(Vector 0 (-g)))
		  E_Bottom -> (HTA_Centre,VTA_Top,(Vector 0 g))
		  E_Left   -> (HTA_Right,VTA_Centre,(Vector (-g) 0))
		  E_Right  -> (HTA_Left,VTA_Centre,(Vector g 0))

   drawLabel (value,s) = do
       drawText hta vta (axisPoint value `pvadd` lp) s

axisMapping :: AxisT z -> Rect -> (Double,Double,Double,Double,Vector,z->Point)
axisMapping (AxisT et a) rect = case et of
    E_Top    -> (x1,y2,x2,y2, (Vector 0 1),    mapx (x1,x2) y2) 
    E_Bottom -> (x1,y1,x2,y1, (Vector 0 (-1)), mapx (x1,x2) y1)
    E_Left   -> (x2,y2,x2,y1, (Vector (1) 0),  mapy (y1,y2) x2)		
    E_Right  -> (x1,y2,x1,y1, (Vector (-1) 0), mapy (y1,y2) x1)
  where
    (Rect (Point x1 y1) (Point x2 y2)) = rect

    mapx xr y x = Point (axis_viewport a xr x) y
    mapy (yr0,yr1) x y = Point x (axis_viewport a (yr1,yr0) y)

renderAxisGrid :: Rect -> AxisT z -> CRender ()
renderAxisGrid rect@(Rect p1 p2) at@(AxisT re a) = do
    preserveCState $ do
        setLineStyle (axis_grid_style a)
        mapM_ (drawGridLine re) (axis_grid a)
  where
    (sx,sy,ex,ey,tp,axisPoint) = axisMapping at rect

    drawGridLine E_Top = vline
    drawGridLine E_Bottom = vline
    drawGridLine E_Left = hline
    drawGridLine E_Right = hline

    vline v = let v' = p_x (axisPoint v)
	      in strokeLines [Point v' (p_y p1),Point v' (p_y p2)]

    hline v = let v' = p_y (axisPoint v)
	      in strokeLines [Point (p_x p1) v',Point (p_x p2) v']

----------------------------------------------------------------------

steps:: Double -> Range -> [Rational]
steps nSteps (min,max) = [ (fromIntegral (min' + i)) * s | i <- [0..n] ]
  where
    min' = floor (min / fromRational s)
    max' = ceiling (max / fromRational s)
    n = (max' - min')
    s = chooseStep nSteps (min,max)

chooseStep :: Double -> Range -> Rational
chooseStep nsteps (min,max) = s
  where
    mult = 10 ^^ (floor ((log (max-min) - log nsteps) / log 10))
    steps = map (mult*) [0.1, 0.2, 0.25, 0.5, 1.0, 2.0, 2.5, 5.0, 10, 20, 25, 50]
    steps' =  sort [ (abs((max-min)/(fromRational s) - nsteps), s) | s <- steps ]
    s = snd (head steps')

-- | Explicitly specify an axis
explicitAxis :: Maybe (Axis x)	 -> AxisFn x
explicitAxis ma _ = ma

autoAxis :: PlotValue x => (x -> String) -> ([x],[x],[x]) -> Axis x -> Maybe (Axis x)
autoAxis labelf (labelvs, tickvs, gridvs) a = Just axis
  where
    axis =  a {
        axis_viewport=newViewport,
	axis_ticks=newTicks,
	axis_grid=if null (axis_grid a) then [] else gridvs,
	axis_labels=newLabels
	}
    newViewport = vmap (min',max')
    newTicks = [ (v,2) | v <- tickvs ] ++ [ (v,5) | v <- labelvs ] 
    newLabels = [(v,labelf v) | v <- labelvs]
    min' = minimum labelvs
    max' = maximum labelvs

data LinearAxisParams = LinearAxisParams {
    -- | The function used to show the axes labels
    la_labelf :: Double -> String,

    -- | The target number of labels to be shown
    la_nLabels :: Int,

    -- | The target number of ticks to be shown
    la_nTicks :: Int,

    -- | If True, the grid is shown at the label values, otherwise the
    -- otherwise the grid is shown at the tick values
    la_gridAtMinor :: Bool
}

defaultLinearAxis = LinearAxisParams {
    la_labelf = showD,
    la_nLabels = 5,
    la_nTicks = 50,
    la_gridAtMinor = False
}

-- | Generate a linear axis automatically.
-- The supplied axis is used as a template, with the viewport, ticks, labels
-- and grid set appropriately for the data displayed against that axies.
-- The resulting axis will only show a grid if the template has some grid
-- values.
autoScaledAxis' :: LinearAxisParams -> Axis Double -> AxisFn Double
autoScaledAxis' lap a ps0 = autoAxis (la_labelf lap) (labelvs,tickvs,gridvs) a
  where
    ps = filter isValidNumber ps0
    (min,max) = (minimum ps,maximum ps)
    range [] = (0,1)
    range _  | min == max = (min-0.5,min+0.5)
	     | otherwise = (min,max)
    labelvs = map fromRational $ steps (fromIntegral (la_nLabels lap)) r
    tickvs = map fromRational $ steps (fromIntegral (la_nTicks lap)) (minimum labelvs,maximum labelvs)
    gridvs = if la_gridAtMinor lap then tickvs else labelvs
    r = range ps

-- | Generate a linear axis automatically.
-- Same as autoScaledAxis', but with labels generated with "showD"
-- (showD is show for doubles, but with any trailing ".0" removed)
autoScaledAxis :: Axis Double -> AxisFn Double
autoScaledAxis = autoScaledAxis' defaultLinearAxis

showD x = case reverse $ show x of
            '0':'.':r -> reverse r
            _ -> show x
    

log10 :: (Floating a) => a -> a
log10 = logBase 10

frac x | 0 <= b = (a,b)
       | otherwise = (a-1,b+1)
 where
  (a,b) = properFraction x

{- 
 Rules: Do no subdivide between powers of 10 until all powers of 10
          get a major ticks.
        Do not subdivide between powers of ten as [1,2,4,6,8,10] when
          5 gets a major ticks 
          (ie the major ticks need to be a subset of the minor tick)
-}
logTicks :: Range -> ([Rational],[Rational],[Rational])
logTicks (low,high) = (major,minor,major)
 where
  ratio = high/low
  lower a l = let (i,r) = frac (log10 a) in
            (maximum (1:(filter (\x -> log10 (fromRational x) <= r) l)))*10^^i
  upper a l = let (i,r) = properFraction (log10 a) in
            (minimum (10:(filter (\x -> r <= log10 (fromRational x)) l)))*10^^i
  inRange (a,b) l x = (lower a l <= x) && (x <= upper b l)
  powers :: (Double,Double) -> [Rational] -> [Rational]
  powers (x,y) l = [a*10^^p | p<-[(floor (log10 x))..(ceiling (log10 y))], a<-l]
  midselection r l = filter (inRange r l) (powers r l)
  major | 17.5 < log10 ratio = map (\x -> 10^^(round x)) $
                         steps (min 5 (log10 ratio)) (log10 low, log10 high)
        | 12 < log10 ratio = map (\x -> 10^^(round x)) $
                         steps ((log10 ratio)/5) (log10 low, log10 high)
        | 6 < log10 ratio = map (\x -> 10^^(round x)) $
                         steps ((log10 ratio)/2) (log10 low, log10 high)
        | 3 < log10 ratio = midselection (low,high) [1,10]
        | 20 < ratio = midselection (low,high) [1,5,10]
        | 6 < ratio = midselection (low,high) [1,2,4,6,8,10]
        | 3 < ratio = midselection (low,high) [1..10]
        | otherwise = steps 5 (low,high)
  (l',h') = (minimum major, maximum major)
  (dl',dh') = (fromRational l', fromRational h')
  ratio' = fromRational (h'/l')
  minor | 50 < log10 ratio' = map (\x -> 10^^(round x)) $
                              steps 50 (log10 $ dl', log10 $ dh')
        | 6 < log10 ratio' = filter (\x -> l'<=x && x <=h') $
                             powers (dl', dh') [1,10]
        | 3 < log10 ratio' = filter (\x -> l'<=x && x <=h') $
                             powers (dl',dh') [1,5,10]
        | 6 < ratio' = filter (\x -> l'<=x && x <=h') $ 
                       powers (dl',dh') [1..10]
        | 3 < ratio' = filter (\x -> l'<=x && x <=h') $ 
                       powers (dl',dh') [1,1.2..10]
        | otherwise = steps 50 (dl', dh')

-- | Generate a log axis automatically.
-- The supplied axis is used as a template, with the viewport, ticks, labels
-- and grid set appropriately for the data displayed against that axies.
-- The resulting axis will only show a grid if the template has some grid
-- values.
autoScaledLogAxis' :: (LogValue->String) -> Axis LogValue -> AxisFn LogValue
autoScaledLogAxis' labelf a ps0 = autoAxis labelf (wrap rlabelvs, wrap rtickvs, wrap rgridvs) a
  where
    ps = filter (\(LogValue x) -> isValidNumber x && 0 < x) ps0
    (min, max) = (minimum ps,maximum ps)
    range [] = (3,30)
    range _  | min == max = (unLogValue min/3,unLogValue max*3)
             | otherwise = (unLogValue min,unLogValue max)
    (rlabelvs, rtickvs, rgridvs) = logTicks (range ps)
    wrap = map (LogValue . fromRational)
    unLogValue (LogValue x) = x

-- | Generate a log axis automatically.
-- Same as autoScaledLogAxis', but with labels generated with "showD"
-- (showD is show for doubles, but with any trailing ".0" removed)
autoScaledLogAxis :: Axis LogValue -> AxisFn LogValue
autoScaledLogAxis = autoScaledLogAxis' (\(LogValue x) -> showD x)

-- | Show independent axes on each side of the layout
independentAxes :: AxisFn x -> AxisFn x' -> AxesFn x x'
independentAxes af1 af2 pts1 pts2 = (af1 pts1, af2 pts2)

-- | Show the same axis on both sides of the layout
linkedAxes :: AxisFn x -> AxesFn x x
linkedAxes af pts1 pts2 = (a,a)
  where
    a = af (pts1++pts2)

-- | Show the same axis on both sides of the layout, but with labels
-- only on the primary side
linkedAxes' :: AxisFn x -> AxesFn x x
linkedAxes' af pts1 pts2 = (a,removeLabels a)
  where
    a  = af (pts1++pts2)
    removeLabels = liftM (\a -> a{axis_title="",axis_labels = []})

----------------------------------------------------------------------

defaultAxisLineStyle = solidLine 1 black
defaultGridLineStyle = dashedLine 1 [5,5] grey8

defaultAxis = Axis {
    axis_viewport = error "Axis.defaultAxis.axis_viewport does not exist",
    axis_title = "",
    axis_ticks = [],
    axis_labels = [],
    axis_grid = [error "Axis.defaultAxis.axis_grid does not exist"],
    axis_label_gap = 10,
    axis_title_style = defaultFontStyle,
    axis_line_style = defaultAxisLineStyle,
    axis_label_style = defaultFontStyle,
    axis_grid_style = defaultGridLineStyle
}

----------------------------------------------------------------------

-- | Map a LocalTime value to a plot cordinate
doubleFromLocalTime :: LocalTime -> Double
doubleFromLocalTime lt = fromIntegral (toModifiedJulianDay (localDay lt)) 
             + fromRational (timeOfDayToDayFraction (localTimeOfDay lt))

-- | Map a plot cordinate to a LocalTime
localTimeFromDouble :: Double -> LocalTime
localTimeFromDouble v = 
  LocalTime (ModifiedJulianDay i) (dayFractionToTimeOfDay (toRational d))
 where
   (i,d) = properFraction v

-- | TimeSeq is a (potentially infinite) set of times. When passes
-- a reference time, the function returns a a pair of lists. The first
-- contains all times in the set less than the reference time in
-- decreasing order. The second contains all times in the set greater
-- than or equal to the reference time, in increasing order.
type TimeSeq = LocalTime-> ([LocalTime],[LocalTime])

coverTS tseq min max = min' ++ enumerateTS tseq min max ++ max'
  where
    min' =  if elemTS min tseq then [] else take 1 (fst (tseq min))
    max' =  if elemTS max tseq then [] else take 1 (snd (tseq max))

enumerateTS tseq min max = reverse (takeWhile (>=min) ts1)  ++ takeWhile (<=max) ts2
  where
    (ts1,ts2) = tseq min

elemTS t tseq = case tseq t of
    (_,(t0:_)) | t == t0 -> True
    _                    -> False

-- | How to display a time
type TimeLabelFn = LocalTime -> String

-- | Create an 'AxisFn' to for a time axis. The first 'TimeSeq' sets the minor ticks,
-- and the ultimate range will aligned to it's elements. The second 'TimeSeq' sets
-- the labels and grid. The 'TimeLabelFn' is used to format LocalTimes for labels.
-- The values to be plotted against this axis can be created with 'doubleFromLocalTime'
timeAxis :: TimeSeq -> TimeSeq -> TimeLabelFn -> Axis LocalTime -> AxisFn LocalTime
timeAxis tseq lseq labelf a pts = Just axis
  where
    axis =  a {
        axis_viewport=vmap(min', max'),
	axis_ticks=[ (t,2) | t <- times] ++ [ (t,5) | t <- ltimes, visible t],
	axis_labels=[ (t,l) | (t,l) <- labels, visible t],
	axis_grid=[ t | t <- ltimes, visible t]
	}
    (min,max) = case pts of
		[] -> (refLocalTime,refLocalTime)
		ps -> (minimum ps, maximum ps)
    refLocalTime = LocalTime (ModifiedJulianDay 0) midnight
    times = coverTS tseq min max
    ltimes = coverTS lseq min max
    min' = minimum times
    max' = maximum times
    visible t = min' <= t && t <= max'
    labels = [ (avg m1 m2, labelf m1) | (m1,m2) <- zip ltimes (tail ltimes) ]
    avg m1 m2 = localTimeFromDouble $ m1' + (m2' - m1')/2
     where
      m1' = doubleFromLocalTime m1
      m2' = doubleFromLocalTime m2

-- | A 'TimeSeq' for calendar days
days :: TimeSeq
days t = (map toTime $ iterate rev t1, map toTime $ tail (iterate fwd t1))
  where t0 = (localDay t)
        t1 = if (toTime t0) < t then t0 else (rev t0)
        rev = pred
        fwd = succ
        toTime d = LocalTime d midnight

-- | A 'TimeSeq' for calendar months
months :: TimeSeq
months t = (map toTime $ iterate rev t1, map toTime $ tail (iterate fwd t1))
  where t0 = let (y,m,d) = toGregorian $ localDay t in fromGregorian y m 1
        t1 = if toTime t0 < t then t0 else (rev t0)
        rev = addGregorianMonthsClip (-1)
        fwd = addGregorianMonthsClip 1
        toTime d = LocalTime d midnight

-- | A 'TimeSeq' for calendar years
years :: TimeSeq
years t = (map toTime $ iterate rev t1, map toTime $ tail (iterate fwd t1))
  where t0 = let (y,m,d) = toGregorian $ localDay t in y
        t1 = if toTime t0 < t then t0 else (rev t0)
        rev = pred
        fwd = succ
        toTime y = LocalTime (fromGregorian y 1 1) midnight

-- | Automatically choose a suitable time axis, based upon the time range of data.
-- The values to be plotted against this axis can be created with 'doubleFromLocalTime'
autoTimeAxis :: Axis LocalTime -> AxisFn LocalTime
autoTimeAxis a [] = timeAxis days days (formatTime defaultTimeLocale "%d-%b")  a []
autoTimeAxis a pts = 
    if tdiff < 15
    then  timeAxis days days (ft "%d-%b")  a pts
    else if tdiff < 90
         then timeAxis days months (ft "%b-%y") a pts
         else if tdiff < 450
              then timeAxis months months (ft "%b-%y") a pts
              else if tdiff < 1800
                   then timeAxis months years (ft "%Y") a pts
                   else timeAxis years years (ft "%Y") a pts
  where
    tdiff = diffDays (localDay t1) (localDay t0)
    t1 = maximum pts
    t0 = minimum pts
    ft = formatTime defaultTimeLocale

-----------------------------------------------------------------------------

class Ord a => PlotValue a where
 toValue :: a -> Double

instance PlotValue Double where
 toValue = id

newtype LogValue = LogValue Double
                    deriving (Eq, Ord)

instance Show LogValue where
 show (LogValue x) = show x

instance PlotValue LogValue where
 toValue (LogValue x) = log x

instance PlotValue LocalTime where
 toValue = doubleFromLocalTime

-- | A linear mapping of points in one range to another
vmap :: PlotValue x => (x,x) -> Range -> x -> Double
vmap (v1,v2) (v3,v4) v = v3 + (toValue v - toValue v1) * (v4-v3) / (toValue v2 - toValue v1)
