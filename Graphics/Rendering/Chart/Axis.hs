-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Axis where

import qualified Graphics.Rendering.Cairo as C
import System.Time
import System.Locale
import Control.Monad
import Data.List

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Renderable

-- | The concrete data type for an axis
data Axis =  Axis {
		   
    -- | The axis_viewport function maps values into device
    -- cordinates.
    axis_viewport :: Range -> Double -> Double,

    -- | The title string to be displayed on the axis. An
    -- empty string means no title.
    axis_title :: String,

    -- | The tick marks on the axis as pairs.
    -- The first element is the position on the axis
    -- (in viewport units) and the second element is the
    -- length of the tick in output coordinates.
    -- The tick starts on the axis, and positive number are drawn
    -- towards the plot area.
    axis_ticks :: [(Double,Double)],
    
    -- | The labels on an axis as pairs. The first element 
    -- is the position on the axis (in viewport units) and
    -- the second is the label text string.
    axis_labels :: [ (Double, String) ],

    -- | The positions on the axis (in viewport units) where
    -- we want to show grid lines.
    axis_grid :: [ Double ],

    -- | How far the labels are to be drawn from the axis.
    axis_label_gap :: Double,

    axis_title_style :: CairoFontStyle,
    axis_line_style :: CairoLineStyle,
    axis_label_style :: CairoFontStyle,
    axis_grid_style :: CairoLineStyle

}

-- | Function type to generate an optional axis given a set
-- of points to be plotted against that axis.
type AxisFn = [Double] -> Maybe Axis

-- | Function type to generate a pair of axes (either top 
-- and bottom, or left and right), given the set of points to
-- be plotted against each of them.
type AxesFn = [Double] -> [Double] -> (Maybe Axis,Maybe Axis)

data AxisT = AxisT RectEdge Axis

instance ToRenderable AxisT where
  toRenderable at = Renderable {
     minsize=minsizeAxis at,
     render=renderAxis at
  }

minsizeAxis :: AxisT -> C.Render RectSize
minsizeAxis (AxisT at a) = do
    let labels = map snd (axis_labels a)
    C.save
    setFontStyle (axis_label_style a)
    labelSizes <- mapM textSize labels
    C.restore
    let (lw,lh) = foldl maxsz (0,0) labelSizes
    let ag = axis_label_gap a
    let tsize = maximum [ max 0 (-l) | (v,l) <- axis_ticks a ]
    let sz = case at of
		     E_Top    -> (lw,max (lh + ag) tsize)
		     E_Bottom -> (lw,max (lh + ag) tsize)
		     E_Left   -> (max (lw + ag) tsize, lh)
		     E_Right  -> (max (lw + ag) tsize, lh)
    return sz

  where
    maxsz (w1,h1) (w2,h2) = (max w1 w2, max h1 h2)


-- | Calculate the amount by which the labels extend beyond
-- the ends of the axis
axisOverhang :: AxisT -> C.Render (Double,Double)
axisOverhang (AxisT at a) = do
    let labels = map snd (sort (axis_labels a))
    C.save
    setFontStyle (axis_label_style a)
    labelSizes <- mapM textSize labels
    C.restore
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

renderAxis :: AxisT -> Rect -> C.Render ()
renderAxis at@(AxisT et a) rect = do
   C.save
   setLineStyle (axis_line_style a)
   strokeLines' True [Point sx sy,Point ex ey]
   mapM_ drawTick (axis_ticks a)
   C.restore
   C.save
   setFontStyle (axis_label_style a)
   mapM_ drawLabel (axis_labels a)
   C.restore
 where
   (sx,sy,ex,ey,tp,axisPoint) = axisMapping at rect

   drawTick (value,length) = 
       let t1 = axisPoint value
	   t2 = t1 `pvadd` (vscale length tp)
       in strokeLines' True [t1,t2]

   (hta,vta,lp) = 
       let g = axis_label_gap a
       in case et of
		  E_Top    -> (HTA_Centre,VTA_Bottom,(Vector 0 (-g)))
		  E_Bottom -> (HTA_Centre,VTA_Top,(Vector 0 g))
		  E_Left   -> (HTA_Right,VTA_Centre,(Vector (-g) 0))
		  E_Right  -> (HTA_Left,VTA_Centre,(Vector g 0))

   drawLabel (value,s) = do
       drawText hta vta (axisPoint value `pvadd` lp) s

axisMapping :: AxisT -> Rect -> (Double,Double,Double,Double,Vector,Double->Point)
axisMapping (AxisT et a) rect = case et of
    E_Top    -> (x1,y2,x2,y2, (Vector 0 1),    mapx (x1,x2) y2) 
    E_Bottom -> (x1,y1,x2,y1, (Vector 0 (-1)), mapx (x1,x2) y1)
    E_Left   -> (x2,y2,x2,y1, (Vector (1) 0),  mapy (y1,y2) x2)		
    E_Right  -> (x1,y2,x1,y1, (Vector (-1) 0), mapy (y1,y2) x1)
  where
    (Rect (Point x1 y1) (Point x2 y2)) = rect

    mapx :: Range -> Double -> Double -> Point
    mapx xr y x = Point (axis_viewport a xr x) y

    mapy :: Range -> Double -> Double -> Point
    mapy (yr0,yr1) x y = Point x (axis_viewport a (yr1,yr0) y)

renderAxisGrid :: Rect -> AxisT -> C.Render ()
renderAxisGrid rect@(Rect p1 p2) at@(AxisT re a) = do
    C.save
    setLineStyle (axis_grid_style a)
    mapM_ (drawGridLine re) (axis_grid a)
    C.restore
  where
    (sx,sy,ex,ey,tp,axisPoint) = axisMapping at rect

    drawGridLine E_Top = vline
    drawGridLine E_Bottom = vline
    drawGridLine E_Left = hline
    drawGridLine E_Right = hline

    vline v = let v' = p_x (axisPoint v)
	      in strokeLines' True [Point v' (p_y p1),Point v' (p_y p2)]

    hline v = let v' = p_y (axisPoint v)
	      in strokeLines' True [Point (p_x p1) v',Point (p_x p2) v']

-- | Same as strokeLines, but with a flag that, when true will
-- adjust each point to land on a whole number. This is useful for
-- drawing known horizontal and vertical lines so that the occupy
-- exactly 

strokeLines' :: Bool -> [Point] -> C.Render ()
strokeLines' False ps = strokeLines ps
strokeLines' True  ps = strokeLines (map adjfn ps)
  where
    adjfn (Point x y)= Point (fromIntegral (round x)) (fromIntegral (round y))

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
explicitAxis :: Maybe Axis -> AxisFn
explicitAxis ma _ = ma

linearTicks r = (major, minor)
 where
  major = steps 5 r
  minor = steps 50 (fromRational (minimum major),fromRational (maximum major))

autoAxis transform (rlabelvs, rtickvs) a = Just axis
  where
    axis =  a {
        axis_viewport=newViewport,
	axis_ticks=newTicks,
	axis_grid=gridvs,
	axis_labels=newLabels
	}
    newViewport = transform (min',max')
    newTicks = [ (v,2) | v <- tickvs ] ++ [ (v,5) | v <- labelvs ] 
    newLabels = [(v,show v) | v <- labelvs]
    labelvs = map fromRational rlabelvs
    tickvs = map fromRational rtickvs
    min' = minimum labelvs
    max' = maximum labelvs

    gridvs = case (axis_grid a) of 
       [] -> []
       _  -> labelvs

-- | Generate a linear axis automatically.
-- The supplied axis is used as a template, with the viewport, ticks, labels
-- and grid set appropriately for the data displayed against that axies.
-- The resulting axis will only show a grid if the template has some grid
-- values.
autoScaledAxis :: Axis -> AxisFn
autoScaledAxis a ps0 = autoAxis vmap (linearTicks (range ps)) a
  where
    ps = filter isValidNumber ps0
    (min,max) = (minimum ps,maximum ps)
    range [] = (0,1)
    range _  | min == max = (min-0.5,min+0.5)
	     | otherwise = (min,max)

log10 :: (Floating a) => a -> a
log10 = logBase 10

frac x | 0 <= b = (a,b)
       | otherwise = (a-1,b+1)
 where
  (a,b) = properFraction x

lmap (x1,x2) r x = vmap (log x1, log x2) r (log x)

{- 
 Rules: Do no subdivide between powers of 10 until all powers of 10
          get a major ticks.
        Do not subdivide between powers of ten as [1,2,4,6,8,10] when
          5 gets a major ticks 
          (ie the major ticks need to be a subset of the minor tick)
-}
logTicks :: Range -> ([Rational],[Rational])
logTicks (low,high) = (major,minor)
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
autoScaledLogAxis :: Axis -> AxisFn
autoScaledLogAxis a ps0 = autoAxis lmap (logTicks (range ps)) a
  where
    ps = filter isValidNumber ps0
    (min, max) = (minimum ps,maximum ps)
    range [] = (3,30)
    range _  | min == max = (min/3,max*3)
	     | otherwise = (min,max)

-- | Show independent axes on each side of the layout
independentAxes :: AxisFn -> AxisFn -> AxesFn
independentAxes af1 af2 pts1 pts2 = (af1 pts1, af2 pts2)

-- | Show the same axis on both sides of the layout
linkedAxes :: AxisFn -> AxesFn
linkedAxes af pts1 pts2 = (a,a)
  where
    a = af (pts1++pts2)

-- | Show the same axis on both sides of the layout, but with labels
-- only on the primary side
linkedAxes' :: AxisFn -> AxesFn
linkedAxes' af pts1 pts2 = (a,removeLabels a)
  where
    a  = af (pts1++pts2)
    removeLabels = liftM (\a -> a{axis_title="",axis_labels = []})

----------------------------------------------------------------------

defaultAxisLineStyle = solidLine 1 0 0 0
defaultGridLineStyle = dashedLine 1 [5,5] 0.8 0.8 0.8

defaultAxis = Axis {
    axis_viewport = vmap (0,1),
    axis_title = "",
    axis_ticks = [(0,10),(1,10)],
    axis_labels = [],
    axis_grid = [0.0,0.5,1.0],
    axis_label_gap = 10,
    axis_title_style = defaultFontStyle,
    axis_line_style = defaultAxisLineStyle,
    axis_label_style = defaultFontStyle,
    axis_grid_style = defaultGridLineStyle
}

----------------------------------------------------------------------

refClockTime = toClockTime CalendarTime {
    ctYear=1970,
    ctMonth=toEnum 0,
    ctDay=1,
    ctHour=0,
    ctMin=0,
    ctSec=0,
    ctPicosec=0,
    ctTZ=0,
    ctWDay=Monday,
    ctYDay=0,
    ctTZName="",
    ctIsDST=False
    }

-- | Map a clocktime value to a plot cordinate
doubleFromClockTime :: ClockTime -> Double
doubleFromClockTime ct = fromIntegral (tdSec (diffClockTimes ct refClockTime))

-- | Map a plot cordinate to a clocktime
clockTimeFromDouble :: Double -> ClockTime
clockTimeFromDouble v = (addToClockTime tdiff refClockTime)
  where
    tdiff = TimeDiff {
       tdYear = 0,
       tdMonth = 0,
       tdDay = 0,
       tdHour = 0,
       tdMin = 0,
       tdSec = floor v,
       tdPicosec = 0
    }

-- | An axis that plots dates, with ticks and labels corresponding to
-- calendar months. The values to be plotted against this axis can
-- be created with 'doubleFromClockTime'
monthsAxis :: Axis -> AxisFn
monthsAxis a pts = Just axis
  where
    axis =  a {
        axis_viewport=newViewport,
	axis_ticks=newTicks,
	axis_labels=newLabels,
	axis_grid=newGrid
	}
    (min,max) = case pts of
		[] -> (refClockTime, nextMonthStart refClockTime)
		ps -> let min = minimum ps
			  max = maximum ps in
			  (clockTimeFromDouble min,clockTimeFromDouble max)
    min' = thisMonthStart min
    max' = nextMonthStart max

    newViewport = vmap (doubleFromClockTime min', doubleFromClockTime max')
    months = takeWhile (<=max') (iterate nextMonthStart min')
    newTicks = [ (doubleFromClockTime ct,5) | ct <- months ]
    newLabels = [ (mlabelv m1 m2, mlabelt m1) | (m1,m2) <- zip months (tail months) ]
    newGrid = case axis_grid a of 
        [] -> []
        _  -> [v | (v,_) <- newTicks]

    mlabelt m =  formatCalendarTime defaultTimeLocale "%b-%y" (toUTCTime m)
    mlabelv m1 m2 = (doubleFromClockTime m2 + doubleFromClockTime m1) / 2

thisMonthStart ct = 
    let calt = (toUTCTime ct) {
            ctDay=1,
	    ctHour=0,
	    ctMin=0,
	    ctSec=0,
	    ctPicosec=0
        } in
        toClockTime calt

nextMonthStart ct =
    let month1 = noTimeDiff{tdMonth=1} in
	addToClockTime month1 (thisMonthStart ct)
 
