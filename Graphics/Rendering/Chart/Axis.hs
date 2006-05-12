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
		   
    -- | The range in model space coordinates covered by
    -- this axis.
    axis_viewport :: Range,

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
	   t2 = t1 `padd` (pscale length tp)
       in strokeLines' True [t1,t2]

   (hta,vta,lp) = 
       let g = axis_label_gap a
       in case et of
		  E_Top    -> (HTA_Centre,VTA_Bottom,(Point 0 (-g)))
		  E_Bottom -> (HTA_Centre,VTA_Top,(Point 0 g))
		  E_Left   -> (HTA_Right,VTA_Centre,(Point (-g) 0))
		  E_Right  -> (HTA_Left,VTA_Centre,(Point g 0))

   drawLabel (value,s) = do
       drawText hta vta (axisPoint value `padd` lp) s

axisMapping (AxisT et a) rect = (sx,sy,ex,ey,tp,map)
  where
   (Rect (Point x1 y1) (Point x2 y2)) = rect

   (vs,ve) = axis_viewport a

   (sx,sy,ex,ey,tp) = case et of
       E_Top    -> (x1,y2,x2,y2, (Point 0 1)) 
       E_Bottom -> (x1,y1,x2,y1, (Point 0 (-1)))
       E_Left   -> (x2,y2,x2,y1, (Point (1) 0))		
       E_Right  -> (x1,y2,x1,y1, (Point (-1) 0))

   map v = let 
        ax = (sx + (ex-sx) * (v - vs) / (ve-vs))
        ay = (sy + (ey-sy) * (v - vs) / (ve-vs))
     in (Point ax ay)

renderAxisGrid :: AxisT -> Rect -> C.Render ()
renderAxisGrid at@(AxisT re a) rect@(Rect p1 p2) = do
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

steps:: Int -> Range -> [Double]
steps nSteps (min,max) = [ min' + i * s | i <- [0..n] ]
  where
    min' = fromIntegral (floor (min / s) ) * s
    max' = fromIntegral (ceiling (max / s) ) * s
    n = (max' - min') / s
    s = chooseStep nSteps (min,max)

chooseStep :: Int -> Range -> Double
chooseStep nsteps (min,max) = s
  where
    mult = 10 ** fromIntegral (floor ((log (max-min) - log (fromIntegral nsteps)) / log 10))
    steps = map (mult*) [0.1, 0.2, 0.25, 0.5, 1.0, 2.0, 2.5, 5.0, 10, 20, 25, 50]
    steps' =  sort [ (abs((max-min)/s - fromIntegral nsteps), s) | s <- steps ]
    s = snd (head steps')

-- | Explicitly specify an axis
explicitAxis :: Maybe Axis -> AxisFn
explicitAxis ma _ = ma

-- | Generate an axis automatically.
-- The supplied axis is used as a template, with the ticks, labels
-- and grid set appropriately for the data displayed against that axies.
autoScaledAxis :: Axis -> AxisFn
autoScaledAxis a pts = Just axis
  where
    axis =  a {
        axis_viewport=newViewport,
	axis_ticks=newTicks,
	axis_grid=labelvs,
	axis_labels=newLabels
	}
    newViewport = (min',max')
    newTicks = [ (v,2) | v <- tickvs ] ++ [ (v,5) | v <- labelvs ] 
    newLabels = [(v,show v) | v <- labelvs]
    (min,max) = case pts of
		[] -> (0,1)
		ps -> let min = minimum ps
			  max = maximum ps in
			  if min == max then (min-0.5,max+0.5)
			                else (min,max)
    labelvs = steps 5 (min,max)
    min' = minimum labelvs
    max' = maximum labelvs
    tickvs = steps 50 (min',max')

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
    removeLabels = liftM (\a -> a{axis_labels = []})

----------------------------------------------------------------------

defaultAxisLineStyle = solidLine 1 0 0 0
defaultGridLineStyle = dashedLine 1 [5,5] 0.8 0.8 0.8

defaultAxis = Axis {
    axis_viewport = (0,1),
    axis_ticks = [(0,10),(1,10)],
    axis_labels = [],
    axis_grid = [],
    axis_label_gap =10,
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

monthsAxis :: Axis -> AxisFn
monthsAxis a pts = Just axis
  where
    axis =  a {
        axis_viewport=newViewport,
	axis_ticks=newTicks,
	axis_labels=newLabels,
	axis_grid=[v | (v,_) <- newTicks]
	}
    (min,max) = case pts of
		[] -> (refClockTime, nextMonthStart refClockTime)
		ps -> let min = minimum ps
			  max = maximum ps in
			  (clockTimeFromDouble min,clockTimeFromDouble max)
    min' = thisMonthStart min
    max' = nextMonthStart max

    newViewport = (doubleFromClockTime min', doubleFromClockTime max')
    months = takeWhile (<=max') (iterate nextMonthStart min')
    newTicks = [ (doubleFromClockTime ct,5) | ct <- months ]
    newLabels = [ (mlabelv m1 m2, mlabelt m1) | (m1,m2) <- zip months (tail months) ]

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
 
