module Chart(
    Point(..),
    Rect(..),
    Axis(..),
    Plot(..),
    PlotPoints(..),
    PlotLines(..),
    Layout1(..),
    Renderable(..),
    HAxis(..),
    VAxis(..),
    defaultAxisLineStyle, 
    defaultPlotLineStyle,
    defaultAxis, 
    defaultPlotPoints,
    defaultPlotLines,
    defaultLayout1,
    filledCircles,
    solidLine,
    independentAxes,
    linkedAxes,
    linkedAxes',
    explicitAxis,
    autoScaledAxis,
    monthsAxis,
    renderableToPNGFile,
    setupRender,
    doubleFromClockTime,
    clockTimeFromDouble,
) where

import qualified Graphics.Rendering.Cairo as C
import Data.List
import Control.Monad
import System.Time
import System.Locale

-- | A point in two dimensions
data Point = Point {
    p_x :: Double,
    p_y :: Double
} deriving Show

-- | scale a point by a constant
pscale :: Double -> Point -> Point
pscale c (Point x y) = (Point (x*c) (y*c))

-- | add two points
padd :: Point -> Point -> Point
padd (Point x1 y1) (Point x2 y2) = (Point (x1+x2) (y1+y2))

-- | subtract two points
psub :: Point -> Point -> Point
psub (Point x1 y1) (Point x2 y2) = (Point (x1-x2) (y1-y2))

-- | A rectangle is defined by two points
data Rect = Rect Point Point

-- | Abstract data type for the style of a plotted point
newtype CairoPointStyle = CairoPointStyle (Point -> C.Render ())

-- | Abstract data type for the style of a line
newtype CairoLineStyle = CairoLineStyle (C.Render ())

-- | Abstract data type for a fill style
newtype CairoFillStyle = CairoFillStyle (C.Render ())

-- | Abstract data type for a font
newtype CairoFontStyle = CairoFontStyle (C.Render ())

type Range = (Double,Double)
type RectSize = (Double,Double)

-- | A Renderable has a minimum size, and a Cairo action for
-- drawing it within a specified rectangle.
class Renderable a where
   minsize  :: a -> C.Render RectSize
   render   :: a -> Rect -> C.Render ()


----------------------------------------------------------------------

-- | The concrete data type for an axis
data Axis =  Axis {
		   
    -- | The range in "plot coordinates" covered by
    -- this axis.
    axis_viewport :: Range,

    axis_line_style :: CairoLineStyle,
    axis_label_style :: CairoFontStyle,

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

    -- | How far the labels are to be drawn from the axis.
    axis_label_gap :: Double 
}

-- | An axis has to live on one side of the plotting area
data AxisType = AT_Top | AT_Bottom | AT_Left | AT_Right

data AxisT = AxisT AxisType Axis

instance Renderable AxisT where
   minsize = minsizeAxis
   render  = renderAxis

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
		     AT_Top    -> (lw,max (lh + ag) tsize)
		     AT_Bottom -> (lw,max (lh + ag) tsize)
		     AT_Left   -> (max (lw + ag) tsize, lh)
		     AT_Right  -> (max (lw + ag) tsize, lh)
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
		       AT_Top -> ohangh
		       AT_Bottom -> ohangh
		       AT_Left -> ohangv
		       AT_Right -> ohangh

renderAxis :: AxisT -> Rect -> C.Render ()
renderAxis (AxisT at a) rect = do
   C.save
   setLineStyle (axis_line_style a)
   strokeLine (Point sx sy) (Point ex ey)
   mapM_ drawTick (axis_ticks a)
   C.restore
   C.save
   setFontStyle (axis_label_style a)
   mapM_ drawLabel (axis_labels a)
   C.restore
 where
   (Rect (Point x1 y1) (Point x2 y2)) = rect

   (vs,ve) = axis_viewport a

   (sx,sy,ex,ey,tp) = case at of
       AT_Top    -> (x1,y2,x2,y2, (Point 0 1)) 
       AT_Bottom -> (x1,y1,x2,y1, (Point 0 (-1)))
       AT_Left   -> (x2,y2,x2,y1, (Point (1) 0))		
       AT_Right  -> (x1,y2,x1,y1, (Point (-1) 0))

   axisPoint value = 
       let ax = (sx + (ex-sx) * (value - vs) / (ve-vs))
	   ay = (sy + (ey-sy) * (value - vs) / (ve-vs))
       in (Point ax ay)

   drawTick (value,length) = 
       let t1 = axisPoint value
	   t2 = t1 `padd` (pscale length tp)
       in strokeLine t1 t2

   (hta,vta,lp) = 
       let g = axis_label_gap a
       in case at of
		  AT_Top    -> (HTA_Centre,VTA_Bottom,(Point 0 (-g)))
		  AT_Bottom -> (HTA_Centre,VTA_Top,(Point 0 g))
		  AT_Left   -> (HTA_Right,VTA_Centre,(Point (-g) 0))
		  AT_Right  -> (HTA_Left,VTA_Centre,(Point g 0))

   drawLabel (value,s) = do
       drawText hta vta (axisPoint value `padd` lp) s

----------------------------------------------------------------------
-- Assorted helper functions in Cairo Usage

moveTo, lineTo :: Point -> C.Render ()
moveTo (Point px py) = C.moveTo px py
lineTo (Point px py) = C.lineTo px py

strokeLine p1 p2 = do
   C.newPath
   moveTo p1
   lineTo p2
   C.stroke

setFontStyle (CairoFontStyle s) = s
setLineStyle (CairoLineStyle s) = s
setFillStyle (CairoFillStyle s) = s

textSize :: String -> C.Render RectSize
textSize s = do
    te <- C.textExtents s
    return (C.textExtentsWidth te, C.textExtentsHeight te)

data HTextAnchor = HTA_Left | HTA_Centre | HTA_Right
data VTextAnchor = VTA_Top | VTA_Centre | VTA_Bottom

-- | Function to draw a textual label anchored by one of it's corners
-- or edges.
drawText :: HTextAnchor -> VTextAnchor -> Point -> String -> C.Render ()
drawText hta vta (Point x y) s = do
    te <- C.textExtents s
    let lx = xadj hta (C.textExtentsWidth te)
    let ly = yadj vta (C.textExtentsHeight te)
    C.moveTo (x+lx) (y+ly)
    C.showText s
  where
    xadj HTA_Left   w = 0
    xadj HTA_Centre w = (-w/2)
    xadj HTA_Right  w = (-w)
    yadj VTA_Top    h = h
    yadj VTA_Centre h = h/2
    yadj VTA_Bottom h = 0

----------------------------------------------------------------------
data Plot = PPoints PlotPoints
	  | PLines  PlotLines

-- | Value defining a series of datapoints, and a style in
-- which to render them
data PlotPoints = PlotPoints {
    plot_points_style :: CairoPointStyle,
    plot_points_values :: [Point]
}

-- | Value defining a series of (possibly disjointed) lines,
-- and a style in which to render them
data PlotLines = PlotLines {
    plot_lines_style :: CairoLineStyle,
    plot_lines_values :: [[Point]]
}

renderPlotLines :: PlotLines -> Rect -> Rect -> C.Render ()
renderPlotLines p r v = do
    C.save
    setLineStyle (plot_lines_style p)
    mapM_ drawLines (plot_lines_values p)
    C.restore
  where
    drawLines (p:ps) = do
	moveTo (pmap r v p)
	mapM_ (\p -> lineTo (pmap r v p)) ps
	C.stroke

pmap (Rect pr1 pr2) (Rect pv1 pv2) (Point x y) =
    Point (p_x pr1 + (x - p_x pv1) * xs)
          (p_y pr2 - (y - p_y pv1) * ys)
  where
    xs = (p_x pr2 - p_x pr1) / (p_x pv2 - p_x pv1)
    ys = (p_y pr2 - p_y pr1) / (p_y pv2 - p_y pv1)
    			
renderPlotPoints :: PlotPoints -> Rect -> Rect -> C.Render ()
renderPlotPoints p r v = do
    C.save
    mapM_ (drawPoint.(pmap r v)) (plot_points_values p)
    C.restore
  where
    (CairoPointStyle drawPoint) = (plot_points_style p)

renderPlot :: Plot -> Rect -> Rect -> C.Render ()
renderPlot (PPoints p) r v = renderPlotPoints p r v
renderPlot (PLines p) r v = renderPlotLines p r v

allPoints:: Plot -> [Point]
allPoints (PPoints ps) = plot_points_values ps
allPoints (PLines  ps) = concat (plot_lines_values ps)

filledCircles :: Double -> Double -> Double -> Double -> CairoPointStyle
filledCircles radius r g b = CairoPointStyle rf
  where
    rf (Point x y) = do
	C.setSourceRGB r g b
        C.newPath
	C.arc x y radius 0 360
	C.fill

solidLine :: Double -> Double -> Double -> Double -> CairoLineStyle
solidLine w r g b = CairoLineStyle (do
    C.setLineWidth w
    C.setSourceRGB r g b
    )

fontStyle :: String -> Double -> C.FontSlant ->
	     C.FontWeight -> CairoFontStyle
fontStyle name size slant weight = CairoFontStyle fn
  where
    fn = do
	 C.selectFontFace name slant weight
	 C.setFontSize size

solidFillStyle :: Double -> Double -> Double -> CairoFillStyle
solidFillStyle r g b = CairoFillStyle fn
   where fn = C.setSourceRGB r g b

----------------------------------------------------------------------

data HAxis = HA_Top | HA_Bottom
data VAxis = VA_Left | VA_Right

type AxisFn = [Double] -> Maybe Axis
type AxesFn = [Double] -> [Double] -> (Maybe Axis,Maybe Axis)

-- | A Layout1 value is a single plot area, with optional axes on
-- each of the 4 sides, and an optional label at the top.
data Layout1 = Layout1 {
    layout1_background :: CairoFillStyle,
    layout1_title :: String,
    layout1_title_style :: CairoFontStyle,
    layout1_horizontal_axes :: AxesFn,
    layout1_vertical_axes :: AxesFn,
    layout1_margin :: Double,
    layout1_plots :: [(HAxis,VAxis,Plot)]
}

instance Renderable Layout1 where
    render = renderLayout1
    minsize  = minsizeLayout1

renderLayout1 :: Layout1 -> Rect -> C.Render ()
renderLayout1 l (Rect p0 p5) = do
    (w0,h0) <- titleSize 

    let margin  = (layout1_margin l)
    let mp = (Point margin margin)

    let ptt = if w0 == 0.0 then p0 else p0 `padd` (Point 0 margin)
    let ptb = if w0 == 0.0 then p0 else ptt `padd` (Point 0 h0)

    (w1,h1,w2,h2) <- axisSizes l

    let p1 = ptb `padd` mp
    let p2 = p1 `padd` (Point w1 h1)
    let p4  = p5 `psub` mp
    let p3  = p4 `psub` (Point w2 h2)
    let titlep = Point ((p_x p0 + p_x p5)/ 2) (p_y ptt)

    -- render the background
    C.save
    setClipRegion p0 p5 
    setFillStyle (layout1_background l)
    C.paint
    C.restore

    -- render the axes
    rMAxis tAxis (mkrect p2 p1 p3 p2)
    rMAxis bAxis (mkrect p2 p3 p3 p4)
    rMAxis lAxis (mkrect p1 p2 p2 p3)
    rMAxis rAxis (mkrect p3 p2 p4 p3)

    -- render the plots
    C.save
    setClipRegion p2 p3 
    mapM_ (rPlot (Rect p2 p3)) (layout1_plots l)
    C.restore

    -- render the title
    rTitle titlep

  where
    titleSize = do
       C.save
       setFontStyle (layout1_title_style l)
       sz <- textSize (layout1_title l)
       C.restore
       return sz

    rTitle titlep = do
        C.save
	setFontStyle (layout1_title_style l)
	drawText HTA_Centre VTA_Top titlep (layout1_title l)
	C.restore

    (bAxis,lAxis,tAxis,rAxis) = getAxes l

    rMAxis (Just at) rect = render at rect
    rMAxis Nothing  _ = return ()

    rPlot :: Rect -> (HAxis,VAxis,Plot) -> C.Render ()
    rPlot rect (ha,va,p) = 
        let mxaxis = case ha of HA_Bottom -> bAxis
				HA_Top    -> tAxis
	    myaxis = case va of VA_Left   -> lAxis
				VA_Right  -> rAxis
        in rPlot1 rect mxaxis myaxis p
	      
    rPlot1 :: Rect -> Maybe AxisT -> Maybe AxisT -> Plot -> C.Render ()
    rPlot1 rect (Just (AxisT _ xaxis)) (Just (AxisT _ yaxis)) p = 
	let (x1,x2) = axis_viewport xaxis
	    (y1,y2) = axis_viewport yaxis
	in renderPlot p rect (Rect (Point x1 y1) (Point x2 y2))
    rPlot1 _ _ _ _ = return ()

    mkrect (Point x1 y1) (Point x2 y2) (Point x3 y3) (Point x4 y4) =
	Rect (Point x1 y2) (Point x3 y4)

    setClipRegion p2 p3 = do    
        C.moveTo (p_x p2) (p_y p2)
	C.lineTo (p_x p2) (p_y p3)
        C.lineTo (p_x p3) (p_y p3)
        C.lineTo (p_x p3) (p_y p2)
        C.lineTo (p_x p2) (p_y p2)
        C.clip

minsizeLayout1 l = do
  let m = layout1_margin l
  (w1,h1,w2,h2) <- axisSizes l
  return (2*m+w1+w2,2*m+h1+h2)

axisSizes l = do
    w1a <- asize fst lAxis
    h1a <- asize snd tAxis
    w2a <- asize fst rAxis
    h2a <- asize snd bAxis
    (h1b,h2b) <- aohang lAxis
    (w1b,w2b) <- aohang tAxis
    (h1c,h2c) <- aohang rAxis
    (w1c,w2c) <- aohang bAxis

    return (maximum [w1a,w1b,w1c],
	    maximum [h1a,h1b,h1c],
	    maximum [w2a,w2b,w2c],
	    maximum [h2a,h2b,h2c] )
  where
    (bAxis,lAxis,tAxis,rAxis) = getAxes l

    asize xyfn Nothing = return 0
    asize xyfn (Just at) = do
        sz <- minsize at
	return (xyfn sz)

    aohang Nothing = return (0,0)
    aohang (Just a) = axisOverhang a


getAxes :: Layout1 -> (Maybe AxisT, Maybe AxisT, Maybe AxisT, Maybe AxisT)
getAxes l = (mk AT_Bottom bAxis, mk AT_Left lAxis,
	     mk AT_Top tAxis, mk AT_Right rAxis)
  where 
    (xvals0,xvals1,yvals0,yvals1) = allPlottedValues (layout1_plots l)
    (bAxis,tAxis) = layout1_horizontal_axes l xvals0 xvals1
    (lAxis,rAxis) = layout1_vertical_axes l yvals0 yvals1
    mk _ Nothing = Nothing
    mk at (Just a) = Just (AxisT at a)


allPlottedValues :: [(HAxis,VAxis,Plot)] -> ( [Double], [Double], [Double], [Double] )
allPlottedValues plots = (xvals0,xvals1,yvals0,yvals1)
  where
    pts = concat [ [ (ha,va,pt)| pt <- allPoints p] | (ha,va,p) <- plots ]
    xvals0 = [ (p_x pt) | (HA_Bottom,_,pt) <- pts  ]
    xvals1 = [ (p_x pt) | (HA_Top,_,pt) <- pts  ]
    yvals0 = [ (p_y pt) | (_,VA_Left,pt) <- pts  ]
    yvals1 = [ (p_y pt) | (_,VA_Right,pt) <- pts  ]

----------------------------------------------------------------------
-- Assorted default data values intended to be used as prototypes.

defaultPointStyle = filledCircles 1 1 1 1
defaultFontStyle = CairoFontStyle (return ())

defaultAxisLineStyle = solidLine 1 0 0 0
defaultPlotLineStyle = solidLine 1 0 0 1

defaultAxis = Axis {
    axis_viewport = (0,1),
    axis_line_style = defaultAxisLineStyle,
    axis_label_style = defaultFontStyle,
    axis_ticks = [(0,10),(1,10)],
    axis_labels = [],
    axis_label_gap =10
}

defaultPlotPoints = PlotPoints {
    plot_points_style =defaultPointStyle,
    plot_points_values = []
}

defaultPlotLines = PlotLines {
    plot_lines_style = defaultPlotLineStyle,
    plot_lines_values = []
}

defaultLayout1 = Layout1 {
    layout1_background = solidFillStyle 1 1 1,
    layout1_title = "",
    layout1_title_style = fontStyle "sans" 15 C.FontSlantNormal C.FontWeightBold,
    layout1_horizontal_axes = linkedAxes (autoScaledAxis defaultAxis),
    layout1_vertical_axes = linkedAxes (autoScaledAxis defaultAxis),
    layout1_margin = 10,
    layout1_plots = []
}

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

explicitAxis :: Maybe Axis -> AxisFn
explicitAxis ma _ = ma

-- | Calculate an axis automatically based upon the data displayed,
autoScaledAxis :: Axis -> AxisFn
autoScaledAxis a pts = Just axis
  where
    axis =  a {
        axis_viewport=newViewport,
	axis_ticks=newTicks,
	axis_labels=newLabels
	}
    newViewport = (min',max')
    newTicks = [ (v,2) | v <- tickvs ] ++ [ (v,10) | v <- labelvs ] 
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

doubleFromClockTime :: ClockTime -> Double
doubleFromClockTime ct = fromIntegral (tdSec (diffClockTimes ct refClockTime))

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
	axis_labels=newLabels
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
    newTicks = [ (doubleFromClockTime ct,10) | ct <- months ]
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
 
----------------------------------------------------------------------

renderableToPNGFile :: Renderable a => a -> Int -> Int -> FilePath -> IO ()
renderableToPNGFile chart width height path = 
    C.withImageSurface C.FormatARGB32 width height $ \result -> do
    C.renderWith result $ rfn
    C.surfaceWriteToPNG result path
  where
    rfn = do
        setupRender
	render chart rect

    rect = Rect (Point 0 0) (Point (fromIntegral width) (fromIntegral height))


setupRender :: C.Render ()
setupRender = do
    -- move to centre of pixels so that stroke width of 1 is
    -- exactly one pixel 
    C.translate 0.5 0.5
