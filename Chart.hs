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
    autoScaleLinkedAxes,
    explicitLinkedAxes,
    renderableToPNGFile,
    setupRender
) where

import qualified Graphics.Rendering.Cairo as Cairo

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
newtype CairoPointStyle = CairoPointStyle (Point -> Cairo.Render ())

-- | Abstract data type for the style of a line
newtype CairoLineStyle = CairoLineStyle (Cairo.Render ())

-- | Abstract data type for a fill style
newtype CairoFillStyle = CairoFillStyle (Cairo.Render ())

-- | Abstract data type for a font
newtype CairoFontStyle = CairoFontStyle (Cairo.Render ())

-- | A Renderable has a minimum size, and a Cairo action for
-- drawing it within a specified rectangle.
class Renderable a where
   minsize  :: a -> Cairo.Render (Double,Double)
   render   :: a -> Rect -> Cairo.Render ()

----------------------------------------------------------------------

-- | The concrete data type for an axis
data Axis =  Axis {
		   
    -- | The range in "plot coordinates" covered by
    -- this axis.
    axis_viewport :: (Double,Double),

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

minsizeAxis :: AxisT -> Cairo.Render (Double,Double)
minsizeAxis (AxisT at a) = do
    let labels = map snd (axis_labels a)
    Cairo.save
    setFontStyle (axis_label_style a)
    labelSizes <- mapM textSize labels
    Cairo.restore
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

renderAxis :: AxisT -> Rect -> Cairo.Render ()
renderAxis (AxisT at a) rect = do
   Cairo.save
   setLineStyle (axis_line_style a)
   strokeLine (Point sx sy) (Point ex ey)
   mapM_ drawTick (axis_ticks a)
   Cairo.restore
   Cairo.save
   setFontStyle (axis_label_style a)
   mapM_ drawLabel (axis_labels a)
   Cairo.restore
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

moveTo, lineTo :: Point -> Cairo.Render ()
moveTo (Point px py) = Cairo.moveTo px py
lineTo (Point px py) = Cairo.lineTo px py

strokeLine p1 p2 = do
   Cairo.newPath
   moveTo p1
   lineTo p2
   Cairo.stroke

setFontStyle (CairoFontStyle s) = s
setLineStyle (CairoLineStyle s) = s
setFillStyle (CairoFillStyle s) = s

textSize :: String -> Cairo.Render (Double,Double)
textSize s = do
    te <- Cairo.textExtents s
    return (Cairo.textExtentsWidth te, Cairo.textExtentsHeight te)

data HTextAnchor = HTA_Left | HTA_Centre | HTA_Right
data VTextAnchor = VTA_Top | VTA_Centre | VTA_Bottom

-- | Function to draw a textual label anchored by one of it's corners
-- or edges.
drawText :: HTextAnchor -> VTextAnchor -> Point -> String -> Cairo.Render ()
drawText hta vta (Point x y) s = do
    te <- Cairo.textExtents s
    let lx = xadj hta (Cairo.textExtentsWidth te)
    let ly = yadj vta (Cairo.textExtentsHeight te)
    Cairo.moveTo (x+lx) (y+ly)
    Cairo.showText s
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

renderPlotLines :: PlotLines -> Rect -> Rect -> Cairo.Render ()
renderPlotLines p r v = do
    Cairo.save
    setLineStyle (plot_lines_style p)
    mapM_ drawLines (plot_lines_values p)
    Cairo.restore
  where
    drawLines (p:ps) = do
	moveTo (pmap r v p)
	mapM_ (\p -> lineTo (pmap r v p)) ps
	Cairo.stroke

pmap (Rect pr1 pr2) (Rect pv1 pv2) (Point x y) =
    Point (p_x pr1 + (x - p_x pv1) * xs)
          (p_y pr1 + (y - p_y pv1) * ys)
  where
    xs = (p_x pr2 - p_x pr1) / (p_x pv2 - p_x pv1)
    ys = (p_y pr2 - p_y pr1) / (p_y pv2 - p_y pv1)
    			
renderPlotPoints :: PlotPoints -> Rect -> Rect -> Cairo.Render ()
renderPlotPoints p r v = do
    Cairo.save
    mapM_ (drawPoint.(pmap r v)) (plot_points_values p)
    Cairo.restore
  where
    (CairoPointStyle drawPoint) = (plot_points_style p)

renderPlot :: Plot -> Rect -> Rect -> Cairo.Render ()
renderPlot (PPoints p) r v = renderPlotPoints p r v
renderPlot (PLines p) r v = renderPlotLines p r v

allPoints:: Plot -> [Point]
allPoints (PPoints ps) = plot_points_values ps
allPoints (PLines  ps) = concat (plot_lines_values ps)

filledCircles :: Double -> Double -> Double -> Double -> CairoPointStyle
filledCircles radius r g b = CairoPointStyle rf
  where
    rf (Point x y) = do
	Cairo.setSourceRGB r g b
        Cairo.newPath
	Cairo.arc x y radius 0 360
	Cairo.fill

solidLine :: Double -> Double -> Double -> Double -> CairoLineStyle
solidLine w r g b = CairoLineStyle (do
    Cairo.setLineWidth w
    Cairo.setSourceRGB r g b
    )

fontStyle :: String -> Double -> Cairo.FontSlant ->
	     Cairo.FontWeight -> CairoFontStyle
fontStyle name size slant weight = CairoFontStyle fn
  where
    fn = do
	 Cairo.selectFontFace name slant weight
	 Cairo.setFontSize size

solidFillStyle :: Double -> Double -> Double -> CairoFillStyle
solidFillStyle r g b = CairoFillStyle fn
   where fn = Cairo.setSourceRGB r g b

----------------------------------------------------------------------

data HAxis = HA_Top | HA_Bottom
data VAxis = VA_Left | VA_Right

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

renderLayout1 :: Layout1 -> Rect -> Cairo.Render ()
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
    Cairo.save
    setClipRegion p0 p5 
    setFillStyle (layout1_background l)
    Cairo.paint
    Cairo.restore

    -- render the axes
    rMAxis AT_Top tAxis (mkrect p2 p1 p3 p2)
    rMAxis AT_Bottom  bAxis(mkrect p2 p3 p3 p4)
    rMAxis AT_Left lAxis (mkrect p1 p2 p2 p3)
    rMAxis AT_Right rAxis (mkrect p3 p2 p4 p3)

    -- render the plots
    Cairo.save
    setClipRegion p2 p3 
    mapM_ (rPlot (Rect p2 p3)) (layout1_plots l)
    Cairo.restore

    -- render the title
    rTitle titlep

  where
    titleSize = do
       Cairo.save
       setFontStyle (layout1_title_style l)
       sz <- textSize (layout1_title l)
       Cairo.restore
       return sz

    rTitle titlep = do
        Cairo.save
	setFontStyle (layout1_title_style l)
	drawText HTA_Centre VTA_Top titlep (layout1_title l)
	Cairo.restore

    (xvals0,xvals1,yvals0,yvals1) = allPlottedValues (layout1_plots l)
    (tAxis,bAxis) = layout1_horizontal_axes l xvals0 xvals1
    (lAxis,rAxis) = layout1_vertical_axes l yvals0 yvals1

    rMAxis at (Just a) rect = render (AxisT at a) rect
    rMAxis _ Nothing  _ = return ()

    rPlot :: Rect -> (HAxis,VAxis,Plot) -> Cairo.Render ()
    rPlot rect (ha,va,p) = 
        let mxaxis = case ha of HA_Bottom -> bAxis
				HA_Top    -> tAxis
	    myaxis = case va of VA_Left   -> lAxis
				VA_Right  -> rAxis
        in rPlot1 rect mxaxis myaxis p
	      
    rPlot1 :: Rect -> Maybe Axis -> Maybe Axis -> Plot -> Cairo.Render ()
    rPlot1 rect (Just xaxis) (Just yaxis) p = 
	let (x1,x2) = axis_viewport xaxis
	    (y1,y2) = axis_viewport yaxis
	in renderPlot p rect (Rect (Point x1 y1) (Point x2 y2))
    rPlot1 _ _ _ _ = return ()

    mkrect (Point x1 y1) (Point x2 y2) (Point x3 y3) (Point x4 y4) =
	Rect (Point x1 y2) (Point x3 y4)

    setClipRegion p2 p3 = do    
        Cairo.moveTo (p_x p2) (p_y p2)
	Cairo.lineTo (p_x p2) (p_y p3)
        Cairo.lineTo (p_x p3) (p_y p3)
        Cairo.lineTo (p_x p3) (p_y p2)
        Cairo.lineTo (p_x p2) (p_y p2)
        Cairo.clip

minsizeLayout1 l = do
  let m = layout1_margin l
  (w1,h1,w2,h2) <- axisSizes l
  return (2*m+w1+w2,2*m+h1+h2)

axisSizes l = do
    w1 <- asize fst AT_Left   tAxis
    h1 <- asize snd AT_Top bAxis
    w2 <- asize fst AT_Right  lAxis
    h2 <- asize snd AT_Bottom    rAxis
    return (w1,h1,w2,h2)
  where
    (xvals0,xvals1,yvals0,yvals1) = allPlottedValues (layout1_plots l)
    (tAxis,bAxis) = layout1_horizontal_axes l xvals0 xvals1
    (lAxis,rAxis) = layout1_vertical_axes l yvals0 yvals1

    asize xyfn at ma = case ma of
	  Nothing -> return 0
	  Just a  -> do
	      sz <- minsize (AxisT at a)
	      return (xyfn sz)

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
    layout1_title_style = fontStyle "sans" 15 Cairo.FontSlantNormal Cairo.FontWeightBold,
    layout1_horizontal_axes = autoScaleLinkedAxes defaultAxis,
    layout1_vertical_axes = autoScaleLinkedAxes defaultAxis,
    layout1_margin = 10,
    layout1_plots = []
}

explicitLinkedAxes :: Axis -> AxesFn
explicitLinkedAxes a _ _ = (Just a, Just a)

autoScaleLinkedAxes :: Axis -> AxesFn
autoScaleLinkedAxes a pts1 pts2 = (Just axis, Just axis)
  where
    axis =  a {
        axis_viewport=newViewport,
	axis_ticks=newTicks,
	axis_labels=newLabels
	}
    newViewport = (min,max)
    newTicks = [(min,10),(max,10)]
    newLabels = [(min,show min), (max,show max)]
    (min,max) = case pts1++pts2 of
		[] -> (0,1)
		ps -> let min = minimum ps
			  max = maximum ps in
			  if min == max then (min-0.5,max+0.5)
			                else (min,max)
    vfn _ = axis_viewport axis


renderableToPNGFile :: Renderable a => a -> Int -> Int -> FilePath -> IO ()
renderableToPNGFile chart width height path = 
    Cairo.withImageSurface Cairo.FormatARGB32 width height $ \result -> do
    Cairo.renderWith result $ rfn
    Cairo.surfaceWriteToPNG result path
  where
    rfn = do
        setupRender
	render chart rect

    rect = Rect (Point 0 0) (Point (fromIntegral width) (fromIntegral height))


setupRender :: Cairo.Render ()
setupRender = do
    -- move to centre of pixels so that stroke width of 1 is
    -- exactly one pixel 
    Cairo.translate 0.5 0.5
