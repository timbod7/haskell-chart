module Chart where

import qualified Graphics.Rendering.Cairo as Cairo

data Point = Point {
    p_x :: Double,
    p_y :: Double
} deriving Show

pscale c (Point x y) = (Point (x*c) (y*c))
padd (Point x1 y1) (Point x2 y2) = (Point (x1+x2) (y1+y2))
psub (Point x1 y1) (Point x2 y2) = (Point (x1-x2) (y1-y2))

data Rect = Rect Point Point

newtype CairoPointStyle = CairoPointStyle (Point -> Cairo.Render ())
newtype CairoLineStyle = CairoLineStyle (Cairo.Render ())
newtype CairoFontStyle = CairoFontStyle (Cairo.Render ())

data Label = Label {
    label_text :: String,
    label_style :: CairoFontStyle
}

class Renderable a where
   minsize  :: a -> Cairo.Render (Double,Double)
   render   :: a -> Rect -> Cairo.Render ()

----------------------------------------------------------------------

data AxisType = AT_Top | AT_Bottom | AT_Left | AT_Right

data Axis =  Axis {
    axis_viewport :: (Double,Double),
    axis_line_style :: CairoLineStyle,
    axis_label_style :: CairoFontStyle,

    axis_ticks :: [(Double,Double)],
    axis_labels :: [ (Double, String) ],
    axis_label_gap :: Double 
}

data AxisT = AxisT AxisType Axis

instance Renderable AxisT where
   minsize = minsizeAxis
   render  = renderAxis 

minsizeAxis :: AxisT -> Cairo.Render (Double,Double)
minsizeAxis (AxisT at a) = do
    let labels = map snd (axis_labels a)
    Cairo.save
    setFontStyle
    labelSizes <- mapM labelSize labels
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

    labelSize s = do
        te <- Cairo.textExtents s
	return (Cairo.textExtentsWidth te, Cairo.textExtentsHeight te)

    (CairoFontStyle setFontStyle) = axis_label_style a

renderAxis :: AxisT -> Rect -> Cairo.Render ()
renderAxis (AxisT at a) rect = do
   Cairo.save
   setLineStyle
   strokeLine (Point sx sy) (Point ex ey)
   mapM_ drawTick (axis_ticks a)
   Cairo.restore
   Cairo.save
   setFontStyle
   mapM_ drawLabel (axis_labels a)
   Cairo.restore
 where
   (CairoLineStyle setLineStyle) = axis_line_style a
   (CairoFontStyle setFontStyle) = axis_label_style a

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

   loffset te =
       let w = Cairo.textExtentsWidth te
	   h = Cairo.textExtentsHeight te
	   g = axis_label_gap a
       in case at of
		  AT_Top    -> (-w/2,-g)
		  AT_Bottom -> (-w/2,h+g)
		  AT_Left   -> (-w-g,h/2)
		  AT_Right  -> (g,h/2)

   drawLabel (value,s) = do
       te <- Cairo.textExtents s
       let (lx,ly) = loffset te
       let (Point ax ay) = axisPoint value
       Cairo.moveTo (ax+lx) (ay+ly)
       Cairo.showText s

----------------------------------------------------------------------
data Plot = PPoints PlotPoints
	  | PLines  PlotLines

data PlotPoints = PlotPoints {
    plot_points_style :: CairoPointStyle,
    plot_points_values :: [Point]
}

data PlotLines = PlotLines {
    plot_lines_style :: CairoLineStyle,
    plot_lines_values :: [Point]
}

renderPlotLines :: PlotLines -> Rect -> Rect -> Cairo.Render ()
renderPlotLines p r v = do
    Cairo.save
    setLineStyle
    drawLines (plot_lines_values p)
    Cairo.restore
  where
    drawLines (p:ps) = do
	moveTo (pmap r v p)
	mapM_ (\p -> lineTo (pmap r v p)) ps
	Cairo.stroke

    (CairoLineStyle setLineStyle) = plot_lines_style p


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

filledCircles :: Double -> Double -> Double -> Double -> CairoPointStyle
filledCircles radius r g b = CairoPointStyle rf
  where
    rf (Point x y) = do
        Cairo.save
	Cairo.setSourceRGB r g b
        Cairo.newPath
	Cairo.arc x y radius 0 360
	Cairo.fill
	Cairo.restore

solidLine :: Double -> Double -> Double -> Double -> CairoLineStyle
solidLine w r g b = CairoLineStyle (do
    Cairo.setLineWidth w
    Cairo.setSourceRGB r g b
    )

----------------------------------------------------------------------

data HAxis = HA_Top | HA_Bottom
data VAxis = VA_Left | VA_Right

data Layout1 = Layout1 {
    layout1_bottom_axis :: Maybe Axis,
    layout1_left_axis :: Maybe Axis,
    layout1_top_axis :: Maybe Axis,
    layout1_right_axis :: Maybe Axis,
    layout1_margin :: Double,
    layout1_plots :: [(HAxis, VAxis, Plot)]
}

instance Renderable Layout1 where
    render = renderLayout1
    minsize  = minsizeLayout1

renderLayout1 :: Layout1 -> Rect -> Cairo.Render ()
renderLayout1 l (Rect p0 p5) = do
    (w1,h1,w2,h2) <- axisSizes l

    let mp = let s = (layout1_margin l) in (Point s s)
    let p1 = p0 `padd` mp
    let p2 = p1 `padd` (Point w1 h1)
    let p4  = p5 `psub` mp
    let p3  = p4 `psub` (Point w2 h2)

    rMAxis AT_Top (layout1_top_axis l) (mkrect p2 p1 p3 p2)
    rMAxis AT_Bottom (layout1_bottom_axis l) (mkrect p2 p3 p3 p4)
    rMAxis AT_Left (layout1_left_axis l) (mkrect p1 p2 p2 p3)
    rMAxis AT_Right (layout1_right_axis l) (mkrect p3 p2 p4 p3)
    Cairo.save
    setClipRegion p2 p3 
    mapM_ (rPlot (Rect p2 p3)) (layout1_plots l)
    Cairo.restore
  where
    rMAxis at (Just a) rect = render (AxisT at a) rect
    rMAxis _ Nothing  _ = return ()

    hvport HA_Bottom = layout1_bottom_axis
    hvport HA_Top = layout1_top_axis
    vvport VA_Left = layout1_left_axis
    vvport VA_Right = layout1_right_axis

    rPlot :: Rect -> (HAxis,VAxis,Plot) -> Cairo.Render ()
    rPlot rect (ha,va,p) = 
        let avport (Just a) = axis_viewport a
	    avport Nothing  = (0,1)
            (x1,x2) = avport (hvport ha l)
	    (y1,y2) = avport (vvport va l)
	in renderPlot p rect (Rect (Point x1 y1) (Point x2 y2))

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
    w1 <- asize fst AT_Left   (layout1_left_axis l)
    h1 <- asize snd AT_Top (layout1_top_axis l)
    w2 <- asize fst AT_Right  (layout1_right_axis l)
    h2 <- asize snd AT_Bottom    (layout1_bottom_axis l)
    return (w1,h1,w2,h2)
  where
    asize xyfn at ma = case ma of
	  Nothing -> return 0
	  Just a  -> do
	      sz <- minsize (AxisT at a)
	      return (xyfn sz)

emptyLayout1 = Layout1 {
    layout1_bottom_axis = Nothing,
    layout1_top_axis = Nothing,
    layout1_left_axis = Nothing,
    layout1_right_axis = Nothing,
    layout1_margin = 10,
    layout1_plots = []
}

----------------------------------------------------------------------

moveTo, lineTo :: Point -> Cairo.Render ()
moveTo (Point px py) = Cairo.moveTo px py
lineTo (Point px py) = Cairo.lineTo px py

strokeLine p1 p2 = do
   Cairo.newPath
   moveTo p1
   lineTo p2
   Cairo.stroke
