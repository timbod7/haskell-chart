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

newtype CairoPointStyle = CairoPointStyle (Cairo.Render ())
newtype CairoLineStyle = CairoLineStyle (Cairo.Render ())
newtype CairoFontStyle = CairoFontStyle (Cairo.Render ())

data Label = Label {
    label_text :: String,
    label_style :: CairoFontStyle
}

data Plot = Plot {
    plot_pointstyle :: Maybe CairoPointStyle,
    plot_linestyle :: Maybe CairoLineStyle,
    plot_values :: [Point],
    plot_viewport :: Rect
}
    
class Renderable a where
   minsize  :: a -> Cairo.Render (Double,Double)
   render   :: a -> Rect -> Cairo.Render ()

----------------------------------------------------------------------

data AxisType = AT_Top | AT_Bottom | AT_Left | AT_Right

data Axis =  Axis {
    axis_viewport :: Rect,
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
   setFontStyle
   mapM_ drawLabel (axis_labels a)
   Cairo.restore
 where
   (CairoLineStyle setLineStyle) = axis_line_style a
   (CairoFontStyle setFontStyle) = axis_label_style a

   (Rect (Point x1 y1) (Point x2 y2)) = rect

   (Rect (Point vx1 vy1) (Point vx2 vy2)) = axis_viewport a

   (sx,sy,ex,ey,vs,ve,tp) = case at of
       AT_Top    -> (x1,y1,x2,y1, vx1, vx2, (Point 0 (-1))) 
       AT_Bottom -> (x1,y2,x2,y2, vx1, vx2, (Point 0 1))		
       AT_Left   -> (x2,y1,x2,y2, vy1, vy2, (Point (1) 0))		
       AT_Right  -> (x1,y1,x1,y2, vy1, vy2,  (Point (-1) 0))

   axisPoint value = 
       let ax = (sx + (ex-sx) * (value - vs) / (ve-vs))
	   ay = (sy + (ey-sy) * (value - vs) / (ve-vs))
       in (Point ax ay)

   drawTick (value,length) = 
       let t1 = axisPoint value
	   t2 = t1 `padd` (pscale length tp)
       in strokeLine t1 t2

   drawLabel (value,s) = do
       let (Point ax ay) = axisPoint value
       Cairo.moveTo ax ay
--       Cairo.scale 1 (-1)
       Cairo.showText s

----------------------------------------------------------------------

data Layout1 = Layout1 {
    layout1_bottom_axis :: Maybe Axis,
    layout1_left_axis :: Maybe Axis,
    layout1_top_axis :: Maybe Axis,
    layout1_right_axis :: Maybe Axis,
    layout1_margin :: Double
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

    renderMAxis AT_Bottom (layout1_bottom_axis l) (mkrect p2 p1 p3 p2)
    renderMAxis AT_Left (layout1_left_axis l) (mkrect p1 p2 p2 p3)
    renderMAxis AT_Top (layout1_top_axis l) (mkrect p2 p3 p3 p4)
    renderMAxis AT_Right (layout1_right_axis l) (mkrect p3 p2 p4 p3)
  where
    renderMAxis at (Just a) rect = render (AxisT at a) rect
    renderMAxis _ Nothing  _ = return ()

    mkrect (Point x1 y1) (Point x2 y2) (Point x3 y3) (Point x4 y4) =
	Rect (Point x1 y2) (Point x3 y4)
    

minsizeLayout1 l = do
  let m = layout1_margin l
  (w1,h1,w2,h2) <- axisSizes l
  return (2*m+w1+w2,2*m+h1+h2)


axisSizes l = do
    w1 <- asize fst AT_Left   (layout1_left_axis l)
    h1 <- asize snd AT_Bottom (layout1_bottom_axis l)
    w2 <- asize fst AT_Right  (layout1_right_axis l)
    h2 <- asize snd AT_Top    (layout1_top_axis l)
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
    layout1_margin = 10
}

----------------------------------------------------------------------

strokeLine p1@(Point p1x p1y) p2@(Point p2x p2y) = do
   Cairo.newPath
   Cairo.moveTo p1x p1y
   Cairo.lineTo p2x p2y
   Cairo.stroke
