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
   minsize  :: a -> (Double,Double)
   render   :: a -> Rect -> Cairo.Render ()

----------------------------------------------------------------------

data AxisType = AS_Top | AS_Bottom | AS_Left | AS_Right

data Axis =  Axis {
    axis_viewport :: Rect,
    axis_type :: AxisType,
    axis_line_style :: CairoLineStyle,
    axis_label_style :: CairoFontStyle,

    axis_ticks :: [(Double,Double)],
    axis_labels :: [ (Double, String) ],
    axis_label_gap :: Double 
}

instance Renderable Axis where
   minsize = minsizeAxis
   render  = renderAxis 

minsizeAxis :: Axis -> (Double,Double)
minsizeAxis a = (s,s)
   where s = maximum (map snd (axis_ticks a))

renderAxis :: Axis -> Rect -> Cairo.Render ()
renderAxis a rect = do
   Cairo.save
   lineStyle
   strokeLine (Point sx sy) (Point ex ey)
   mapM_ drawTick (axis_ticks a)
   fontStyle
   mapM_ drawLabel (axis_labels a)
   Cairo.restore
 where
   (CairoLineStyle lineStyle) = axis_line_style a
   (CairoFontStyle fontStyle) = axis_label_style a

   (Rect (Point x1 y1) (Point x2 y2)) = rect

   (Rect (Point vx1 vy1) (Point vx2 vy2)) = axis_viewport a

   (sx,sy,ex,ey,vs,ve,tp) = case axis_type a of
       AS_Top    -> (x1,y1,x2,y1, vx1, vx2, (Point 0 (-1))) 
       AS_Bottom -> (x1,y2,x2,y2, vx1, vx2, (Point 0 1))		
       AS_Left   -> (x2,y1,x2,y2, vy1, vy2, (Point (1) 0))		
       AS_Right  -> (x1,y1,x1,y2, vy1, vy2,  (Point (-1) 0))

   axisPoint value = 
       let ax = (sx + (ex-sx) * (value - vs) / (ve-vs))
	   ay = (sy + (ey-sy) * (value - vs) / (ve-vs))
       in (Point ax ay)

   drawTick (value,length) = 
       let t1 = axisPoint value
	   t2 = t1 `padd` (pscale length tp)
       in strokeLine t1 t2

   drawLabel (value,s) = 
       return ()

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
    renderMAxis (layout1_bottom_axis l) (mkrect p2 p1 p3 p2)
    renderMAxis (layout1_left_axis l) (mkrect p1 p2 p2 p3)
    renderMAxis (layout1_top_axis l) (mkrect p2 p3 p3 p4)
    renderMAxis (layout1_right_axis l) (mkrect p3 p2 p4 p3)
  where
    renderMAxis (Just a) rect = render a rect
    renderMAxis Nothing  _ = return ()

    mkrect (Point x1 y1) (Point x2 y2) (Point x3 y3) (Point x4 y4) =
	Rect (Point x1 y2) (Point x3 y4)

    p1 = p0 `padd` mp
    p2 = p1 `padd` (Point w1 h1)

    p4  = p5 `psub` mp
    p3  = p4 `psub` (Point w2 h2)
    
    mp = let s = (layout1_margin l) in (Point s s)
    (w1,h1,w2,h2) = axisSizes l

minsizeLayout1 l = (2*m+w1+w2,2*m+h1+h2)
  where 
    m = layout1_margin l
    (w1,h1,w2,h2) = axisSizes l

axisSizes l = (w1,h1,w2,h2)
  where
    w1 = maybe 0 (\a -> fst (minsize a)) (layout1_left_axis l) 
    h1 = maybe 0 (\a -> snd (minsize a)) (layout1_bottom_axis l) 
    w2 = maybe 0 (\a -> fst (minsize a)) (layout1_right_axis l) 
    h2 = maybe 0 (\a -> snd (minsize a)) (layout1_top_axis l) 

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
