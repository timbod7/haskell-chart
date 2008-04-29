-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Plot(
    Plot(..),
    ToPlot(..),
    PlotPoints(..),
    PlotErrPoints(..),
    PlotLines(..),
    PlotFillBetween(..),
    ErrPoint(..),

    defaultPlotLineStyle,
    defaultPlotPoints,
    defaultPlotErrPoints,
    defaultPlotFillBetween,
    defaultPlotLines
    
    ) where

import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart.Types
import Control.Monad

-- | Interface to control plotting on a 2D area.
data Plot = Plot {

    -- | Given the mapping between model space coordinates and device coordinates,
    -- render this plot into a chart.
    plot_render :: PointMapFn -> C.Render (),

    -- | Render a small sample of this plot into the given rectangle.
    -- This is for used to generate a the legend a chart.
    plot_render_legend :: Rect -> C.Render (),

    -- | All of the model space coordinates to be plotted. These are
    -- used to autoscale the axes where necessary.
    plot_all_points :: [Point]
};

-- | a type class abstracting the conversion of a value to a Plot.
class ToPlot a where
   toPlot :: a -> Plot

----------------------------------------------------------------------

-- | Value defining a series of (possibly disjointed) lines,
-- and a style in which to render them
data PlotLines = PlotLines {
    plot_lines_style :: CairoLineStyle,
    plot_lines_values :: [[Point]]
}

instance ToPlot PlotLines where
    toPlot p = Plot {
        plot_render = renderPlotLines p,
	plot_render_legend = renderPlotLegendLines p,
	plot_all_points = concat (plot_lines_values p)
    }

renderPlotLines :: PlotLines -> PointMapFn -> C.Render ()
renderPlotLines p pmap = do
    C.save
    setLineStyle (plot_lines_style p)
    mapM_ drawLines (plot_lines_values p)
    C.restore
  where
    drawLines (p:ps) = do
	moveTo (pmap p)
	mapM_ (\p -> lineTo (pmap p)) ps
	C.stroke

renderPlotLegendLines :: PlotLines -> Rect -> C.Render ()
renderPlotLegendLines p r@(Rect p1 p2) = do
    C.save
    setLineStyle (plot_lines_style p)
    let y = (p_y p1 + p_y p2) / 2
    moveTo (Point (p_x p1) y)
    lineTo (Point (p_x p2) y)
    C.stroke
    C.restore

defaultPlotLineStyle = solidLine 1 blue

defaultPlotLines = PlotLines {
    plot_lines_style = defaultPlotLineStyle,
    plot_lines_values = []
}
----------------------------------------------------------------------

-- | Value defining a series of datapoints and error intervals, and a style in
-- which to render them

data ErrPoint = ErrPoint {
      ep_x :: Double,
      ep_y :: Double,
      ep_dx :: Double,
      ep_dy :: Double
} deriving Show

data PlotErrPoints = PlotErrPoints {
    plot_errpoints_line_style :: CairoLineStyle,
    plot_errpoints_point_style :: CairoPointStyle,
    plot_errpoints_tick_length :: Double,
    plot_errpoints_values :: [ErrPoint]
}

instance ToPlot PlotErrPoints where
    toPlot p = Plot {
        plot_render = renderPlotErrPoints p,
	plot_render_legend = renderPlotLegendErrPoints p,
	plot_all_points = map (\(ErrPoint x y _ _) -> Point x y ) $plot_errpoints_values p
    }

renderPlotErrPoints :: PlotErrPoints -> PointMapFn -> C.Render ()
renderPlotErrPoints p pmap = do
    C.save
    mapM_ (drawPoint.pmap.(\(ErrPoint x y _ _) -> Point x y)) (plot_errpoints_values p)
    mapM_ (drawErrPoint.epmap) (plot_errpoints_values p)
    C.restore
  where
    epmap (ErrPoint x y dx dy) = ErrPoint x' y' (abs $ x''-x') (abs $ y''-y')
        where (Point x' y') = pmap (Point x y)
              (Point x'' y'') = pmap (Point (x+dx) (y+dy))
    drawErrPoint = drawErrPoint0 p
    (CairoPointStyle drawPoint) = plot_errpoints_point_style p

drawErrPoint0 ps (ErrPoint x y dx dy) = do
        let (CairoLineStyle setls) = plot_errpoints_line_style ps
        let tl = plot_errpoints_tick_length ps
        setls
        C.newPath
        C.moveTo (x-dx-tl) y
        C.lineTo (x+dx+tl) y
        C.moveTo x (y-dy-tl)
        C.lineTo x (y+dy+tl)
        C.moveTo (x-dx) (y-tl)
        C.lineTo (x-dx) (y+tl)
        C.moveTo (x-tl) (y-dy)
        C.lineTo (x+tl) (y-dy)
        C.moveTo (x+dx) (y-tl)
        C.lineTo (x+dx) (y+tl)
        C.moveTo (x-tl) (y+dy)
        C.lineTo (x+tl) (y+dy)
	C.stroke

renderPlotLegendErrPoints :: PlotErrPoints -> Rect -> C.Render ()
renderPlotLegendErrPoints p r@(Rect p1 p2) = do
    C.save
    drawPoint (Point (p_x p1) ((p_y p1 + p_y p2)/2)) 
    drawPoint (Point ((p_x p1 + p_x p2)/2) ((p_y p1 + p_y p2)/2))
    drawPoint (Point (p_x p2) ((p_y p1 + p_y p2)/2))
    drawErrPoint (ErrPoint (p_x p1) ((p_y p1 + p_y p2)/2) dx dx ) 
    drawErrPoint (ErrPoint ((p_x p1 + p_x p2)/2) ((p_y p1 + p_y p2)/2) dx dx)
    drawErrPoint (ErrPoint (p_x p2) ((p_y p1 + p_y p2)/2) dx dx)
    C.restore

  where
    drawErrPoint = drawErrPoint0 p
    dx = min ((p_x p2 - p_x p1)/6) ((p_y p2 - p_y p1)/2)
    (CairoPointStyle drawPoint) = plot_errpoints_point_style p

defaultPlotErrPoints = PlotErrPoints {
    plot_errpoints_line_style = solidLine 0.7 blue,
    plot_errpoints_point_style = hollowCircles 4 1 blue,
    plot_errpoints_tick_length = 3,
    plot_errpoints_values = []
}
----------------------------------------------------------------------

-- | Value defining a series of datapoints, and a style in
-- which to render them
data PlotPoints = PlotPoints {
    plot_points_style :: CairoPointStyle,
    plot_points_values :: [Point]
}

instance ToPlot PlotPoints where
    toPlot p = Plot {
        plot_render = renderPlotPoints p,
	plot_render_legend = renderPlotLegendPoints p,
	plot_all_points = plot_points_values p
    }

renderPlotPoints :: PlotPoints -> PointMapFn -> C.Render ()
renderPlotPoints p pmap = do
    C.save
    mapM_ (drawPoint.pmap) (plot_points_values p)
    C.restore
  where
    (CairoPointStyle drawPoint) = (plot_points_style p)


renderPlotLegendPoints :: PlotPoints -> Rect -> C.Render ()
renderPlotLegendPoints p r@(Rect p1 p2) = do
    C.save
    drawPoint (Point (p_x p1) ((p_y p1 + p_y p2)/2))
    drawPoint (Point ((p_x p1 + p_x p2)/2) ((p_y p1 + p_y p2)/2))
    drawPoint (Point (p_x p2) ((p_y p1 + p_y p2)/2))
    C.restore

  where
    (CairoPointStyle drawPoint) = (plot_points_style p)

defaultPlotPoints = PlotPoints {
    plot_points_style =defaultPointStyle,
    plot_points_values = []
}
----------------------------------------------------------------------
-- | Value specifying a plot filling the area between two sets of Y
-- coordinates, given common X coordinates.

data PlotFillBetween = PlotFillBetween {
    plot_fillbetween_style :: CairoFillStyle,
    plot_fillbetween_values :: [ (Double, (Double,Double))]
}

instance ToPlot PlotFillBetween where
    toPlot p = Plot {
        plot_render = renderPlotFillBetween p,
	plot_render_legend = renderPlotLegendFill p,
	plot_all_points = plotAllPointsFillBetween p
    }

renderPlotFillBetween :: PlotFillBetween -> PointMapFn -> C.Render ()
renderPlotFillBetween p pmap = renderPlotFillBetween' p (plot_fillbetween_values p) pmap

renderPlotFillBetween' p [] _ = return ()
renderPlotFillBetween' p vs pmap  = do
    C.save
    setFillStyle (plot_fillbetween_style p)
    moveTo p0
    mapM_ lineTo p1s
    mapM_ lineTo (reverse p2s)
    lineTo p0
    C.fill
    C.restore
  where
    (p0:p1s) = map pmap [ Point x y1 | (x,(y1,y2)) <- vs ]
    p2s = map pmap [ Point x y2 | (x,(y1,y2)) <- vs ]

renderPlotLegendFill :: PlotFillBetween -> Rect -> C.Render ()
renderPlotLegendFill p r = do
    C.save
    setFillStyle (plot_fillbetween_style p)
    rectPath r
    C.fill
    C.restore

plotAllPointsFillBetween :: PlotFillBetween -> [Point]
plotAllPointsFillBetween p = concat [ [Point x y1, Point x y2]
				      | (x,(y1,y2)) <- plot_fillbetween_values p]


defaultPlotFillBetween = PlotFillBetween {
    plot_fillbetween_style=solidFillStyle (Color 0.5 0.5 1.0),
    plot_fillbetween_values=[]
}