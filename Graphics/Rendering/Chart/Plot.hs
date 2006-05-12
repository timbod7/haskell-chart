module Graphics.Rendering.Chart.Plot(
    Plot(..),
    ToPlot(..),
    PlotPoints(..),
    PlotLines(..),
    PlotFillBetween(..),

    defaultPlotLineStyle,
    defaultPlotPoints,
    defaultPlotFillBetween,
    defaultPlotLines
    
    ) where

import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart.Types
import Control.Monad

-- | Interface to control plotting on a 2D area.
data Plot = Plot {

    -- | Given the model space coordinates, and the device coordinates
    -- render this plot into a chart.
    plot_render :: Rect -> Rect -> C.Render (),

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
    			
renderPlotLegendLines :: PlotLines -> Rect -> C.Render ()
renderPlotLegendLines p r@(Rect p1 p2) = do
    C.save
    setLineStyle (plot_lines_style p)
    let y = (p_y p1 + p_y p2) / 2
    moveTo (Point (p_x p1) y)
    lineTo (Point (p_x p2) y)
    C.stroke
    C.restore

defaultPlotLineStyle = solidLine 1 0 0 1

defaultPlotLines = PlotLines {
    plot_lines_style = defaultPlotLineStyle,
    plot_lines_values = []
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

renderPlotPoints :: PlotPoints -> Rect -> Rect -> C.Render ()
renderPlotPoints p r v = do
    C.save
    mapM_ (drawPoint.(pmap r v)) (plot_points_values p)
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

renderPlotFillBetween :: PlotFillBetween -> Rect -> Rect -> C.Render ()
renderPlotFillBetween p r v = renderPlotFillBetween' p (plot_fillbetween_values p) r v

renderPlotFillBetween' p [] _ _ = return ()
renderPlotFillBetween' p vs r v = do
    C.save
    setFillStyle (plot_fillbetween_style p)
    moveTo p0
    mapM_ lineTo p1s
    mapM_ lineTo (reverse p2s)
    lineTo p0
    C.fill
    C.restore
  where
    (p0:p1s) = map (pmap r v) [ Point x y1 | (x,(y1,y2)) <- vs ]
    p2s = map (pmap r v) [ Point x y2 | (x,(y1,y2)) <- vs ]

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
    plot_fillbetween_style=solidFillStyle 0.5 0.5 1.0,
    plot_fillbetween_values=[]
}