module Plot where

import qualified Graphics.Rendering.Cairo as C
import Types

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

renderPlotLegendPoints :: PlotPoints -> Rect -> C.Render ()
renderPlotLegendPoints p r = return ()

renderPlotLegendLines :: PlotLines -> Rect -> C.Render ()
renderPlotLegendLines p r@(Rect p1 p2) = do
    C.save
    setLineStyle (plot_lines_style p)
    let y = (p_y p1 + p_y p2) / 2
    moveTo (Point (p_x p1) y)
    lineTo (Point (p_x p2) y)
    C.stroke
    C.restore


renderPlotLegend :: Plot -> Rect -> C.Render ()
renderPlotLegend (PPoints p) r = renderPlotLegendPoints p r
renderPlotLegend (PLines p)  r = renderPlotLegendLines p r

allPoints:: Plot -> [Point]
allPoints (PPoints ps) = plot_points_values ps
allPoints (PLines  ps) = concat (plot_lines_values ps)

defaultPlotLineStyle = solidLine 1 0 0 1

defaultPlotPoints = PlotPoints {
    plot_points_style =defaultPointStyle,
    plot_points_values = []
}

defaultPlotLines = PlotLines {
    plot_lines_style = defaultPlotLineStyle,
    plot_lines_values = []
}
