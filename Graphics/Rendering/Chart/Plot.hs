-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Plot(
    Plot(..),
    ToPlot(..),
    PlotPoints(..),
    PlotErrBars(..),
    PlotLines(..),
    PlotFillBetween(..),
    ErrPoint(..),
    symErrPoint,

    defaultPlotLineStyle,
    defaultPlotPoints,
    defaultPlotErrBars,
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
    plot_render :: PointMapFn -> CRender (),

    -- | Render a small sample of this plot into the given rectangle.
    -- This is for used to generate a the legend a chart.
    plot_render_legend :: Rect -> CRender (),

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

renderPlotLines :: PlotLines -> PointMapFn -> CRender ()
renderPlotLines p pmap = preserveCState $ do
    setLineStyle (plot_lines_style p)
    mapM_ drawLines (plot_lines_values p)
  where
    drawLines (p:ps) = do
	moveTo (pmap p)
	mapM_ (\p -> lineTo (pmap p)) ps
	c $ C.stroke

renderPlotLegendLines :: PlotLines -> Rect -> CRender ()
renderPlotLegendLines p r@(Rect p1 p2) = preserveCState $ do
    setLineStyle (plot_lines_style p)
    let y = (p_y p1 + p_y p2) / 2
    moveTo (Point (p_x p1) y)
    lineTo (Point (p_x p2) y)
    c $ C.stroke

defaultPlotLineStyle = (solidLine 1 blue){ 
     line_cap = C.LineCapRound,
     line_join = C.LineJoinRound
 }

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

renderPlotPoints :: PlotPoints -> PointMapFn -> CRender ()
renderPlotPoints p pmap = preserveCState $ do
    mapM_ (drawPoint.pmap) (plot_points_values p)
  where
    (CairoPointStyle drawPoint) = (plot_points_style p)


renderPlotLegendPoints :: PlotPoints -> Rect -> CRender ()
renderPlotLegendPoints p r@(Rect p1 p2) = preserveCState $ do
    drawPoint (Point (p_x p1) ((p_y p1 + p_y p2)/2))
    drawPoint (Point ((p_x p1 + p_x p2)/2) ((p_y p1 + p_y p2)/2))
    drawPoint (Point (p_x p2) ((p_y p1 + p_y p2)/2))

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

renderPlotFillBetween :: PlotFillBetween -> PointMapFn -> CRender ()
renderPlotFillBetween p pmap = renderPlotFillBetween' p (plot_fillbetween_values p) pmap

renderPlotFillBetween' p [] _ = return ()
renderPlotFillBetween' p vs pmap  = preserveCState $ do
    setFillStyle (plot_fillbetween_style p)
    moveTo p0
    mapM_ lineTo p1s
    mapM_ lineTo (reverse p2s)
    lineTo p0
    c $ C.fill
  where
    (p0:p1s) = map pmap [ Point x y1 | (x,(y1,y2)) <- vs ]
    p2s = map pmap [ Point x y2 | (x,(y1,y2)) <- vs ]

renderPlotLegendFill :: PlotFillBetween -> Rect -> CRender ()
renderPlotLegendFill p r = preserveCState $ do
    setFillStyle (plot_fillbetween_style p)
    rectPath r
    c $ C.fill

plotAllPointsFillBetween :: PlotFillBetween -> [Point]
plotAllPointsFillBetween p = concat [ [Point x y1, Point x y2]
				      | (x,(y1,y2)) <- plot_fillbetween_values p]


defaultPlotFillBetween = PlotFillBetween {
    plot_fillbetween_style=solidFillStyle (Color 0.5 0.5 1.0),
    plot_fillbetween_values=[]
}

----------------------------------------------------------------------

-- | Value for holding a point with associated error bounds for
-- each axis.

data ErrValue x = ErrValue {
      ev_low :: x,
      ev_best :: x,
      ev_high :: x
} deriving Show

data ErrPoint = ErrPoint {
      ep_x :: ErrValue Double,
      ep_y :: ErrValue Double
} deriving Show

-- | When the error is symetric, we can simply pass in dx for the error
symErrPoint x y dx dy = ErrPoint (ErrValue (x-dx) x (x+dx))
                                 (ErrValue (y-dy) y (y+dy))

-- | Value defining a series of error intervals, and a style in
-- which to render them
data PlotErrBars = PlotErrBars {
    plot_errbars_line_style :: CairoLineStyle,
    plot_errbars_tick_length :: Double,
    plot_errbars_overhang :: Double,
    plot_errbars_values :: [ErrPoint]
}

instance ToPlot PlotErrBars where
    toPlot p = Plot {
        plot_render = renderPlotErrBars p,
	plot_render_legend = renderPlotLegendErrBars p,
	plot_all_points = [Point (ev_best x) (ev_best y)
                          | ErrPoint x y <- plot_errbars_values p]
    }

renderPlotErrBars :: PlotErrBars -> PointMapFn -> CRender ()
renderPlotErrBars p pmap = preserveCState $ do
    mapM_ (drawErrBar.epmap) (plot_errbars_values p)
  where
    epmap (ErrPoint (ErrValue xl x xh) (ErrValue yl y yh)) =
        ErrPoint (ErrValue xl'' x' xh'') (ErrValue yl'' y' yh'')
        where (Point x' y') = pmap (Point x y)
              (Point xl' yl') = pmap (Point xl yl)
              (Point xh' yh') = pmap (Point xh yh)
              (xl'', xh'') = (min xl' xh', max xl' xh')
              (yl'', yh'') = (min yl' yh', max yl' yh')
    drawErrBar = drawErrBar0 p

drawErrBar0 ps (ErrPoint (ErrValue xl x xh) (ErrValue yl y yh)) = do
        let tl = plot_errbars_tick_length ps
        let oh = plot_errbars_overhang ps
        setLineStyle (plot_errbars_line_style ps)
        c $ C.newPath
        c $ C.moveTo (xl-oh) y
        c $ C.lineTo (xh+oh) y
        c $ C.moveTo x (yl-oh)
        c $ C.lineTo x (yh+oh)
        c $ C.moveTo xl (y-tl)
        c $ C.lineTo xl (y+tl)
        c $ C.moveTo (x-tl) yl
        c $ C.lineTo (x+tl) yl
        c $ C.moveTo xh (y-tl)
        c $ C.lineTo xh (y+tl)
        c $ C.moveTo (x-tl) yh
        c $ C.lineTo (x+tl) yh
	c $ C.stroke

renderPlotLegendErrBars :: PlotErrBars -> Rect -> CRender ()
renderPlotLegendErrBars p r@(Rect p1 p2) = preserveCState $ do
    drawErrBar (symErrPoint (p_x p1) ((p_y p1 + p_y p2)/2) dx dx )
    drawErrBar (symErrPoint ((p_x p1 + p_x p2)/2) ((p_y p1 + p_y p2)/2) dx dx)
    drawErrBar (symErrPoint (p_x p2) ((p_y p1 + p_y p2)/2) dx dx)

  where
    drawErrBar = drawErrBar0 p
    dx = min ((p_x p2 - p_x p1)/6) ((p_y p2 - p_y p1)/2)

defaultPlotErrBars = PlotErrBars {
    plot_errbars_line_style = solidLine 1 blue,
    plot_errbars_tick_length = 3,
    plot_errbars_overhang = 0,
    plot_errbars_values = []
}
