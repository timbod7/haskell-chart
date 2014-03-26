-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Lines
-- Copyright   :  (c) Tim Docker 2006, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Line plots
--
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Lines(
    PlotLines(..),
    defaultPlotLines,
    defaultPlotLineStyle,
    hlinePlot,
    vlinePlot,

    plot_lines_title,
    plot_lines_style,
    plot_lines_values,
    plot_lines_limit_values,
) where

import Control.Lens
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Plot.Types
import Data.Colour (opaque)
import Data.Colour.Names (blue)
import Data.Default.Class

-- | Value defining a series of (possibly disjointed) lines,
--   and a style in which to render them.
data PlotLines x y = PlotLines {
    _plot_lines_title        :: String,
    _plot_lines_style        :: LineStyle,

    -- | The lines to be plotted
    _plot_lines_values       :: [[(x,y)]],

    -- | Additional lines to be plotted, specified using
    -- the Limit type to allow referencing the edges of
    -- the plot area.
    _plot_lines_limit_values :: [[(Limit x, Limit y)]]
}

instance ToPlot PlotLines where
    toPlot p = Plot {
        _plot_render     = renderPlotLines p,
        _plot_legend     = [(_plot_lines_title p, renderPlotLegendLines p)],
        _plot_all_points = ( map fst pts ++ xs, map snd pts ++ ys )
    }
      where
        pts = concat (_plot_lines_values p)
        xs = [ x | (LValue x,_) <- concat (_plot_lines_limit_values p)]
        ys = [ y | (_,LValue y) <- concat (_plot_lines_limit_values p)]

renderPlotLines :: PlotLines x y -> PointMapFn x y -> ChartBackend ()
renderPlotLines p pmap = 
  withLineStyle (_plot_lines_style p) $ do
    mapM_ (drawLines (mapXY pmap)) (_plot_lines_values p)
    mapM_ (drawLines pmap) (_plot_lines_limit_values p)
  where
    drawLines mapfn pts = alignStrokePoints (map mapfn pts) >>= strokePointPath 

renderPlotLegendLines :: PlotLines x y -> Rect -> ChartBackend ()
renderPlotLegendLines p (Rect p1 p2) = 
  withLineStyle (_plot_lines_style p) $ do
    let y = (p_y p1 + p_y p2) / 2
    ps <- alignStrokePoints [Point (p_x p1) y, Point (p_x p2) y]
    strokePointPath ps

defaultPlotLineStyle :: LineStyle
defaultPlotLineStyle = (solidLine 1 $ opaque blue){
     _line_cap  = LineCapRound,
     _line_join = LineJoinRound
 }

{-# DEPRECATED defaultPlotLines "Use the according Data.Default instance!" #-}
defaultPlotLines :: PlotLines x y
defaultPlotLines = def

instance Default (PlotLines x y) where
  def = PlotLines 
    { _plot_lines_title        = ""
    , _plot_lines_style        = defaultPlotLineStyle
    , _plot_lines_values       = []
    , _plot_lines_limit_values = []
    }

-- | Helper function to plot a single horizontal line.
hlinePlot :: String -> LineStyle -> b -> Plot a b
hlinePlot t ls v = toPlot defaultPlotLines {
    _plot_lines_title        = t,
    _plot_lines_style        = ls,
    _plot_lines_limit_values = [[(LMin, LValue v),(LMax, LValue v)]]
    }

-- | Helper function to plot a single vertical line.
vlinePlot :: String -> LineStyle -> a -> Plot a b
vlinePlot t ls v = toPlot defaultPlotLines {
    _plot_lines_title        = t,
    _plot_lines_style        = ls,
    _plot_lines_limit_values = [[(LValue v,LMin),(LValue v,LMax)]]
    }

$( makeLenses ''PlotLines )
