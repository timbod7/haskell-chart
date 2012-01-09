-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Lines
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Line plots
--
{-# OPTIONS_GHC -XTemplateHaskell #-}

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

import Data.Accessor.Template
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Plot.Types
import Data.Colour (opaque)
import Data.Colour.Names (black, blue)

-- | Value defining a series of (possibly disjointed) lines,
--   and a style in which to render them.
data PlotLines x y = PlotLines {
    plot_lines_title_        :: String,
    plot_lines_style_        :: CairoLineStyle,

    -- | The lines to be plotted
    plot_lines_values_       :: [[(x,y)]],

    -- | Additional lines to be plotted, specified using
    -- the Limit type to allow referencing the edges of
    -- the plot area.
    plot_lines_limit_values_ :: [[(Limit x, Limit y)]]
}

instance ToPlot PlotLines where
    toPlot p = Plot {
        plot_render_     = renderPlotLines p,
        plot_legend_     = [(plot_lines_title_ p, renderPlotLegendLines p)],
        plot_all_points_ = ( map fst pts ++ xs, map snd pts ++ ys )
    }
      where
        pts = concat (plot_lines_values_ p)
        xs = [ x | (LValue x,_) <- concat (plot_lines_limit_values_ p)]
        ys = [ y | (_,LValue y) <- concat (plot_lines_limit_values_ p)]

renderPlotLines :: PlotLines x y -> PointMapFn x y -> CRender ()
renderPlotLines p pmap = preserveCState $ do
    setLineStyle (plot_lines_style_ p)
    mapM_ (drawLines (mapXY pmap)) (plot_lines_values_ p)
    mapM_ (drawLines pmap) (plot_lines_limit_values_ p)
  where
    drawLines mapfn pts = strokePath (map mapfn pts)

renderPlotLegendLines :: PlotLines x y -> Rect -> CRender ()
renderPlotLegendLines p r@(Rect p1 p2) = preserveCState $ do
    setLineStyle (plot_lines_style_ p)
    let y = (p_y p1 + p_y p2) / 2
    strokePath [Point (p_x p1) y, Point (p_x p2) y]

defaultPlotLineStyle :: CairoLineStyle
defaultPlotLineStyle = (solidLine 1 $ opaque blue){
     line_cap_  = C.LineCapRound,
     line_join_ = C.LineJoinRound
 }

defaultPlotLines :: PlotLines x y
defaultPlotLines = PlotLines {
    plot_lines_title_        = "",
    plot_lines_style_        = defaultPlotLineStyle,
    plot_lines_values_       = [],
    plot_lines_limit_values_ = []
}

-- | Helper function to plot a single horizontal line.
hlinePlot :: String -> CairoLineStyle -> b -> Plot a b
hlinePlot t ls v = toPlot defaultPlotLines {
    plot_lines_title_        = t,
    plot_lines_style_        = ls,
    plot_lines_limit_values_ = [[(LMin, LValue v),(LMax, LValue v)]]
    }

-- | Helper function to plot a single vertical line.
vlinePlot :: String -> CairoLineStyle -> a -> Plot a b
vlinePlot t ls v = toPlot defaultPlotLines {
    plot_lines_title_        = t,
    plot_lines_style_        = ls,
    plot_lines_limit_values_ = [[(LValue v,LMin),(LValue v,LMax)]]
    }

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.
$( deriveAccessors ''PlotLines )




