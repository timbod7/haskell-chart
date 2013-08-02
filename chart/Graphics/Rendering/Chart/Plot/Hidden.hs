-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Hidden
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Plots that don't show, but occupy space so as to effect axis
-- scaling
--
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Hidden(
    PlotHidden(..),
) where

import Control.Lens
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Plot.Types

-- | Value defining some hidden x and y values. The values don't
--   get displayed, but still affect axis scaling.
data PlotHidden x y = PlotHidden {
    _plot_hidden_x_values :: [x],
    _plot_hidden_y_values :: [y]
}

instance ToPlot PlotHidden where
    toPlot ph = Plot {
        _plot_render     = \_ -> return (),
        _plot_legend     = [],
        _plot_all_points = (_plot_hidden_x_values ph, _plot_hidden_y_values ph)
    }

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.

$( makeLenses ''PlotHidden )

