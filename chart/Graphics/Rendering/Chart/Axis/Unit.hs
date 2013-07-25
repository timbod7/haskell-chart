-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.Unit
-- Copyright   :  (c) Tim Docker 2010
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Calculate and render unit indexed axes

module Graphics.Rendering.Chart.Axis.Unit(
    unitAxis,
) where

import Graphics.Rendering.Chart.Axis.Types

instance PlotValue () where
    toValue () = 0
    fromValue  = const ()
    autoAxis   = const unitAxis

unitAxis :: AxisData ()
unitAxis = AxisData {
    axis_viewport_ = \(x0,x1) _ -> (x0+x1)/2,
    axis_tropweiv_ = \_       _ -> (),
    axis_ticks_    = [((), 0)],
    axis_labels_   = [[((), "")]],
    axis_grid_     = []
}
