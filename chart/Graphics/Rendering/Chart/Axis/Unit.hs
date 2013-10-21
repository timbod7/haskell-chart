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

import Data.Default.Class

import Graphics.Rendering.Chart.Axis.Types

instance PlotValue () where
    toValue () = 0
    fromValue  = const ()
    autoAxis   = const unitAxis

unitAxis :: AxisData ()
unitAxis = AxisData {
    _axis_visibility = def 
                     { _axis_show_ticks  = False
                     , _axis_show_labels = False 
                     },
    _axis_viewport = \(x0,x1) _ -> (x0+x1)/2,
    _axis_tropweiv = \_       _ -> (),
    _axis_ticks    = [((), 0)],
    _axis_labels   = [[((), "")]],
    _axis_grid     = []
}
