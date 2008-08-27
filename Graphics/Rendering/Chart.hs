-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- A framework for creating 2D charts in Haskell.
--
-- The basic model is that you define a value of type 'Renderable',
-- typically by applying 'toRenderable' to some other value. This
-- 'Renderable' is then actually displayed or output by calling either
-- 'renderableToPNGFile', or 'Graphics.Rendering.Chart.Gtk.renderableToWindow'.
--
-- Currently, the only useful 'Renderable' for displaying charts
-- is created by applying 'toRenderable' to a value of type
-- 'Graphics.Rendering.Chart.Layout.Layout1'
--
-- For a simpler though less flexible API, see "Graphics.Rendering.Chart.Simple".
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Chart(

    module Graphics.Rendering.Chart.Types,
    module Graphics.Rendering.Chart.Renderable,
    module Graphics.Rendering.Chart.Layout,
    module Graphics.Rendering.Chart.Axis,
    module Graphics.Rendering.Chart.Plot,
    module Graphics.Rendering.Chart.Legend,
    module Graphics.Rendering.Chart.Pie,

) where

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Layout
import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Plot
import Graphics.Rendering.Chart.Legend
import Graphics.Rendering.Chart.Pie
