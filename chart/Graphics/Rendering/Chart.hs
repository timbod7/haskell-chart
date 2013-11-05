----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart
-- Copyright   :  (c) Tim Docker 2006-2013
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- A framework for creating 2D charts in Haskell.
--
-- The basic model is that you define a value representing a chart to
-- be displayed, and then convert it to a 'Renderable' by applying
-- 'toRenderable'. This 'Renderable' is then actually output by
-- calling a function in an appropriate graphics backend, eg
-- 'renderableToFile'.
--
-- Currently, there are three types of charts:
--
--     * 'Layout' is a standard XY chart
-- 
--     * 'LayoutLR' is an XY chart with independent left
--       and right axes
--
--     *  'PieLayout' is a pie chart
--
-- 'Layout' and 'LayoutLR' charts can be stacked vertically using
-- the 'StackedLayouts' type.
-- 
-- 'Renderable's can be composed in arbitrary ways using the
-- "Graphics.Rendering.Chart.Grid" module.
--
-- Many of the record structure involved in the API have a large
-- number of fields. 'Lens'es are provided to access each field. Also,
-- for each record type, there is generally a default value, which can
-- be accessed through the 'def' value of the 'Default' typeclass.
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Chart(

    module Graphics.Rendering.Chart.Geometry,
    module Graphics.Rendering.Chart.Drawing,
    module Graphics.Rendering.Chart.Renderable,
    module Graphics.Rendering.Chart.Layout,
    module Graphics.Rendering.Chart.Axis,
    module Graphics.Rendering.Chart.Plot,
    module Graphics.Rendering.Chart.Legend,

) where

import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Layout
import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Plot
import Graphics.Rendering.Chart.Legend
