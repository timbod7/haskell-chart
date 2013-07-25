----------------------------------------------------------------------------
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
-- 'renderableToPNGFile', or 'renderableToWindow'.
--
-- Currently, there are two kinds of 'Renderable' for displaying charts:
--
--     * a standard two axes chart can be is created by applying
--      'toRenderable' to a value of type 'Layout1'
--
--     *  a pie chart can be is created by applying
--      'toRenderable' to a value of type 'PieLayout'
--
-- Multiple Renderables can be composed using the "Graphics.Rendering.Chart.Grid" module.
--
-- Many of the record structure involved in the API have a large
-- number of fields.  For each record type X, there is generally a
-- default value called defaultX with sensibly initialised fields.
-- For example, 'Layout1' has 'defaultLayout1', etc.
--
-- For a simpler though less flexible API, see "Graphics.Rendering.Chart.Simple".
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
