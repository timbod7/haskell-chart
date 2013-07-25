-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Types
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Datatypes and functions common to the implementation of the various
-- plot types.
--
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Types(
    Plot(..),
    joinPlot,
    ToPlot(..),

    mapXY,

    plot_render,
    plot_legend,
    plot_all_points,

    ) where

import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Renderable
import Control.Monad
import Data.Accessor.Template
import Data.Colour
import Data.Colour.Names

-- | Interface to control plotting on a 2D area.
data Plot x y = Plot {

    -- | Given the mapping between model space coordinates and device
    --   coordinates, render this plot into a chart.
    plot_render_     :: PointMapFn x y -> ChartBackend (),

    -- | Details for how to show this plot in a legend. For each item
    --   the string is the text to show, and the function renders a
    --   graphical sample of the plot.
    plot_legend_     :: [ (String, Rect -> ChartBackend ()) ],

    -- | All of the model space coordinates to be plotted. These are
    --   used to autoscale the axes where necessary.
    plot_all_points_ :: ([x],[y])
}

-- | A type class abstracting the conversion of a value to a Plot.
class ToPlot a where
   toPlot :: a x y -> Plot x y

-- | Join any two plots together (they will share a legend).
joinPlot :: Plot x y -> Plot x y -> Plot x y
joinPlot Plot{ plot_render_     = renderP
             , plot_legend_     = legendP
             , plot_all_points_ = (xsP,ysP) }
         Plot{ plot_render_     = renderQ
             , plot_legend_     = legendQ
             , plot_all_points_ = (xsQ,ysQ) }

       = Plot{ plot_render_     = \a-> renderP a >> renderQ a
             , plot_legend_     = legendP ++ legendQ
             , plot_all_points_ = ( xsP++xsQ, ysP++ysQ )
             }


----------------------------------------------------------------------

mapXY :: PointMapFn x y -> ((x,y) -> Point)
mapXY f (x,y) = f (LValue x, LValue y)

----------------------------------------------------------------------



----------------------------------------------------------------------

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.
$( deriveAccessors ''Plot )
