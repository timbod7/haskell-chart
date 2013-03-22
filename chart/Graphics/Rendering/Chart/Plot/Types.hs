-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Types
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Datatypes and functions common to the implementation of the various
-- plot types.
--
{-# OPTIONS_GHC -XTemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Types(
    Plot(..),
    joinPlot,
    ToPlot(..),

    mapXY,

    plot_render,
    plot_legend,
    plot_all_points,

    ) where

import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Renderable
import Control.Monad
import Control.Lens
import Data.Colour
import Data.Colour.Names

-- | Interface to control plotting on a 2D area.
data Plot x y = Plot {

    -- | Given the mapping between model space coordinates and device
    --   coordinates, render this plot into a chart.
    _plot_render     :: PointMapFn x y -> CRender (),

    -- | Details for how to show this plot in a legend. For each item
    --   the string is the text to show, and the function renders a
    --   graphical sample of the plot.
    _plot_legend     :: [ (String, Rect -> CRender ()) ],

    -- | All of the model space coordinates to be plotted. These are
    --   used to autoscale the axes where necessary.
    _plot_all_points :: ([x],[y])
}

-- | A type class abstracting the conversion of a value to a Plot.
class ToPlot a where
   toPlot :: a x y -> Plot x y

-- | Join any two plots together (they will share a legend).
joinPlot :: Plot x y -> Plot x y -> Plot x y
joinPlot Plot{ _plot_render     = renderP
             , _plot_legend     = legendP
             , _plot_all_points = (xsP,ysP) }
         Plot{ _plot_render     = renderQ
             , _plot_legend     = legendQ
             , _plot_all_points = (xsQ,ysQ) }

       = Plot{ _plot_render     = \a-> renderP a >> renderQ a
             , _plot_legend     = legendP ++ legendQ
             , _plot_all_points = ( xsP++xsQ, ysP++ysQ )
             }


----------------------------------------------------------------------

mapXY :: PointMapFn x y -> ((x,y) -> Point)
mapXY f (x,y) = f (LValue x, LValue y)

----------------------------------------------------------------------



----------------------------------------------------------------------

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.
$( makeLenses ''Plot )
