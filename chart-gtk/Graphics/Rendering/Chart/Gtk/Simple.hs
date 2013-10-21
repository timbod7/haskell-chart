-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Gtk.Simple
-- Copyright   :  (c) David Roundy 2007, Tim Docker 2012
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Additional gtk specific functions to support the "simple" chart
-- framework. See Graphics.Rendering.Chart.Simple for details.
-----------------------------------------------------------------------------

module Graphics.Rendering.Chart.Gtk.Simple(
   plotWindow
   ) where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Simple.Internal

-- | Display a plot on the screen.

plotWindow :: PlotWindowType a => a
plotWindow = plw []
class PlotWindowType t where
    plw     :: [UPlot] -> t
instance (PlotArg a, PlotWindowType r) => PlotWindowType (a -> r) where
    plw args = \ a -> plw (toUPlot a ++ args)
instance PlotWindowType (IO a) where
    plw args = do
        renderableToWindow (layoutDddToRenderable $ uplot (reverse args)) 640 480
        return undefined


