-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Simple
-- Copyright   :  (c) David Roundy 2007
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- An even simpler framework for creating 2D charts in Haskell.
--
-- The basic idea is to make it as easy to plot as octave, which means that
-- you provide no more information than you wish to provide.  We provide
-- four plotting functions, which differ only in their output.  One
-- produces a "Layout1" that you can customize using other
-- Graphics.Rendering.Chart functions.  The other three produce their
-- output directly.  All three accept the same input and produce the same plots.
--
-- The plot functions accept a variable number of arguments.  You must
-- provide a [Double] which defines the points on the x axis, which must
-- precede any of the "y" values.  The y values may either be [Double] or
-- functions.  After any given y value, you can give either Strings or
-- PlotKinds describing how you'd like that y printed.
--
-- Examples:
--
-- > plotPDF "foo.pdf" [0,0.1..10] sin "- " cos ". " cos "o"
--
-- > plotPS "foo.ps" [0,0.1..10] (sin . exp) "- " (sin . exp) "o-"
-----------------------------------------------------------------------------
module Graphics.Rendering.Chart.Simple( plot, PlotKind(..), xcoords,
                                        plotPDF, plotPS,
                                        plotLayout, plotPNG, LayoutDDD,
                                        layoutDddToRenderable
                                      , PlotPDFType(..)
                                      , PlotPSType(..)
                                      , PlotPNGType(..)
                                      , uplot
                                      ) where

import Graphics.Rendering.Chart.Simple.Internal
