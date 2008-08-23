-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

{-# OPTIONS_GHC -XTemplateHaskell #-}

module Graphics.Rendering.Chart.Plot(
    Plot(..),
    ToPlot(..),
    PlotPoints(..),
    PlotErrBars(..),
    PlotLines(..),
    PlotFillBetween(..),
    ErrPoint(..),
    symErrPoint,

    defaultPlotLineStyle,
    defaultPlotPoints,
    defaultPlotErrBars,
    defaultPlotFillBetween,
    defaultPlotLines,

    plot_lines_style,
    plot_lines_values,

    plot_render,
    plot_render_legend,
    plot_all_points,

    plot_points_style,
    plot_points_values,

    plot_fillbetween_style,
    plot_fillbetween_values,

    plot_errbars_line_style,
    plot_errbars_tick_length,
    plot_errbars_overhang,
    plot_errbars_values,

    ) where

import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart.Types
import Control.Monad
import Data.Accessor.Template

-- | Interface to control plotting on a 2D area.
data Plot x y = Plot {

    -- | Given the mapping between model space coordinates and device coordinates,
    -- render this plot into a chart.
    plot_render_ :: PointMapFn x y -> CRender (),

    -- | Render a small sample of this plot into the given rectangle.
    -- This is for used to generate a the legend a chart.
    plot_render_legend_ :: Rect -> CRender (),

    -- | All of the model space coordinates to be plotted. These are
    -- used to autoscale the axes where necessary.
    plot_all_points_ :: [(x,y)]
}

-- | a type class abstracting the conversion of a value to a Plot.
class ToPlot a where
   toPlot :: a x y -> Plot x y

----------------------------------------------------------------------

-- | Value defining a series of (possibly disjointed) lines,
-- and a style in which to render them
data PlotLines x y = PlotLines {
    plot_lines_style_ :: CairoLineStyle,
    plot_lines_values_ :: [[(x,y)]]
}


instance ToPlot PlotLines where
    toPlot p = Plot {
        plot_render_ = renderPlotLines p,
	plot_render_legend_ = renderPlotLegendLines p,
	plot_all_points_ = concat (plot_lines_values_ p)
    }

renderPlotLines :: PlotLines x y -> PointMapFn x y -> CRender ()
renderPlotLines p pmap = preserveCState $ do
    setLineStyle (plot_lines_style_ p)
    mapM_ drawLines (plot_lines_values_ p)
  where
    drawLines (p:ps) = do
	moveTo (pmap p)
	mapM_ (\p -> lineTo (pmap p)) ps
	c $ C.stroke

renderPlotLegendLines :: PlotLines x y -> Rect -> CRender ()
renderPlotLegendLines p r@(Rect p1 p2) = preserveCState $ do
    setLineStyle (plot_lines_style_ p)
    let y = (p_y p1 + p_y p2) / 2
    moveTo (Point (p_x p1) y)
    lineTo (Point (p_x p2) y)
    c $ C.stroke

defaultPlotLineStyle = (solidLine 1 blue){ 
     line_cap_ = C.LineCapRound,
     line_join_ = C.LineJoinRound
 }

defaultPlotLines = PlotLines {
    plot_lines_style_ = defaultPlotLineStyle,
    plot_lines_values_ = []
}
----------------------------------------------------------------------

-- | Value defining a series of datapoints, and a style in
-- which to render them
data PlotPoints x y = PlotPoints {
    plot_points_style_ :: CairoPointStyle,
    plot_points_values_ :: [(x,y)]
}


instance ToPlot PlotPoints where
    toPlot p = Plot {
        plot_render_ = renderPlotPoints p,
	plot_render_legend_ = renderPlotLegendPoints p,
	plot_all_points_ = plot_points_values_ p
    }

renderPlotPoints :: PlotPoints x y -> PointMapFn x y -> CRender ()
renderPlotPoints p pmap = preserveCState $ do
    mapM_ (drawPoint.pmap) (plot_points_values_ p)
  where
    (CairoPointStyle drawPoint) = (plot_points_style_ p)


renderPlotLegendPoints :: PlotPoints x y -> Rect -> CRender ()
renderPlotLegendPoints p r@(Rect p1 p2) = preserveCState $ do
    drawPoint (Point (p_x p1) ((p_y p1 + p_y p2)/2))
    drawPoint (Point ((p_x p1 + p_x p2)/2) ((p_y p1 + p_y p2)/2))
    drawPoint (Point (p_x p2) ((p_y p1 + p_y p2)/2))

  where
    (CairoPointStyle drawPoint) = (plot_points_style_ p)

defaultPlotPoints = PlotPoints {
    plot_points_style_ =defaultPointStyle,
    plot_points_values_ = []
}
----------------------------------------------------------------------
-- | Value specifying a plot filling the area between two sets of Y
-- coordinates, given common X coordinates.

data PlotFillBetween x y = PlotFillBetween {
    plot_fillbetween_style_ :: CairoFillStyle,
    plot_fillbetween_values_ :: [ (x, (y,y))]
}


instance ToPlot PlotFillBetween where
    toPlot p = Plot {
        plot_render_ = renderPlotFillBetween p,
	plot_render_legend_ = renderPlotLegendFill p,
	plot_all_points_ = plotAllPointsFillBetween p
    }

renderPlotFillBetween :: PlotFillBetween x y -> PointMapFn x y -> CRender ()
renderPlotFillBetween p pmap = renderPlotFillBetween' p (plot_fillbetween_values_ p) pmap

renderPlotFillBetween' p [] _ = return ()
renderPlotFillBetween' p vs pmap  = preserveCState $ do
    setFillStyle (plot_fillbetween_style_ p)
    moveTo p0
    mapM_ lineTo p1s
    mapM_ lineTo (reverse p2s)
    lineTo p0
    c $ C.fill
  where
    (p0:p1s) = map pmap [ (x,y1) | (x,(y1,y2)) <- vs ]
    p2s = map pmap [ (x,y2) | (x,(y1,y2)) <- vs ]

renderPlotLegendFill :: PlotFillBetween x y -> Rect -> CRender ()
renderPlotLegendFill p r = preserveCState $ do
    setFillStyle (plot_fillbetween_style_ p)
    rectPath r
    c $ C.fill

plotAllPointsFillBetween :: PlotFillBetween x y -> [(x,y)]
plotAllPointsFillBetween p = concat [ [(x, y1), (x, y2)]
				      | (x,(y1,y2)) <- plot_fillbetween_values_ p]


defaultPlotFillBetween = PlotFillBetween {
    plot_fillbetween_style_=solidFillStyle (Color 0.5 0.5 1.0),
    plot_fillbetween_values_=[]
}

----------------------------------------------------------------------

-- | Value for holding a point with associated error bounds for
-- each axis.

data ErrValue x = ErrValue {
      ev_low :: x,
      ev_best :: x,
      ev_high :: x
} deriving Show

data ErrPoint x y = ErrPoint {
      ep_x :: ErrValue x,
      ep_y :: ErrValue y
} deriving Show

-- | When the error is symetric, we can simply pass in dx for the error
symErrPoint x y dx dy = ErrPoint (ErrValue (x-dx) x (x+dx))
                                 (ErrValue (y-dy) y (y+dy))

-- | Value defining a series of error intervals, and a style in
-- which to render them
data PlotErrBars x y = PlotErrBars {
    plot_errbars_line_style_ :: CairoLineStyle,
    plot_errbars_tick_length_ :: Double,
    plot_errbars_overhang_ :: Double,
    plot_errbars_values_ :: [ErrPoint x y]
}


instance ToPlot PlotErrBars where
    toPlot p = Plot {
        plot_render_ = renderPlotErrBars p,
	plot_render_legend_ = renderPlotLegendErrBars p,
	plot_all_points_ = concat
         [[((ev_low x),(ev_low y)), ((ev_high x),(ev_high y))]
         | ErrPoint x y <- plot_errbars_values_ p]
    }

renderPlotErrBars :: PlotErrBars x y -> PointMapFn x y -> CRender ()
renderPlotErrBars p pmap = preserveCState $ do
    mapM_ (drawErrBar.epmap) (plot_errbars_values_ p)
  where
    epmap (ErrPoint (ErrValue xl x xh) (ErrValue yl y yh)) =
        ErrPoint (ErrValue xl' x' xh') (ErrValue yl' y' yh')
        where (Point x' y') = pmap (x,y)
              (Point xl' yl') = pmap (xl,yl)
              (Point xh' yh') = pmap (xh,yh)
    drawErrBar = drawErrBar0 p

drawErrBar0 ps (ErrPoint (ErrValue xl x xh) (ErrValue yl y yh)) = do
        let tl = plot_errbars_tick_length_ ps
        let oh = plot_errbars_overhang_ ps
        setLineStyle (plot_errbars_line_style_ ps)
        c $ C.newPath
        c $ C.moveTo (xl-oh) y
        c $ C.lineTo (xh+oh) y
        c $ C.moveTo x (yl-oh)
        c $ C.lineTo x (yh+oh)
        c $ C.moveTo xl (y-tl)
        c $ C.lineTo xl (y+tl)
        c $ C.moveTo (x-tl) yl
        c $ C.lineTo (x+tl) yl
        c $ C.moveTo xh (y-tl)
        c $ C.lineTo xh (y+tl)
        c $ C.moveTo (x-tl) yh
        c $ C.lineTo (x+tl) yh
	c $ C.stroke

renderPlotLegendErrBars :: PlotErrBars x y -> Rect -> CRender ()
renderPlotLegendErrBars p r@(Rect p1 p2) = preserveCState $ do
    drawErrBar (symErrPoint (p_x p1) ((p_y p1 + p_y p2)/2) dx dx )
    drawErrBar (symErrPoint ((p_x p1 + p_x p2)/2) ((p_y p1 + p_y p2)/2) dx dx)
    drawErrBar (symErrPoint (p_x p2) ((p_y p1 + p_y p2)/2) dx dx)

  where
    drawErrBar = drawErrBar0 p
    dx = min ((p_x p2 - p_x p1)/6) ((p_y p2 - p_y p1)/2)

defaultPlotErrBars = PlotErrBars {
    plot_errbars_line_style_ = solidLine 1 blue,
    plot_errbars_tick_length_ = 3,
    plot_errbars_overhang_ = 0,
    plot_errbars_values_ = []
}

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor for each field
$( deriveAccessors ''Plot )
$( deriveAccessors ''PlotLines )
$( deriveAccessors ''PlotPoints )
$( deriveAccessors ''PlotFillBetween )
$( deriveAccessors ''PlotErrBars )
