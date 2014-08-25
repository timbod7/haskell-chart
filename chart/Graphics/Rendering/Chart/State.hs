{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Graphics.Rendering.Chart.State(
  plot,
  plotLeft,
  plotRight,
  line,
  points,
  takeColor,
  takeShape,

  CState,
  layout,
  colors,
  shapes
  
  ) where

import Control.Lens
import Control.Monad.State(State,execState)
import Data.Default.Class
import Data.List(cycle)

import Data.Colour
import Data.Colour.Names

import Graphics.Rendering.Chart.Layout
import Graphics.Rendering.Chart.Plot
import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Renderable

-- | The state held when monadically constructing a graphical element
data CState l = CState {
  _layout :: l,                    -- ^ The graphical element being constructed
  _colors :: [AlphaColour Double], -- ^ An infinite source of colors, for use in plots
  _shapes :: [PointShape]          -- ^ An infinite source of shapes, for use in plots
  }

$( makeLenses ''CState )

type EC l a = State (CState l) a

instance (Default l) => Default (CState l) where
  def = CState def colors shapes
    where
      colors = cycle (map opaque [blue,red,green,orange,yellow,violet])
      shapes = cycle [PointShapeCircle,PointShapePlus,PointShapeCross,PointShapeStar]
      
instance (Default l,ToRenderable l) => ToRenderable (EC l a) where
  toRenderable ec = toRenderable (_layout (execState ec def))

-- | Add a plot to the `Layout` being constructed.
plot :: ToPlot p => EC (Layout x y) (p x y) -> EC (Layout x y) ()
plot pm = do
  p <- pm
  layout . layout_plots %= (toPlot p:)

-- | Add a plot against the left axis to the `LayoutLR` being constructed.
plotLeft :: ToPlot p => EC (LayoutLR x y1 y2 ) (p x y1) -> EC (LayoutLR x y1 y2) ()
plotLeft pm = do
  p <- pm
  layout . layoutlr_plots %= (Left (toPlot p):)

-- | Add a plot against the right axis tof the `LayoutLR` being constructed.
plotRight :: ToPlot p => EC (LayoutLR x y1 y2 ) (p x y2) -> EC (LayoutLR x y1 y2) ()
plotRight pm = do
  p <- pm
  layout . layoutlr_plots %= (Right (toPlot p):)

-- | Pop and return the next color from the state
takeColor :: EC l (AlphaColour Double)
takeColor = do
  (c:cs) <- use colors
  colors .= cs
  return c

-- | Pop and return the next shape from the state
takeShape :: EC l PointShape
takeShape = do
  (c:cs) <- use shapes
  shapes .= cs
  return c

-- | Constuct a line plot with the given title and
-- data, using the next available color.
line :: String -> [[(x,y)]]  -> EC l (PlotLines x y)
line title values = do
    color <- takeColor
    return (
      plot_lines_values .~ values $
      plot_lines_title .~ title $
      plot_lines_style . line_color .~ color $
      def )

-- | Construct a scatter plot with the given title and dtda, using the
-- next available color and point shape.
points :: String -> [(x,y)]  -> EC l (PlotPoints x y)
points title values = do
    color <- takeColor
    shape <- takeShape
    return (
      plot_points_values .~ values $
      plot_points_title .~ title $
      plot_points_style . point_color .~ color $
      plot_points_style . point_shape .~ shape $
      plot_points_style . point_radius .~ 2 $
      def )

