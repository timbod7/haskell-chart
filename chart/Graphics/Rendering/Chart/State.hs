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
  colors,
  shapes,

  execEC,
  liftCState,
  ) where

import Control.Lens
import Control.Monad.State
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
data CState = CState {
  _colors :: [AlphaColour Double], -- ^ An infinite source of colors, for use in plots
  _shapes :: [PointShape]          -- ^ An infinite source of shapes, for use in plots
  }

$( makeLenses ''CState )

-- | We use nested State monads to give nice syntax. The outer state
-- is the graphical element being constructed (typically a
-- layout). The inner state contains any additional state
-- reqired. This approach means that lenses and the state monad lens
-- operators can be used directly on the value being constructed.
type EC l a = StateT l (State CState) a

instance Default CState where
  def = CState colors shapes
    where
      colors = cycle (map opaque [blue,red,green,orange,yellow,violet])
      shapes = cycle [PointShapeCircle,PointShapePlus,PointShapeCross,PointShapeStar]
      
instance (Default l,ToRenderable l) => ToRenderable (EC l a) where
  toRenderable ec = toRenderable (execEC ec)

-- | Run the monadic `EC` computation, and return the graphical
-- element (ie the outer monad' state)
execEC :: (Default l) => EC l a -> l
execEC ec = evalState (execStateT ec def) def

-- | Lift a a computation over `CState`
liftCState :: State CState a -> EC l a
liftCState = lift

-- | Add a plot to the `Layout` being constructed.
plot :: ToPlot p => EC (Layout x y) (p x y) -> EC (Layout x y) ()
plot pm = do
  p <- pm
  layout_plots %= (toPlot p:)

-- | Add a plot against the left axis to the `LayoutLR` being constructed.
plotLeft :: ToPlot p => EC (LayoutLR x y1 y2 ) (p x y1) -> EC (LayoutLR x y1 y2) ()
plotLeft pm = do
  p <- pm
  layoutlr_plots %= (Left (toPlot p):)

-- | Add a plot against the right axis tof the `LayoutLR` being constructed.
plotRight :: ToPlot p => EC (LayoutLR x y1 y2 ) (p x y2) -> EC (LayoutLR x y1 y2) ()
plotRight pm = do
  p <- pm
  layoutlr_plots %= (Right (toPlot p):)

-- | Pop and return the next color from the state
takeColor :: EC l (AlphaColour Double)
takeColor = liftCState $ do
  (c:cs) <- use colors
  colors .= cs
  return c

-- | Pop and return the next shape from the state
takeShape :: EC l PointShape
takeShape = liftCState $ do
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

