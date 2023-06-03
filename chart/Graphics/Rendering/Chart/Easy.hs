{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Easy
-- Copyright   :  (c) Tim Docker 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- A high level API for generating a plot quickly.
--
-- Importing the Easy module brings into scope all core functions and types required
-- for working with the chart library. This includes key external dependencies such as
-- Control.Lens and Data.Colour. The module also provides several helper functions for
-- quickly generating common plots. Note that chart backends must still be explicitly
-- imported, as some backends cannot be built on all platforms.
--
-- Example usage:
--
-- > import Graphics.Rendering.Chart.Easy
-- > import Graphics.Rendering.Chart.Backend.Cairo
-- >
-- > signal :: [Double] -> [(Double,Double)]
-- > signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]
-- >
-- > main = toFile def "example.png" $ do
-- >     layout_title .= "Amplitude Modulation"
-- >     plot (line "am" [signal [0,(0.5)..400]])
-- >     plot (points "am points" (signal [0,7..400]))
--
-- More examples can be found on the <https://github.com/timbod7/haskell-chart/wiki library's wiki>

module Graphics.Rendering.Chart.Easy(

  module Control.Lens,
  module Data.Default.Class,
  module Data.Colour,
  module Data.Colour.Names,

  module Graphics.Rendering.Chart,
  module Graphics.Rendering.Chart.State,

  line,
  points,
  bars,
  setColors,
  setShapes
  ) where

import Control.Lens
import Control.Monad(unless)
import Data.Default.Class
import Data.Colour hiding (over) -- overlaps with lens over function
import Data.Colour.Names
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.State

-- | Set the contents of the colour source, for
-- subsequent plots
setColors :: [AlphaColour Double] -> EC l ()
setColors cs = liftCState $ colors .= cycle cs

-- | Set the contents of the shape source, for
-- subsequent plots
setShapes :: [PointShape] -> EC l ()
setShapes ps = liftCState $ shapes .= cycle ps

-- | Constuct a line plot with the given title and
-- data, using the next available color.
line :: String -> [[(x,y)]]  -> EC l (PlotLines x y)
line title values = liftEC $ do
    color <- takeColor
    plot_lines_title .= title
    plot_lines_values .= values
    plot_lines_style . line_color .= color

-- | Construct a scatter plot with the given title and data, using the
-- next available color and point shape.
points :: String -> [(x,y)]  -> EC l (PlotPoints x y)
points title values = liftEC $ do
    color <- takeColor
    shape <- takeShape
    plot_points_values .= values
    plot_points_title .= title
    plot_points_style . point_color .= color
    plot_points_style . point_shape .= shape
    plot_points_style . point_radius .= 2

    -- Show borders for unfilled shapes
    unless (isFilled shape) $ do
        plot_points_style . point_border_color .= color
        plot_points_style . point_border_width .= 1

isFilled :: PointShape -> Bool
isFilled PointShapeCircle = True
isFilled PointShapePolygon{} = True
isFilled _ = False

-- | Construct a bar chart with the given titles and data, using the
-- next available colors
bars :: (PlotValue x, BarsPlotValue y) => [String] -> [(x,[y])] -> EC l (PlotBars x y)
bars titles vals = liftEC $ do
    styles <- sequence [fmap mkStyle takeColor | _ <- titles]
    plot_bars_titles .= titles
    plot_bars_values .= vals
    plot_bars_style .= BarsClustered
    plot_bars_spacing .= BarsFixGap 30 5
    plot_bars_item_styles .= styles
  where
    mkStyle c = (solidFillStyle c, Just (solidLine 1.0 $ opaque black))
