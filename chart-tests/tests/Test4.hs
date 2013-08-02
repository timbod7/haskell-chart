module Test4 where 

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class

import Utils

chart :: Bool -> Bool -> Renderable (Layout1Pick Double Double)
chart xrev yrev = layout1ToRenderable layout
  where

    points = plot_points_style .~ filledCircles 3 (opaque red)
           $ plot_points_values .~ [ (x, 10**x) | x <- [0.5,1,1.5,2,2.5 :: Double] ]
           $ plot_points_title .~ "values"
           $ def

    lines = plot_lines_values .~ [ [(x, 10**x) | x <- [0,3]] ]
          $ plot_lines_title .~ "values"
          $ def

    layout = layout1_title .~ "Log/Linear Example"
           $ layout1_bottom_axis . laxis_title .~ "horizontal"
           $ layout1_bottom_axis . laxis_reverse .~ xrev
           $ layout1_left_axis . laxis_generate .~ autoScaledLogAxis def
           $ layout1_left_axis . laxis_title .~ "vertical"
           $ layout1_left_axis . laxis_reverse .~ yrev
           $ layout1_plots .~ [Left (toPlot points), Left (toPlot lines) ]
           $ def

main = main' "test4" (chart False False)


