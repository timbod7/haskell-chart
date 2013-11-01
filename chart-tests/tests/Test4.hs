module Test4 where 

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class

import Utils

chart :: Bool -> Bool -> Renderable (LayoutPick Double Double Double)
chart xrev yrev = layoutToRenderable layout
  where

    points = plot_points_style .~ filledCircles 3 (opaque red)
           $ plot_points_values .~ [ (x, 10**x) | x <- [0.5,1,1.5,2,2.5 :: Double] ]
           $ plot_points_title .~ "values"
           $ def

    lines = plot_lines_values .~ [ [(x, 10**x) | x <- [0,3]] ]
          $ plot_lines_title .~ "values"
          $ def

    layout = layout_title .~ "Log/Linear Example"
           $ layout_x_axis . laxis_title .~ "horizontal"
           $ layout_x_axis . laxis_reverse .~ xrev
           $ layout_y_axis . laxis_generate .~ autoScaledLogAxis def
           $ layout_y_axis . laxis_title .~ "vertical"
           $ layout_y_axis . laxis_reverse .~ yrev
           $ layout_plots .~ [ toPlot points, toPlot lines ]
           $ def

-- main = main' "test4" (chart False False)


