module Test4 where 

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Accessor
import System.Environment(getArgs)

chart :: (ChartBackend m) => Bool -> Bool -> Renderable m ()
chart xrev yrev = layout1ToRenderable layout
  where

    points = plot_points_style ^= filledCircles 3 (opaque red)
           $ plot_points_values ^= [ (x, 10**x) | x <- [0.5,1,1.5,2,2.5 :: Double] ]
           $ plot_points_title ^= "values"
           $ defaultPlotPoints

    lines = plot_lines_values ^= [ [(x, 10**x) | x <- [0,3]] ]
          $ plot_lines_title ^= "values"
          $ defaultPlotLines

    layout = layout1_title ^= "Log/Linear Example"
           $ layout1_bottom_axis ^: laxis_title ^= "horizontal"
           $ layout1_bottom_axis ^: laxis_reverse ^= xrev
           $ layout1_left_axis ^: laxis_generate ^= autoScaledLogAxis defaultLogAxis
           $ layout1_left_axis ^: laxis_title ^= "vertical"
           $ layout1_left_axis ^: laxis_reverse ^= yrev
	   $ layout1_plots ^= [Left (toPlot points), Left (toPlot lines) ]
           $ defaultLayout1

main1 :: [String] -> IO (PickFn ())
main1 ["small"]  = renderableToPNGFile (chart False False) 320 240 "test4_small.png"
main1 ["big"]    = renderableToPNGFile (chart False False) 800 600 "test4_big.png"

main = getArgs >>= main1


