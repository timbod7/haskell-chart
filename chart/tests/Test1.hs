module Test1 where 

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Accessor

import Utils

chart :: (ChartBackend m) => Double -> Renderable m ()
chart lwidth = layout1ToRenderable (layout lwidth)

layout :: (ChartBackend m) => Double -> Layout1 m Double Double
layout lwidth = layout1
  where
    layout1 = layout1_title ^= "Amplitude Modulation"
            $ layout1_plots ^= [Left (toPlot sinusoid1),
			       Left (toPlot sinusoid2)]
            $ layout1_plot_background ^= Just (solidFillStyle $ opaque white)
            $ defaultLayout1

    am x = (sin (x*pi/45) + 1) / 2 * (sin (x*pi/5))

    sinusoid1 = plot_lines_values ^= [[ (x,(am x)) | x <- [0,(0.5)..400]]]
              $ plot_lines_style  ^= solidLine lwidth (opaque blue)
              $ plot_lines_title ^="am"
              $ defaultPlotLines

    sinusoid2 = plot_points_style ^= filledCircles 2 (opaque red)
              $ plot_points_values ^= [ (x,(am x)) | x <- [0,7..400]]
              $ plot_points_title ^="am points"
              $ defaultPlotPoints

main = main' "test1" (chart 0.25)


