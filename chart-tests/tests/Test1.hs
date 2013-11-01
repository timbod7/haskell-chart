module Test1 where 

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Control.Lens
import Data.Default.Class

import Utils

chart :: Double -> Renderable (LayoutPick Double Double Double)
chart lwidth = layoutToRenderable (layout lwidth)

layout :: Double -> Layout Double Double
layout lwidth = layout'
  where
    layout' = layout_title .~ "Amplitude Modulation"
            $ layout_plots .~ [ toPlot sinusoid1
                              , toPlot sinusoid2 ]
            $ layout_plot_background .~ Just (solidFillStyle $ opaque white)
            $ def

    am x = (sin (x*pi/45) + 1) / 2 * (sin (x*pi/5))

    sinusoid1 = plot_lines_values .~ [[ (x,(am x)) | x <- [0,(0.5)..400]]]
              $ plot_lines_style  .~ solidLine lwidth (opaque blue)
              $ plot_lines_title .~"am"
              $ def

    sinusoid2 = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ [ (x,(am x)) | x <- [0,7..400]]
              $ plot_points_title .~"am points"
              $ def

-- main = main' "test1" (chart 0.25)


