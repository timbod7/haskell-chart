module Test1 where 

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Accessor
import System.Environment(getArgs)

chart lwidth = toRenderable (layout lwidth)

layout :: Double -> Layout1 Double Double
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

main1 :: [String] -> IO (PickFn ())
main1 ["small"]  = renderableToPNGFile (chart 0.25) 320 240 "test1_small.png"
main1 ["big"]    = renderableToPNGFile (chart 0.25) 800 600 "test1_big.png"
main1 _          = renderableToWindow  (chart 1.00) 640 480 >> return undefined

main = getArgs >>= main1


