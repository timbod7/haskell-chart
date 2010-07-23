module TestParametric where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Colour
import Data.Colour.Names
import Data.Accessor
import System.Environment(getArgs)

chart lwidth = toRenderable layout
  where
    circle = [ (r a * sin (a*dr),r a * cos (a*dr)) | a <- [0,0.5..360::Double] ]
      where
        dr = 2 * pi / 360
        r a = 0.8 * cos (a * 20 * pi /360)

    circleP = plot_lines_values ^= [circle]
            $ plot_lines_style ^= solidLine lwidth (opaque blue) 
            $ defaultPlotLines

    layout = layout1_title ^= "Parametric Plot"
           $ layout1_plots ^= [Left (toPlot circleP)]
           $ defaultLayout1

main1 :: [String] -> IO (PickFn ())
main1 ["small"]  = renderableToPNGFile (chart 1.00) 320 240 "test_parametric_small.png"
main1 ["big"]    = renderableToPNGFile (chart 1.00) 800 600 "test_parametric_big.png"
main1 _          = renderableToWindow  (chart 1.00) 640 480 >> return undefined

main = getArgs >>= main1
