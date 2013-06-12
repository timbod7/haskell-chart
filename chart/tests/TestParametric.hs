module TestParametric where

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Accessor
import System.Environment(getArgs)

chart :: (ChartBackend m) => Double -> Renderable m () --(Layout1Pick Double Double)
chart lwidth = layout1ToRenderable layout
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
main1 ["small"]  = renderableToPNGFile (chart 0.25) 320 240 "test1_small.png"
main1 ["big"]    = renderableToPNGFile (chart 0.25) 800 600 "test1_big.png"

main = getArgs >>= main1
