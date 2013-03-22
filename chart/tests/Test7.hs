module Test7 where

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Control.Lens
import System.Environment(getArgs)

chart = toRenderable layout
  where
    vals :: [(Double,Double,Double,Double)]
    vals = [ (x,sin (exp x),sin x/2,cos x/10) | x <- [1..20]]
    bars = plot_errbars_values .~ [symErrPoint x y dx dy | (x,y,dx,dy) <- vals]
         $ plot_errbars_title .~ "test"
         $ defaultPlotErrBars

    points = plot_points_style .~ filledCircles 2 (opaque red)
           $ plot_points_values .~ [(x,y) |  (x,y,dx,dy) <- vals]
           $ plot_points_title .~ "test data"
           $ defaultPlotPoints

    layout = layout1_title .~ "Error Bars"
           $ layout1_plots .~ [Left (toPlot bars),
                               Left (toPlot points)]
           $ defaultLayout1


main1 :: [String] -> IO (PickFn ())
main1 ["small"]  = renderableToPNGFile chart 320 240 "test7_small.png"
main1 ["big"]    = renderableToPNGFile chart 800 600 "test7_big.png"

main = getArgs >>= main1
