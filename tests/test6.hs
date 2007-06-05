module Main where

import System.Random
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk

b = 0.1

trial bits frac = scanl (*) 1 (map f bits)
 where
  f True = (1+frac*(1+b))
  f False = (1-frac)

plot s n m t = defaultPlotLines {
        plot_lines_style = s,
        plot_lines_values =
         [[Point (fromIntegral x) y | (x,y) <-
                      filter (\(x,_)->x `mod` (m+1)==0) $ take n $ zip [0..] t]]
    }



layout n t = defaultLayout1 {
       layout1_title="Simulation of betting on a biased coin",			   
       layout1_horizontal_axes=linkedAxes (autoScaledAxis defaultAxis),
        layout1_vertical_axes=linkedAxes (autoScaledLogAxis defaultAxis),
        layout1_plots = [
         ("f=0.05",HA_Bottom,VA_Left,(toPlot (plot s1 n 0 (t 0.05)))),
         ("f=0.1",HA_Bottom,VA_Left,(toPlot (plot s2 n 0 (t 0.1))))]
    }
 where
  s1 = solidLine 1 0 1 0
  s2 = solidLine 1 0 0 1

main = do
  bits <- fmap randoms getStdGen
  renderableToWindow (toRenderable (layout 1001 (trial bits))) 640 480
  renderableToPNGFile (toRenderable (layout 1001 (trial bits))) 640 480 "test6.png"
