module Test5 where 

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class
import System.Random

import Utils

----------------------------------------------------------------------
-- Example thanks to Russell O'Connor

chart :: Double -> Renderable (Layout1Pick Double LogValue)
chart lwidth = layout1ToRenderable (layout 1001 (trial bits) :: Layout1 Double LogValue)
  where
    bits = randoms $ mkStdGen 0

    layout n t = layout1_title .~ "Simulation of betting on a biased coin"
               $ layout1_plots .~ [
                      Left (toPlot (plot "f=0.05" s1 n 0 (t 0.05))),
                      Left (toPlot (plot "f=0.1" s2 n 0 (t 0.1)))
                     ]
               $ def

    plot tt s n m t = plot_lines_style .~ s
                 $ plot_lines_values .~
                       [[(fromIntegral x, LogValue y) | (x,y) <-
                         filter (\(x,_)-> x `mod` (m+1)==0) $ take n $ zip [0..] t]]
                 $ plot_lines_title .~ tt
                 $ def

    b = 0.1

    trial bits frac = scanl (*) 1 (map f bits)
      where
        f True = (1+frac*(1+b))
        f False = (1-frac)

    s1 = solidLine lwidth $ opaque green
    s2 = solidLine lwidth $ opaque blue

main = main' "test5" (chart 0.25)


