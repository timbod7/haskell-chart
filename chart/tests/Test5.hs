module Test5 where 

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Accessor
import System.Random
import System.Environment(getArgs)

----------------------------------------------------------------------
-- Example thanks to Russell O'Connor

chart :: (ChartBackend m) => Double -> Renderable m ()
chart lwidth = layout1ToRenderable (layout 1001 (trial bits) :: (ChartBackend m) => Layout1 m Double LogValue)
  where
    bits = randoms $ mkStdGen 0

    layout n t = layout1_title ^= "Simulation of betting on a biased coin"
               $ layout1_plots ^= [
                      Left (toPlot (plot "f=0.05" s1 n 0 (t 0.05))),
                      Left (toPlot (plot "f=0.1" s2 n 0 (t 0.1)))
                     ]
               $ defaultLayout1

    plot tt s n m t = plot_lines_style ^= s
                 $ plot_lines_values ^=
                       [[(fromIntegral x, LogValue y) | (x,y) <-
                         filter (\(x,_)-> x `mod` (m+1)==0) $ take n $ zip [0..] t]]
                 $ plot_lines_title ^= tt
                 $ defaultPlotLines

    b = 0.1

    trial bits frac = scanl (*) 1 (map f bits)
      where
        f True = (1+frac*(1+b))
        f False = (1-frac)

    s1 = solidLine lwidth $ opaque green
    s2 = solidLine lwidth $ opaque blue

main1 :: [String] -> IO (PickFn ())
main1 ["small"]  = renderableToPNGFile (chart 0.25) 320 240 "test5_small.png"
main1 ["big"]    = renderableToPNGFile (chart 0.25) 800 600 "test5_big.png"

main = getArgs >>= main1


