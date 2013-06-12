module Test14a where 

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Plot
import Data.Colour
import Data.Colour.Names
import Data.Accessor
import System.Random
import Prices(prices1)

import Utils

-- demonstrate AreaSpots4D

chart :: (ChartBackend m) => Double -> Renderable m ()
chart lwidth = layout1ToRenderable layout
  where
    layout = layout1_title ^="Price History"
           $ layout1_background ^= solidFillStyle (opaque white)
           $ layout1_left_axis ^: laxis_override ^= axisTicksHide
 	   $ layout1_plots ^= [ Left (toPlot price1), Left (toPlot spots) ]
           $ setLayout1Foreground (opaque black)
           $ defaultLayout1

    price1 = plot_lines_style ^= lineStyle
           $ plot_lines_values ^= [[ (d, v) | (d,v,_) <- prices1]]
           $ plot_lines_title ^= "price 1"
           $ defaultPlotLines

    spots = area_spots_4d_title ^= "random value"
          $ area_spots_4d_max_radius ^= 20
          $ area_spots_4d_values ^= values
          $ defaultAreaSpots4D
    
    points = map (\ (d,v,z,t)-> (d,v) ) values
    values = [ (d, v, z, t) | ((d,v,_),z,t) <- zip3 prices1 zs ts ]
    zs,ts :: [Int]
    zs     = randoms $ mkStdGen 0
    ts     = randomRs (-2,27) $ mkStdGen 1

    lineStyle = line_width ^= 3 * lwidth
              $ line_color ^= opaque blue
              $ defaultPlotLines ^. plot_lines_style

main = main' "test14" (chart 0.25)
