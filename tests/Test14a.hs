module Test14a where 

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.AreaSpots
import Data.Colour
import Data.Colour.Names
import Data.Accessor
import System.Random
import System.Environment(getArgs)
import Prices(prices1)

-- demonstrate AreaSpots4D

chart :: Double -> Renderable ()
chart lwidth = toRenderable layout
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

main1 :: [String] -> IO (PickFn ())
main1 ["small"]  = renderableToPNGFile (chart 0.25) 320 240 "test14_small.png"
main1 ["big"]    = renderableToPNGFile (chart 0.25) 800 600 "test14_big.png"
main1 _          = renderableToWindow  (chart 1.00) 640 480 >> return undefined

main = getArgs >>= main1
