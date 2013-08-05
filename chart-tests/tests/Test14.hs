module Test14 where 

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class
import Data.Time.LocalTime
import System.Random
import Prices(prices1)

import Utils

-- demonstrate AreaSpots

chart :: Double -> Renderable (Layout1Pick LocalTime Double)
chart lwidth = layout1ToRenderable layout
  where
    layout = layout1_title .~"Price History"
           $ layout1_background .~ solidFillStyle (opaque white)
           $ layout1_left_axis . laxis_override .~ axisTicksHide
 	   $ layout1_plots .~ [ Left (toPlot price1), Left (toPlot spots) ]
           $ setLayout1Foreground (opaque black)
           $ def

    price1 = plot_lines_style .~ lineStyle
           $ plot_lines_values .~ [[ (d, v) | (d,v,_) <- prices1]]
           $ plot_lines_title .~ "price 1"
           $ def

    spots = area_spots_title .~ "random value"
          $ area_spots_max_radius .~ 20
          $ area_spots_values .~ values
          $ def
    
    points = map (\ (d,v,z)-> (d,v) ) values
    values = [ (d, v, z) | ((d,v,_),z) <- zip prices1 zs ]
    zs    :: [Int]
    zs     = randoms $ mkStdGen 0

    lineStyle = line_width .~ 3 * lwidth
              $ line_color .~ opaque blue
              $ def ^. plot_lines_style

-- main = main' "test14" (chart 0.25)
