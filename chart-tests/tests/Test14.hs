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

chart :: Double -> Renderable (LayoutPick LocalTime Double Double)
chart lwidth = layoutToRenderable layout
  where
    layout = layout_title .~"Price History"
           $ layout_background .~ solidFillStyle (opaque white)
           $ layout_left_axis_visibility . axis_show_ticks .~ False
 	   $ layout_plots .~ [ toPlot price1, toPlot spots ]
           $ setLayoutForeground (opaque black)
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
