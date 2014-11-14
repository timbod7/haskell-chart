import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens
import System.Random
import System.Environment(getArgs)
import Prices(prices1)

-- demonstrate AreaSpots4D

chart :: Double -> Renderable ()
chart lwidth = toRenderable layout
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

    spots = area_spots_4d_title .~ "random value"
          $ area_spots_4d_max_radius .~ 20
          $ area_spots_4d_values .~ values
          $ def
    
    points = map (\ (d,v,z,t)-> (d,v) ) values
    values = [ (d, v, z, t) | ((d,v,_),z,t) <- zip3 prices1 zs ts ]
    zs,ts :: [Int]
    zs     = randoms $ mkStdGen 0
    ts     = randomRs (-2,27) $ mkStdGen 1

    lineStyle = line_width .~ 3 * lwidth
              $ line_color .~ opaque blue
              $ def

main = renderableToFile def (chart 0.25) "example8_big.png"
