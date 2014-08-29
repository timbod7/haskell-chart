import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Time.LocalTime
import System.Random

import Prices(prices1)

values :: [ (LocalTime,Double,Int,Int) ]
values = [ (d, v, z, t) | ((d,v,_),z,t) <- zip3 prices1 zs ts ]
  where
    zs     = randoms $ mkStdGen 0
    ts     = randomRs (-2,27) $ mkStdGen 1

main = toFile def "example8_big.png" $ do
  layout_title .= "Price History"
  layout_background .= solidFillStyle (opaque white)
  layout_foreground .= (opaque black)
  layout_left_axis_visibility . axis_show_ticks .= False

  plot (line "price 1" [[ (d,v) | (d,v,_) <- prices1]] )

  plot $ liftEC $ do
    area_spots_4d_title .= "random value"
    area_spots_4d_max_radius .= 20
    area_spots_4d_values .= values
