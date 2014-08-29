import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Time.LocalTime

import Prices(prices1)

fillBetween title vs = liftEC $ do
  plot_fillbetween_title .= title
  color <- takeColor
  plot_fillbetween_style .= solidFillStyle color
  plot_fillbetween_values .= vs

main = toFile def "example3_big.png" $ do
    layout_title .= "Price History"
    plot (fillBetween "price 1" [ (d,(0,v2)) | (d,v1,v2) <- prices1])
    plot (fillBetween "price 2" [ (d,(0,v1)) | (d,v1,v2) <- prices1])
