import System.Environment(getArgs)
import Data.Time.LocalTime

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Default.Class
import Control.Lens

import Prices(prices1)

green1 = opaque $ sRGB 0.5 1 0.5
blue1 = opaque $ sRGB 0.5 0.5 1

chart = toRenderable layout
  where
    price1 = plot_fillbetween_style .~ solidFillStyle green1
           $ plot_fillbetween_values .~ [ (d,(0,v2)) | (d,v1,v2) <- prices1]
           $ plot_fillbetween_title .~ "price 1"
           $ def

    price2 = plot_fillbetween_style .~ solidFillStyle blue1
           $ plot_fillbetween_values .~ [ (d,(0,v1)) | (d,v1,v2) <- prices1]
           $ plot_fillbetween_title .~ "price 2"
           $ def

    layout = layout1_title .~ "Price History"
           $ layout1_grid_last .~ True
 	   $ layout1_plots .~ [Left (toPlot price1),
                               Left (toPlot price2)]
           $ def


main1 ["small"]  = renderableToPNGFile chart 320 240 "example4_small.png"
main1 ["big"]    = renderableToPNGFile chart 800 600 "example4_big.png"

main = getArgs >>= main1
