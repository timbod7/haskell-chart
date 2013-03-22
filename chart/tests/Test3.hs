module Test3 where

import Graphics.Rendering.Chart
import Data.Time.LocalTime
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Accessor
import System.Environment(getArgs)
import Prices(prices1)

green1 = opaque $ sRGB 0.5 1 0.5
blue1 = opaque $ sRGB 0.5 0.5 1

chart = toRenderable layout
  where
    price1 = plot_fillbetween_style ^= solidFillStyle green1
           $ plot_fillbetween_values ^= [ (d,(0,v2)) | (d,v1,v2) <- prices1]
           $ plot_fillbetween_title ^= "price 1"
           $ defaultPlotFillBetween

    price2 = plot_fillbetween_style ^= solidFillStyle blue1
           $ plot_fillbetween_values ^= [ (d,(0,v1)) | (d,v1,v2) <- prices1]
           $ plot_fillbetween_title ^= "price 2"
           $ defaultPlotFillBetween

    layout = layout1_title ^= "Price History"
           $ layout1_grid_last ^= True
           $ layout1_plots ^= [Left (toPlot price1),
                               Left (toPlot price2)]
           $ defaultLayout1


main1 :: [String] -> IO (PickFn ())
main1 ["small"]  = renderableToPNGFile chart 320 240 "test3_small.png"
main1 ["big"]    = renderableToPNGFile chart 800 600 "test3_big.png"

main = getArgs >>= main1
