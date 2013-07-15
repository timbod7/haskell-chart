module Test3 where

import Graphics.Rendering.Chart
import Data.Time.LocalTime
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Accessor
import Data.Default
import Prices(prices1)

import Utils

green1 = opaque $ sRGB 0.5 1 0.5
blue1 = opaque $ sRGB 0.5 0.5 1

chart :: Renderable (Layout1Pick LocalTime Double)
chart = layout1ToRenderable layout
  where
    price1 = plot_fillbetween_style ^= solidFillStyle green1
           $ plot_fillbetween_values ^= [ (d,(0,v2)) | (d,v1,v2) <- prices1]
           $ plot_fillbetween_title ^= "price 1"
           $ def

    price2 = plot_fillbetween_style ^= solidFillStyle blue1
           $ plot_fillbetween_values ^= [ (d,(0,v1)) | (d,v1,v2) <- prices1]
           $ plot_fillbetween_title ^= "price 2"
           $ def

    layout = layout1_title ^= "Price History"
           $ layout1_grid_last ^= True
 	   $ layout1_plots ^= [Left (toPlot price1),
                               Left (toPlot price2)]
           $ def

main = main' "test3" chart
