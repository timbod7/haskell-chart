module Test2 where

import Graphics.Rendering.Chart
import Data.Time.LocalTime
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Control.Lens
import Data.Default.Class
import Prices(prices2)

import Utils

chart :: [(LocalTime,Double,Double)] -> Bool -> Double -> Renderable (LayoutPick LocalTime Double Double)
chart prices showMinMax lwidth = layoutLRToRenderable layout
  where

    lineStyle c = line_width .~ 3 * lwidth
                $ line_color .~ c
                $ def ^. plot_lines_style

    limitLineStyle c = line_width .~ lwidth
                $ line_color .~ opaque c
                $ line_dashes .~ [5,10]
                $ def ^. plot_lines_style

    price1 = plot_lines_style .~ lineStyle (opaque blue)
           $ plot_lines_values .~ [[ (d, v) | (d,v,_) <- prices]]
           $ plot_lines_title .~ "price 1"
           $ def

    price2 = plot_lines_style .~ lineStyle (opaque green)
	   $ plot_lines_values .~ [[ (d, v) | (d,_,v) <- prices]]
           $ plot_lines_title .~ "price 2"
           $ def

    (min1,max1) = (minimum [v | (_,v,_) <- prices],maximum [v | (_,v,_) <- prices])
    (min2,max2) = (minimum [v | (_,_,v) <- prices],maximum [v | (_,_,v) <- prices])
    limits | showMinMax = [ Left $ hlinePlot "min/max" (limitLineStyle blue) min1,
                            Left $ hlinePlot "" (limitLineStyle blue) max1,
                            Right $ hlinePlot "min/max" (limitLineStyle green) min2,
                            Right $ hlinePlot "" (limitLineStyle green) max2 ]
           | otherwise  = []

    bg = opaque $ sRGB 0 0 0.25
    fg = opaque white
    fg1 = opaque $ sRGB 0.0 0.0 0.15

    layout = layoutlr_title .~"Price History"
           $ layoutlr_background .~ solidFillStyle bg
           $ updateAllAxesStylesLR (axis_grid_style .~ solidLine 1 fg1)
           $ layoutlr_left_axis . laxis_override .~ axisGridHide
           $ layoutlr_right_axis . laxis_override .~ axisGridHide
           $ layoutlr_x_axis . laxis_override .~ axisGridHide
 	   $ layoutlr_plots .~ ([Left (toPlot price1), Right (toPlot price2)] ++ limits)
           $ layoutlr_grid_last .~ False
           $ setLayoutLRForeground fg
           $ def

-- main = main' "test2" (chart prices2 True 0.25)
