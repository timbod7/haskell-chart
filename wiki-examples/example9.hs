import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class
import System.Random
import System.Environment(getArgs)
import ExampleStocks

-- demonstrate Candles

chart = toRenderable layout
  where
    layout = layoutlr_title .~"Stock Prices"
           $ layoutlr_background .~ solidFillStyle (opaque white)
           $ layoutlr_left_axis_visibility . axis_show_ticks .~ False
           $ layoutlr_plots .~ [ Right (toPlot msftArea)
                              , Right (toPlot msftLine)
                              , Right (toPlot msftCandle)
                              , Left  (toPlot aaplArea)
                              , Left  (toPlot aaplLine)
                              , Left  (toPlot aaplCandle) ]
           $ setLayoutLRForeground (opaque black)
           $ def

    aaplLine = plot_lines_style  .~ lineStyle 2 green
             $ plot_lines_values .~ [[ (d, cl)
                                     | (d,(lo,op,cl,hi)) <- pricesAAPL]]
             $ plot_lines_title  .~ "AAPL closing"
             $ def

    msftLine = plot_lines_style  .~ lineStyle 2 purple
             $ plot_lines_values .~ [[ (d, cl)
                                     | (d,(lo,op,cl,hi)) <- pricesMSFT]]
             $ plot_lines_title  .~ "MSFT closing"
             $ def

    aaplArea = plot_fillbetween_style  .~ solidFillStyle (withOpacity green 0.4)
             $ plot_fillbetween_values .~ [ (d, (lo,hi))
                                          | (d,(lo,op,cl,hi)) <- pricesAAPL]
             $ plot_fillbetween_title  .~ "AAPL spread"
             $ def

    msftArea = plot_fillbetween_style .~ solidFillStyle (withOpacity purple 0.4)
             $ plot_fillbetween_values .~ [ (d, (lo,hi))
                                          | (d,(lo,op,cl,hi)) <- pricesMSFT]
             $ plot_fillbetween_title  .~ "MSFT spread"
             $ def

    aaplCandle = plot_candle_line_style  .~ lineStyle 1 blue
               $ plot_candle_fill        .~ True
               $ plot_candle_tick_length .~ 0
               $ plot_candle_width       .~ 2
               $ plot_candle_values      .~ [ Candle d lo op 0 cl hi
                                            | (d,(lo,op,cl,hi)) <- pricesAAPL]
               $ plot_candle_title       .~ "AAPL candle"
               $ def

    msftCandle = plot_candle_line_style  .~ lineStyle 1 red
               $ plot_candle_fill        .~ True
               $ plot_candle_rise_fill_style .~ solidFillStyle (opaque pink)
               $ plot_candle_fall_fill_style .~ solidFillStyle (opaque red)
               $ plot_candle_tick_length .~ 0
               $ plot_candle_width       .~ 2
               $ plot_candle_values      .~ [ Candle d lo op 0 cl hi
                                            | (d,(lo,op,cl,hi)) <- pricesMSFT]
               $ plot_candle_title       .~ "MSFT candle"
               $ def

    lineStyle n colour = line_width .~ n
                       $ line_color .~ opaque colour
                       $ def

main = renderableToFile def chart "example9_big.png"
