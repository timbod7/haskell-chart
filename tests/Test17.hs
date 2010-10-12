module Test17 where 

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Colour
import Data.Colour.Names
import Data.Accessor
import System.Random
import System.Environment(getArgs)
import ExampleStocks

-- demonstrate Candles

chart :: Double -> Renderable ()
chart lwidth = toRenderable layout
  where
    layout = layout1_title ^="Stock Prices"
           $ layout1_background ^= solidFillStyle (opaque white)
           $ layout1_left_axis ^: laxis_override ^= axisTicksHide
 	   $ layout1_plots ^= [ Right (toPlot msftArea)
                              , Right (toPlot msftLine)
                              , Right (toPlot msftCandle)
                              , Left  (toPlot aaplArea)
                              , Left  (toPlot aaplLine)
                              , Left  (toPlot aaplCandle) ]
           $ setLayout1Foreground (opaque black)
           $ defaultLayout1

    aaplLine = plot_lines_style  ^= lineStyle 2 green
             $ plot_lines_values ^= [[ (d, cl)
                                     | (d,(lo,op,cl,hi)) <- pricesAAPL]]
             $ plot_lines_title  ^= "AAPL closing"
             $ defaultPlotLines

    msftLine = plot_lines_style  ^= lineStyle 2 purple
             $ plot_lines_values ^= [[ (d, cl)
                                     | (d,(lo,op,cl,hi)) <- pricesMSFT]]
             $ plot_lines_title  ^= "MSFT closing"
             $ defaultPlotLines

    aaplArea = plot_fillbetween_style  ^= solidFillStyle (withOpacity green 0.4)
             $ plot_fillbetween_values ^= [ (d, (lo,hi))
                                          | (d,(lo,op,cl,hi)) <- pricesAAPL]
             $ plot_fillbetween_title  ^= "AAPL spread"
             $ defaultPlotFillBetween

    msftArea = plot_fillbetween_style ^= solidFillStyle (withOpacity purple 0.4)
             $ plot_fillbetween_values ^= [ (d, (lo,hi))
                                          | (d,(lo,op,cl,hi)) <- pricesMSFT]
             $ plot_fillbetween_title  ^= "MSFT spread"
             $ defaultPlotFillBetween

    aaplCandle = plot_candle_line_style  ^= lineStyle 1 blue
               $ plot_candle_fill        ^= True
               $ plot_candle_tick_length ^= 0
               $ plot_candle_width       ^= 2
               $ plot_candle_values      ^= [ Candle d lo op 0 cl hi
                                            | (d,(lo,op,cl,hi)) <- pricesAAPL]
               $ plot_candle_title       ^= "AAPL candle"
               $ defaultPlotCandle

    msftCandle = plot_candle_line_style  ^= lineStyle 1 red
               $ plot_candle_fill        ^= True
               $ plot_candle_rise_fill_style ^= solidFillStyle (opaque pink)
               $ plot_candle_fall_fill_style ^= solidFillStyle (opaque red)
               $ plot_candle_tick_length ^= 0
               $ plot_candle_width       ^= 2
               $ plot_candle_values      ^= [ Candle d lo op 0 cl hi
                                            | (d,(lo,op,cl,hi)) <- pricesMSFT]
               $ plot_candle_title       ^= "MSFT candle"
               $ defaultPlotCandle

    lineStyle n colour = line_width ^= n * lwidth
                       $ line_color ^= opaque colour
                       $ defaultPlotLines ^. plot_lines_style

main1 :: [String] -> IO (PickFn ())
main1 ["small"]  = renderableToPNGFile (chart 0.25) 320 240 "test17_small.png"
main1 ["big"]    = renderableToPNGFile (chart 0.25) 800 600 "test17_big.png"
main1 _          = renderableToWindow  (chart 1.00) 640 480 >> return undefined

main = getArgs >>= main1
