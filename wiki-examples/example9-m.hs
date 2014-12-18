import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Time.LocalTime

import ExampleStocks(pricesAAPL,pricesMSFT)

lineStyle n colour = line_width .~ n
                   $ line_color .~ opaque colour
                   $ def

fbetween label color vals = liftEC $ do
  plot_fillbetween_style .= solidFillStyle (withOpacity color 0.4)
  plot_fillbetween_values .= [ (d, (lo,hi)) | (d,(lo,op,cl,hi)) <- vals]
  plot_fillbetween_title .= label

cline label color vals = liftEC $ do
  plot_lines_style .= lineStyle 2 color
  plot_lines_values .= [[ (d, cl) | (d,(lo,op,cl,hi)) <- vals]]
  plot_lines_title  .= label

candle label color vals = liftEC $ do
  plot_candle_line_style  .= lineStyle 1 color
  plot_candle_fill .= True
  plot_candle_rise_fill_style .= solidFillStyle (opaque white)
  plot_candle_fall_fill_style .= solidFillStyle (opaque color)
  plot_candle_tick_length .= 0
  plot_candle_width .= 2
  plot_candle_values .= [ Candle d lo op 0 cl hi | (d,(lo,op,cl,hi)) <- vals]
  plot_candle_title .= label

main = toFile def "example9_big.png" $ do
  layoutlr_title .= "Stock Prices"
  layoutlr_left_axis . laxis_override .= axisGridHide
  layoutlr_right_axis . laxis_override .= axisGridHide

  plotLeft (fbetween "AAPL spread" green pricesAAPL)
  plotLeft (cline "AAPL closing" green pricesAAPL)
  plotLeft (candle "AAPL candle" blue pricesAAPL)

  plotRight (fbetween "MSFT spread" purple pricesMSFT)
  plotRight (cline "MSFT closing" purple pricesMSFT)
  plotRight (candle "MSFT candle" red pricesMSFT)
