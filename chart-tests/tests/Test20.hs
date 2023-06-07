module Test20 where

import Text.Printf
import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class

import Utils
import Test.QuickCheck (Result(Failure))

dat :: [[Double]]
dat = [ [0.0,                 23.81232131645]
      , [83.87632543135,      0.0]
      , [0.0,                 0.0]
      , [66.22192174833207,   50.85424119528999]
      , [18.507408294149144,  29.94826136042779]
      , [271.34564215397256,  482.0060747629345]
      , [0.33308595521927825, 0.25399999403605966]
      , [8.418936013584233,   5.144029932796894]
      , [67.19053209933879,   62.55976275368557]
      , [0.20500418021076805, 0.7397264674905577]
      , [93.52395426151023,   96.01214737959424]
      , [486.5332691543843,   333.4124444949074]
      , [151.27192832718126,  317.4545157262858]
      , [42.246424931587924,  56.89305428360467]
      , [8.812241283978576,   3.0449891300138225]
      , [41.763424901388305,  23.924663084356638]
      , [50.77174917622324,   91.54897286917759]
      , [0.743806669182276,   0.14540395376496337]
      , [3.152519452338129,   3.76835741734118]
      , [55.75637240640731,   66.5350935501769]
      , [0.9546959351374888,  0.6673023316342984]
      , [58.81299411301322,   41.6766677808916]
      , [7.496126744615885,   1.4640493059283133]
      , [1.21316661759154927, 2.547769800092524232]
      , [0.97655290356243897, 0.90002335523825158]
      , [0.74607620980626329, 0.618295299567391062]
      , [33.3228047562302481, 58.28982568453356767]
      , [4.73981774972079753, 5.092283613261595367]
      ]

chart :: Renderable (LayoutPick LogValue PlotIndex PlotIndex)
chart = layoutToRenderable layout
 where
  layout =
      -- title + legend
        layout_title .~ "Sample Horizontal Log Bars"
      $ layout_title_style . font_size .~ 10
      $ layout_legend . _Just . legend_position .~ LegendAbove
      $ layout_legend . _Just . legend_margin .~ 10

      -- X
      $ layout_x_axis . laxis_style . axis_grid_style .~ solidLine 0.15 (opaque lightgrey)
      $ layout_x_axis . laxis_style . axis_label_gap .~ 3
      $ layout_x_axis . laxis_override .~ axisGridAtBigTicks
      $ layout_top_axis_visibility . axis_show_line .~ True
      $ layout_top_axis_visibility . axis_show_ticks .~ True
      $ layout_top_axis_visibility . axis_show_labels .~ True
      $ layout_bottom_axis_visibility . axis_show_ticks .~ True

      -- Y
      $ layout_y_axis . laxis_generate .~ autoIndexAxis' True alabels
      $ layout_y_axis . laxis_override .~ axisGridAtTicks
      $ layout_y_axis . laxis_reverse .~ True
      $ layout_y_axis . laxis_style . axis_grid_style .~ solidLine 0.3 (opaque lightgrey)
      $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 9
      $ layout_left_axis_visibility . axis_show_ticks .~ False

      -- data
      $ layout_plots .~ [ plotHBars bars2 ]

      $ def :: Layout LogValue PlotIndex

  bars2 = plot_bars_values_with_labels .~ addIndexes dat'
      $ plot_bars_titles .~ ["","after","before"]
      $ plot_bars_style .~ BarsStacked
      $ plot_bars_spacing .~ BarsFixGap 12 5
      $ plot_bars_item_styles .~ map (\c -> (solidFillStyle $ withOpacity c 0.7, Nothing)) [grey, red, green]
      $ plot_bars_label_bar_hanchor .~ BHA_Right
      $ plot_bars_label_bar_vanchor .~ BVA_Centre
      $ plot_bars_label_text_hanchor .~ HTA_Left
      $ plot_bars_label_text_vanchor .~ VTA_Centre
      $ plot_bars_label_offset .~ Vector 3 0
      $ plot_bars_label_style . font_slant .~ FontSlantItalic
      $ def

  dat' = map (\[a,b] -> [ (LogValue (min a b), if a == b then "0.0" else "")
                        , if a < b then
                                     let v = b - a in
                                     (LogValue v, printf "%0.2f" v)
                                   else (LogValue 0, "")
                        , if b < a then
                                     let v = a - b in
                                     (LogValue v, printf "%0.2f" (-v))
                                   else (LogValue 0, "")
                        ]) dat

  alabels =
    ["addedDataPointName", "removedDataPointName", "nullDataPointName"] ++
    map (\n -> "longDataPointName" ++ show n) (take (length dat - 3) [1..])

