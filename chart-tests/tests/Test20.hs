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
dat = [[662.2192174833207,   508.5424119528999]
      ,[18.507408294149144,  29.94826136042779]
      ,[371.34564215397256,  582.0060747629345]
      ,[0.33308595521927825, 0.25399999403605966]
      ,[8.418936013584233,   5.144029932796894]
      ,[671.9053209933879,   625.5976275368557]
      ,[0.20500418021076805, 0.7397264674905577]
      ,[93.52395426151023,   96.01214737959424]
      ,[486.5332691543843,   333.4124444949074]
      ,[151.27192832718126,  317.4545157262858]
      ,[42.246424931587924,  56.89305428360467]
      ,[8.812241283978576,   3.0449891300138225]
      ,[41.763424901388305,  23.924663084356638]
      ,[50.77174917622324,   91.54897286917759]
      ,[0.743806669182276,   0.14540395376496337]
      ,[3.152519452338129,   3.76835741734118]
      ,[557.5637240640731,   665.350935501769]
      ,[0.9546959351374888,  0.6673023316342984]
      ,[588.1299411301322,   416.766677808916]
      ,[7.496126744615885,   1.4640493059283133]]

chart :: Renderable (LayoutPick PlotIndex LogValue LogValue)
chart = layoutToRenderable layout
 where
  layout =
      -- title
        layout_title .~ "Sample Log Bars"
      $ layout_title_style . font_size .~ 10

      -- X
      $ layout_x_axis . laxis_generate .~ autoIndexTicksAxis alabels
      $ layout_x_axis . laxis_override .~ axisGridAtTicks
      $ layout_x_axis . laxis_style . axis_grid_style .~ solidLine 0.3 (opaque lightgrey)
      $ layout_bottom_axis_visibility . axis_show_ticks .~ False

      -- Y
      $ layout_y_axis . laxis_style . axis_grid_style .~ solidLine 0.15 (opaque lightgrey)
      $ layout_y_axis . laxis_override .~ axisGridAtBigTicks
      $ layout_left_axis_visibility . axis_show_ticks .~ True
      $ layout_right_axis_visibility . axis_show_line .~ True
      $ layout_right_axis_visibility . axis_show_ticks .~ True

      -- data
      $ layout_plots .~ [ plotBars bars2 ]
      $ def :: Layout PlotIndex LogValue

  bars2 = plot_bars_titles .~ ["","after","before"]
      $ plot_bars_values_with_labels .~ addIndexes dat'
      $ plot_bars_style .~ BarsStacked
      $ plot_bars_spacing .~ BarsFixGap 20 5
      $ plot_bars_item_styles .~ map (\c -> (solidFillStyle $ withOpacity c 0.7, Nothing)) [grey, red, green]
      $ def

  dat' = map (\[a,b] -> [ (LogValue (min a b), "")
                        , if a < b then
                                     let v = b - a in
                                     (LogValue v, printf "%0.2f" v)
                                   else (LogValue 0, "")
                        , if b < a then
                                     let v = a - b in
                                     (LogValue v, printf "%0.2f" (-v))
                                   else (LogValue 0, "")
                        ]) dat

  alabels = map (\n -> "#" ++ show n) $ take (length dat) [1..]

