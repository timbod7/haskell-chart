module Test9 where 

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Accessor
import Data.Default

import Utils

chart :: (ChartBackend m) => Bool -> Renderable m (Layout1Pick PlotIndex Double)
chart borders = layout1ToRenderable layout
 where
  layout = 
        layout1_title ^= "Sample Bars" ++ btitle
      $ layout1_title_style ^: font_size ^= 10
      $ layout1_bottom_axis ^: laxis_generate ^= autoIndexAxis alabels
      $ layout1_left_axis ^: laxis_override ^= (axisGridHide.axisTicksHide)
      $ layout1_plots ^= [ Left (plotBars bars2) ]
      $ def :: (ChartBackend m) => Layout1 m PlotIndex Double

  bars2 = plot_bars_titles ^= ["Cash","Equity"]
      $ plot_bars_values ^= addIndexes [[20,45],[45,30],[30,20],[70,25]]
      $ plot_bars_style ^= BarsClustered
      $ plot_bars_spacing ^= BarsFixGap 30 5
      $ plot_bars_item_styles ^= map mkstyle (cycle defaultColorSeq)
      $ def

  alabels = [ "Jun", "Jul", "Aug", "Sep", "Oct" ]

  btitle = if borders then "" else " (no borders)"
  bstyle = if borders then Just (solidLine 1.0 $ opaque black) else Nothing
  mkstyle c = (solidFillStyle c, bstyle)

main = main' "test9" (chart True)


