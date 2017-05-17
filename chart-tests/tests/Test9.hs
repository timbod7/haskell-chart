module Test9 where

import Control.Arrow
import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart

import Utils

chart :: Bool -> Renderable (LayoutPick PlotIndex Double Double)
chart borders = layoutToRenderable layout
 where
  layout =
        layout_title .~ "Sample Bars" ++ btitle
      $ layout_title_style . font_size .~ 10
      $ layout_x_axis . laxis_generate .~ autoIndexAxis alabels
      $ layout_y_axis . laxis_override .~ axisGridHide
      $ layout_left_axis_visibility . axis_show_ticks .~ False
      $ layout_plots .~ [ plotBars bars2 ]
      $ def :: Layout PlotIndex Double

  bars2 = plot_bars_titles .~ ["Cash","Equity"]
      $ plot_bars_values_with_labels .~ addLabels (addIndexes vals)
      $ plot_bars_style .~ BarsClustered
      $ plot_bars_spacing .~ BarsFixGap 30 5
      $ plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq)
      $ def

  vals = [[20,45],[45,30],[30,20],[70,25]]
  alabels = [ "Jun", "Jul", "Aug", "Sep", "Oct" ]

  btitle = if borders then "" else " (no borders)"
  bstyle = if borders then Just (solidLine 1.0 $ opaque black) else Nothing
  mkstyle c = (solidFillStyle c, bstyle)

-- main = main' "test9" (chart True)


