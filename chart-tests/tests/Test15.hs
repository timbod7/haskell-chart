module Test15 where 

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Accessor
import Data.Default

import Utils

chart lo = layout1ToRenderable layout
 where
  layout = 
        layout1_title ^= "Legend Test"
      $ layout1_title_style ^: font_size ^= 10
      $ layout1_bottom_axis ^: laxis_generate ^= autoIndexAxis alabels
      $ layout1_left_axis ^: laxis_override ^= (axisGridHide.axisTicksHide)
      $ layout1_plots ^= [ Left (plotBars bars2) ]
      $ layout1_legend ^= Just lstyle
      $ def :: (ChartBackend m) => Layout1 m PlotIndex Double

  bars2 = plot_bars_titles ^= ["A","B","C","D","E","F","G","H","I","J"]
      $ plot_bars_values ^= addIndexes [[2,3,4,2,1,5,6,4,8,1,3],
                                        [7,4,5,6,2,4,4,5,7,8,9]
                                       ]
      $ plot_bars_style ^= BarsClustered
      $ plot_bars_spacing ^= BarsFixGap 30 5
      $ plot_bars_item_styles ^= map mkstyle (cycle defaultColorSeq)
      $ def

  alabels = [ "X", "Y" ]

  lstyle = legend_orientation ^= lo $ def

  btitle = ""
  mkstyle c = (solidFillStyle c, Nothing)

main = main' "test15" (chart (LORows 3))


