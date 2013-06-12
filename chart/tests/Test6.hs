module Test6 where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Simple

import Utils

chart :: (ChartBackend m) => Renderable m ()
chart = layout1ToRenderable (plotLayout pp) {layout1_title_="Graphics.Rendering.Chart.Simple example"}
  where
    pp = plot xs sin "sin"
                 cos "cos" "o"
                 (sin.sin.cos) "sin.sin.cos" "."
                 (/3) "- "
                 (const 0.5)
                 [0.1,0.7,0.5::Double] "+"
    xs = [0,0.3..3] :: [Double]

main = main' "test6" chart
