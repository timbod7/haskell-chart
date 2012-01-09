module Test6 where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Simple
import System.Environment(getArgs)

chart :: Renderable ()
chart = toRenderable (plotLayout pp){layout1_title_="Graphics.Rendering.Chart.Simple example"}
  where
    pp = plot xs sin "sin"
                 cos "cos" "o"
                 (sin.sin.cos) "sin.sin.cos" "."
                 (/3) "- "
                 (const 0.5)
                 [0.1,0.7,0.5::Double] "+"
    xs = [0,0.3..3] :: [Double]

main1 :: [String] -> IO (PickFn ())
main1 ["small"]  = renderableToPNGFile chart 320 240 "test6_small.png"
main1 ["big"]    = renderableToPNGFile chart 800 600 "test6_big.png"

main = getArgs >>= main1
