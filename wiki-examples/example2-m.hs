import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Time.LocalTime

import Prices(prices,mkDate,filterPrices)

prices' :: [(LocalTime,Double,Double)]
prices' = filterPrices prices (mkDate 1 1 2005) (mkDate 31 12 2006)

main = toFile def "example2_big.png" $ do
    layoutlr_title .= "Price History"
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    plotLeft (line "price 1" [[ (d,v) | (d,v,_) <- prices']])
    plotRight (line "price 2" [[ (d,v) | (d,_,v) <- prices']])
