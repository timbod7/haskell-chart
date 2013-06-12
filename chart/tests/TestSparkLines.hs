module TestSparkLines where 

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.SparkLine
import Data.Colour
import Data.Colour.Names
import Data.Accessor
import System.Random
import System.Environment(getArgs)
import ExampleStocks

-- demonstrate SparkLine

msft = SparkLine { sl_options = smoothSpark { so_limits=(bot,top), so_height=40 }
                 , sl_data    = closing }
  where
    closing  = [ cl | (d,(lo,op,cl,hi)) <- reverse pricesMSFT ]
    lowpoint = [ lo | (d,(lo,op,cl,hi)) <- reverse pricesMSFT ]
    bot = minimum lowpoint
    top = maximum closing

chart :: (ChartBackend m) => Renderable m ()
chart = sparkLineToRenderable msft
chartSize = sparkSize msft

main1 :: [String] -> IO (PickFn ())
main1 _  = sparkLineToPNG "test_sparkline.png" msft

main = getArgs >>= main1
