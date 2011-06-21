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

msft = SparkLine { sparkOptions = smoothSpark { limits=(bot,top), height=40 }
                 , sparkData    = closing }
  where
    closing  = [ cl | (d,(lo,op,cl,hi)) <- reverse pricesMSFT ]
    lowpoint = [ lo | (d,(lo,op,cl,hi)) <- reverse pricesMSFT ]
    bot = minimum lowpoint
    top = maximum closing

main1 :: [String] -> IO (PickFn ())
main1 _  = sparkLineToPNG "test_sparkline.png" msft

main = getArgs >>= main1
