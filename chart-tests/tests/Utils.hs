module Utils (main') where

import System.Environment(getArgs)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

mainCairo :: String -> Renderable a -> [String] -> IO (PickFn a)
mainCairo name chart ["small"]  = renderableToPNGFile chart 320 240 $ name ++ "_small.png"
mainCairo name chart ["big"]    = renderableToPNGFile chart 800 600 $ name ++ "_big.png" 

main' :: String -> Renderable a -> IO (PickFn a)
main' name chart = getArgs >>= mainCairo name chart