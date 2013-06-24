module Utils (main') where

import System.Environment(getArgs)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

mainCairo :: String -> Renderable CRender () -> [String] -> IO (PickFn ())
mainCairo name chart ["small"]  = renderableToPNGFile chart 320 240 $ name ++ "_small.png"
mainCairo name chart ["big"]    = renderableToPNGFile chart 800 600 $ name ++ "_big.png" 

main' :: String -> Renderable CRender () -> IO (PickFn ())
main' name chart = getArgs >>= mainCairo name chart