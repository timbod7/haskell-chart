module Drawing.Cairo where

import Control.Monad

import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Backend.Cairo

import Drawing.Tests

main :: IO ()
main = (flip mapM_) tests $ \(name, w, h, draw) -> do
  render (name ++ ".png") w h draw

render :: FilePath -> Int -> Int -> CBProgram a -> IO ()
render f w h m = void $ cBackendToFile (FileOptions (w,h) PNG) m f
