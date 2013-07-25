
import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Backend.Cairo 

import Tests

main :: IO ()
main = (flip mapM_) tests $ \(name, w, h, draw) -> do
  render (name ++ ".png") w h draw

render :: FilePath -> Int -> Int -> ChartBackend a -> IO ()
render f w h m = renderToFile m CairoPNG w h f