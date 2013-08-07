
import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Backend.Diagrams

import Diagrams.Core.Types ( renderDia )
import Diagrams.TwoD ( SizeSpec2D(..) )
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal

import Tests (tests)

main :: IO ()
main = (flip mapM_) tests $ \(name, w, h, draw) -> do
  render (name ++ ".png") w h draw
  

render :: FilePath -> Int -> Int -> ChartBackend a -> IO ()
render f w h m = do
  env <- defaultEnv vectorAlignmentFns
  let (d, _) = runBackend env m
  fst $ renderDia Cairo (CairoOptions f (Dims (fromIntegral w) (fromIntegral h)) PNG True) d