module Drawing.DiagramsCairo where

import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Backend.Diagrams

import Diagrams.Core ( renderDia )
import Diagrams.TwoD.Size( dims2D )
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal

import Drawing.Tests (tests)

main :: IO ()
main = (flip mapM_) tests $ \(name, w, h, draw) -> do
  render (name ++ ".png") w h draw


render :: FilePath -> Int -> Int -> ChartBackend a -> IO ()
render f w h m = do
  env <- defaultEnv bitmapAlignmentFns (fromIntegral w) (fromIntegral h)
  let (d, _) = runBackend env m
  fst $ renderDia Cairo (CairoOptions f (dims2D (fromIntegral w) (fromIntegral h)) PNG False) d
