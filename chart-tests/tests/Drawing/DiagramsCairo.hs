module Drawing.DiagramsCairo where

import Control.Monad(forM_)

import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Backend.Diagrams

import Diagrams.Core ( renderDia )
import Diagrams.Prelude ( dims, V2(..) )
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal

import Drawing.Tests (tests)

main :: IO ()
main = do
  let fonts = commonFonts
  forM_ tests $ \(name, w, h, draw) -> do
    render fonts (name ++ ".png") w h draw


render :: FontSelector Double -> FilePath -> Int -> Int -> ChartBackend a -> IO ()
render fonts f w h m = do
  let env = createEnv bitmapAlignmentFns fonts
  let (d, _) = runBackend (fromIntegral w, fromIntegral h) env m
  fst $ renderDia Cairo (CairoOptions f (dims $ V2 (fromIntegral w) (fromIntegral h)) PNG False) d
