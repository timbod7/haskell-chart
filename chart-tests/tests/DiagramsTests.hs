module DiagramsTests where

import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Backend.Diagrams

import Control.Monad (void)

import Diagrams.Core (renderDia)
import Diagrams.Prelude (dims, V2(..))
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Graphics.Rendering.Chart.Renderable (render, Renderable)

import Tests (allTests, showTests)
import qualified Tests as T

import qualified Data.ByteString.Lazy as BS
import Lucid.Svg (renderBS)

data DiagramsBackend = BackendCairo | BackendSVG | BackendSVGEmbedded | BackendEPS

run :: DiagramsBackend -> IO ()
run backend = do
  -- Only load the environment once, to speed things up.
  env <- defaultEnvironment backend
  showTests (fmap (\(x,_,_) -> x) allTests) (go backend env)

  where
    go backend env (name, (w,h), ir) = do
      let env' = env { envOutputSize = (fromIntegral w, fromIntegral h) }
      putStrLn (name ++ "...")
      renderDiagram backend env' (name, (fromIntegral w, fromIntegral h), ir)

renderDiagram :: DiagramsBackend -> DEnv Double -> (String, (Double, Double), T.LineWidth -> Renderable ()) -> IO ()
renderDiagram BackendCairo env (name, (w,h), ir) = do
  let (diagram, _) = runBackendR env (ir 1.0)
  fst $ renderDia Cairo (CairoOptions (name ++ ".png") (dims $ V2 w h) PNG False) diagram

renderDiagram BackendEPS env (name, (w,h), ir) = do
  let cr = render (ir 0.25) (w, h)
      path = name ++ ".eps"
  void (cBackendToEPSFile cr env path)

renderDiagram BackendSVG env (name, (w,h), ir) = do
  let cr = render (ir 0.25) (w, h)
      (svg, _) = cBackendToSVG cr env
      path = name ++ ".svg"
  BS.writeFile path (renderBS svg)

renderDiagram BackendSVGEmbedded env (name, (w,h), ir) = do
  let cr = render (ir 0.25) (w, h)
      (svg, _) = cBackendToEmbeddedFontSVG cr env
      path = name ++ "-embedded.svg"
  BS.writeFile path (renderBS svg)

defaultEnvironment :: DiagramsBackend -> IO (DEnv Double)
defaultEnvironment backend = case backend of
  BackendCairo       -> defaultEnv bitmapAlignmentFns 500 500
  BackendEPS         -> defaultEnv bitmapAlignmentFns 0 0
  BackendSVG         -> defaultEnv bitmapAlignmentFns 0 0
  BackendSVGEmbedded -> defaultEnv bitmapAlignmentFns 500 500
