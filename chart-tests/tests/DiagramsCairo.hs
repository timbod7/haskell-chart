module DiagramsCairo where

import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Backend.Diagrams

import Diagrams.Core ( renderDia )
import Diagrams.Prelude ( dims, V2(..) )
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Graphics.Rendering.Chart.Renderable ( render, Renderable )

import System.Environment ( getArgs )

import Tests ( allTests, showTests )
import qualified Tests as T

main = do
    args <- getArgs
    main1 args

main1 :: [String] -> IO ()
main1 args = do
  -- Only load the environment once, to speed things up.
  let env = defaultEnv bitmapAlignmentFns
  let renderDiagram :: (String, (Int, Int), T.LineWidth -> Renderable ()) -> IO ()
      renderDiagram (n,(w,h),ir) = do
        let (d, _) = runBackendRenderable (fromIntegral w, fromIntegral h) env (ir 1.0)
        fst $ renderDia Cairo (CairoOptions (n ++ ".png") (dims $ V2 (fromIntegral w) (fromIntegral h)) PNG False) d
  showTests (fmap (\(x,_,_) -> x) allTests) renderDiagram
