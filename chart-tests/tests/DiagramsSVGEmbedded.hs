
import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Backend.Diagrams

import Diagrams.Core.Types ( renderDia )
import Diagrams.TwoD ( SizeSpec2D(..) )
import Diagrams.Backend.SVG
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
  env <- defaultEnv bitmapAlignmentFns 500 500
  let renderDiagram :: (String, (Int, Int), T.OutputType -> Renderable ()) -> IO ()
      renderDiagram (n,(w,h),ir) = do
        renderableToEmbeddedFontSVGFile' (ir T.SVG) (env { envOutputSize = (fromIntegral w, fromIntegral h) }) (n ++ "-embedded.svg")
        return ()
  showTests (fmap (\(x,_,_) -> x) allTests) renderDiagram
  