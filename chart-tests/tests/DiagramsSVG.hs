
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
  env <- defaultEnv bitmapAlignmentFns
  let renderDiagram :: (String, (Int, Int), T.OutputType -> Renderable ()) -> IO ()
      renderDiagram (n,(w,h),ir) = do
        let (d, _) = runBackendR env (ir T.PNG) (fromIntegral w) (fromIntegral h)
        renderSVG (n ++ ".svg") (Dims (fromIntegral w) (fromIntegral h))  d
  showTests (fmap (\(x,_,_) -> x) allTests) renderDiagram
  