
import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Backend.Diagrams

import Diagrams.Core.Types ( renderDia )
import Diagrams.TwoD ( SizeSpec2D(..) )
import Graphics.Rendering.Chart.Renderable ( render, Renderable )
import qualified Diagrams.Backend.Postscript as DEPS

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
        renderableToEPSFile' (ir T.PDF) (env { envOutputSize = (fromIntegral w, fromIntegral h) }) (n ++ ".eps")
        return ()
  showTests (fmap (\(x,_,_) -> x) allTests) renderDiagram
  