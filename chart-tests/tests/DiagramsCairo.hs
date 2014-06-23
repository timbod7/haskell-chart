
import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Backend.Diagrams

import Diagrams.Core ( renderDia )
import Diagrams.TwoD ( SizeSpec2D(..) )
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
  env <- defaultEnv bitmapAlignmentFns 500 500
  let renderDiagram :: (String, (Int, Int), T.LineWidth -> Renderable ()) -> IO ()
      renderDiagram (n,(w,h),ir) = do
        let env' = env { envOutputSize = (fromIntegral w, fromIntegral h) }
            (d, _) = runBackendR env' (ir 1.0)
        fst $ renderDia Cairo (CairoOptions (n ++ ".png") (Dims (fromIntegral w) (fromIntegral h)) PNG False) d
  showTests (fmap (\(x,_,_) -> x) allTests) renderDiagram
  
