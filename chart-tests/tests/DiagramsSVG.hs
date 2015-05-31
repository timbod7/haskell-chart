module DiagramsSVG where

import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Backend.Diagrams

import qualified Data.ByteString.Lazy as BS

import Graphics.Rendering.Chart.Renderable ( render, Renderable )
import Lucid.Svg ( renderBS )

import System.Environment ( getArgs )

import Tests ( allTests, showTests )
import qualified Tests as T

main = do
    args <- getArgs
    main1 args

main1 :: [String] -> IO ()
main1 args = do
    -- We don't use the renderableToFile function as we want to construct the
    -- environment once for speed
    env0 <- defaultEnv bitmapAlignmentFns 0 0
    showTests (fmap (\(x,_,_) -> x) allTests) (renderDiagram env0)
  where
    renderDiagram :: DEnv Double -> (String, (Int, Int), T.LineWidth -> Renderable ()) -> IO ()
    renderDiagram env0 (n,(w,h),ir) = do
      let cr = render (ir 0.25) (fromIntegral w, fromIntegral h)
          env = env0{ envOutputSize = (fromIntegral w, fromIntegral h) }
          (svg, _) = cBackendToSVG cr env
          path = n ++ ".svg"
      putStrLn (path ++ "...")
      BS.writeFile path (renderBS svg)
