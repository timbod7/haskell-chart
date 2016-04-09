module DiagramsSVG where

import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Backend.Diagrams
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Backend.Diagrams

import qualified Data.ByteString.Lazy as BS

import Diagrams.Core ( renderDia )
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
    -- We don't use the renderableToFile function as we want to construct the
    -- environment once for speed
    env0 <- defaultEnv bitmapAlignmentFns
    showTests (fmap (\(x,_,_) -> x) allTests) (renderDiagram env0)
  where
    renderDiagram :: DEnv Double -> (String, (Int, Int), T.LineWidth -> Renderable ()) -> IO ()
    renderDiagram env (name, (w,h), ir) = do
      let path = name ++ ".eps"
      renderableToFile (FileOptions (fromIntegral w, fromIntegral h) Backend.Diagrams.SVG loadSansSerifFonts) path (ir 0.25)
      putStrLn (path ++ "...")
