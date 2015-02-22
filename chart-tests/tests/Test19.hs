module Test19 where

import Graphics.Rendering.Chart.Easy
import Control.Lens
import System.Random
import Data.Default.Class

samples :: [Double]
samples = zipWith (+) raw (tail raw)
  where
    raw = take 2000 (randomRs (0,50) (mkStdGen 0))

chart = toRenderable $ do
  plot $ fmap histToPlot $ liftEC $ do
      plot_hist_title .= "Demo histogram"
      plot_hist_bins .= 100
      plot_hist_values .= samples
      plot_hist_norm_func .= const id
  





