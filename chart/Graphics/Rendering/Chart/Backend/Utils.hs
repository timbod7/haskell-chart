
-- | This module provides the utilities to implement 'ChartBackend's.
module Graphics.Rendering.Chart.Backend.Utils
  ( runChartBackend
  ) where

import Control.Monad.Reader
import Control.Monad.Operational

import Graphics.Rendering.Chart.Backend

runChartBackend :: ChartBackendEnv 
                -> ChartBackend a 
                -> ProgramView (ChartBackendInstr ChartBackend) a
runChartBackend env m = view $ runReaderT (toProgram m) env



