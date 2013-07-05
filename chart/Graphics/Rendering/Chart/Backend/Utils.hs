
-- | This module provides the utilities to implement 'ChartBackend's.
module Graphics.Rendering.Chart.Backend.Utils
  ( runChartBackend
  ) where

import Control.Monad.Reader
import Control.Monad.Operational

import Graphics.Rendering.Chart.Backend

runChartBackend :: ChartBackendEnv 
                -> ChartBackend a 
                -> ProgramViewT (ChartBackendInstr ChartBackend) (Reader ChartBackendEnv) a
runChartBackend env m = runReader (viewT $ toProgram m) env



