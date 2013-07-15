
-- | The backend to render charts with the diagrams library.
module Graphics.Rendering.Chart.Backend.Diagrams
  ( runBackend
  ) where

import Data.Default
import Data.Colour
import Data.Colour.SRGB
import Data.List (unfoldr)
import Data.Monoid

import Control.Monad.Reader

import qualified Diagrams.Prelude as D

import Graphics.Rendering.Chart.Backend as G
import Graphics.Rendering.Chart.Backend.Utils
import Graphics.Rendering.Chart.Geometry as G
import Graphics.Rendering.Chart.Renderable

-- -----------------------------------------------------------------------
-- Backend
-- -----------------------------------------------------------------------

-- | Run this backends renderer.
runBackend :: (Renderable (Path R2) b)
           => ChartBackendEnv -- ^ Environment to start rendering with.
           -> ChartBackend a  -- ^ Chart render code.
           -> D.Diagram b R2  -- ^ The diagram.
runBackend env m = undefined













