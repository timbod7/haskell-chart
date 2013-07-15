
{-# LANGUAGE FlexibleContexts #-}

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

import Diagrams.Prelude (Diagram, R2)
import qualified Diagrams.Prelude as D

import Graphics.Rendering.Chart.Backend as G
import Graphics.Rendering.Chart.Backend.Utils
import Graphics.Rendering.Chart.Geometry as G
import Graphics.Rendering.Chart.Renderable

-- -----------------------------------------------------------------------
-- Backend
-- -----------------------------------------------------------------------

-- | Run this backends renderer.
runBackend :: (D.Renderable (D.Path R2) b)
           => ChartBackendEnv   -- ^ Environment to start rendering with.
           -> ChartBackend a    -- ^ Chart render code.
           -> (Diagram b R2, a) -- ^ The diagram.
runBackend env m = compileBackend
      strokePathD fillPathD fillClipD textSizeD drawTextD 
      withTransformD withLineStyleD withFillStyleD withFontStyleD withClipRegionD
      env m

strokePathD :: (D.Renderable (D.Path R2) b)
            => ChartBackendEnv -> Path -> Diagram b R2
strokePathD = undefined

fillPathD :: (D.Renderable (D.Path R2) b)
          => ChartBackendEnv -> Path -> Diagram b R2
fillPathD = undefined

fillClipD :: (D.Renderable (D.Path R2) b)
          => ChartBackendEnv -> Diagram b R2
fillClipD = undefined

textSizeD :: (D.Renderable (D.Path R2) b)
          => ChartBackendEnv -> String -> (Diagram b R2, TextSize)
textSizeD = undefined

drawTextD :: (D.Renderable (D.Path R2) b)
          => ChartBackendEnv -> Point -> String -> Diagram b R2
drawTextD = undefined

withTransformD :: (D.Renderable (D.Path R2) b)
               => ChartBackendEnv -> Diagram b R2 -> Diagram b R2
withTransformD = undefined

withLineStyleD :: (D.Renderable (D.Path R2) b)
               => ChartBackendEnv -> Diagram b R2 -> Diagram b R2
withLineStyleD = undefined

withFillStyleD :: (D.Renderable (D.Path R2) b)
               => ChartBackendEnv -> Diagram b R2 -> Diagram b R2
withFillStyleD = undefined

withFontStyleD :: (D.Renderable (D.Path R2) b)
               => ChartBackendEnv -> Diagram b R2 -> Diagram b R2
withFontStyleD = undefined

withClipRegionD :: (D.Renderable (D.Path R2) b)
                => ChartBackendEnv -> Diagram b R2 -> Diagram b R2
withClipRegionD = undefined













