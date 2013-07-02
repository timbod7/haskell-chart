
-- | This module provides the utilities to implement 'ChartBackend's.
module Graphics.Rendering.Chart.Backend.Utils
  ( withTransform'
  , withFillStyle', withFontStyle'
  , withLineStyle', withClipRegion'
  ) where

import Control.Monad.Reader

import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Backend

-- | Helper to correctly update the backends environment when 
--   implementing 'withTransform'.
withTransform' :: ChartBackend m => Matrix -> m a -> m a
withTransform' t m = local (\s -> s { cbeTransform = (cbeTransform s) * t }) m

-- | Helper to correctly update the backends environment when 
--   implementing 'withFontStyle'.
withFontStyle' :: ChartBackend m => FontStyle -> m a -> m a
withFontStyle' fs m = local (\s -> s { cbeFontStyle = fs }) m

-- | Helper to correctly update the backends environment when 
--   implementing 'withFillStyle'.
withFillStyle' :: ChartBackend m => FillStyle -> m a -> m a
withFillStyle' fs m = local (\s -> s { cbeFillStyle = fs }) m

-- | Helper to correctly update the backends environment when 
--   implementing 'withLineStyle'.
withLineStyle' :: ChartBackend m => LineStyle -> m a -> m a
withLineStyle' ls m = local (\s -> s { cbeLineStyle = ls }) m

-- | Helper to correctly update the backends environment when 
--   implementing 'withClipRegion'.
withClipRegion' :: ChartBackend m => Rect -> m a -> m a
withClipRegion' clip m = local (\s -> s { 
  cbeClipRegion = intersectRect (cbeClipRegion s) (LValue clip)
  }) m




