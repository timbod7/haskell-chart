
{-# LANGUAGE FlexibleContexts #-}

-- | This module provides the interface class for 'ChartBackend's.
module Graphics.Rendering.Chart.Backend
  ( ChartBackendEnv(..)
  , defaultEnv
  
  , ChartBackend(..)
  , TextSize(..)
  
  , getTransform
  , getFillStyle, getFontStyle
  , getLineStyle, getClipRegion
  , withTransform'
  , withFillStyle', withFontStyle'
  , withLineStyle', withClipRegion'
  ) where

import Data.Colour
import Data.Default
import Data.Monoid

import Control.Monad.Reader

import Graphics.Rendering.Chart.Types 
import Graphics.Rendering.Chart.Geometry

-- -----------------------------------------------------------------------
-- Rendering Backend Environment
-- -----------------------------------------------------------------------

-- | The environment present in the 'ChartBackend'.
data ChartBackendEnv = ChartBackendEnv
  { cbePointAlignFn :: Point -> Point
  -- ^ An adjustment applied immediately prior to points
  --   being displayed in device coordinates.
  --
  --   When device coordinates correspond to pixels, a cleaner
  --   image is created if this transform rounds to the nearest
  --   pixel. With higher-resolution output, this transform can
  --   just be the identity function.

  -- | A adjustment applied immediately prior to coordinates
  --   being transformed.
  , cbeCoordAlignFn :: Point -> Point
  
  -- | The current transformation.
  , cbeTransform :: Matrix
  
  -- | The current 'FontStyle'.
  , cbeFontStyle :: FontStyle
  
  -- | The current 'FillStyle'.
  , cbeFillStyle :: FillStyle
  
  -- | The current 'LineStyle'.
  , cbeLineStyle :: LineStyle
  
  -- | The current clip region, if there is one.
  , cbeClipRegion :: Maybe Rect
  }

-- | Produce a environment with no transformation and clipping. 
--   It will use the default styles.
defaultEnv :: (Point -> Point) -- ^ The point alignment function ('cbePointAlignFn')
           -> (Point -> Point) -- ^ The coordinate alignment function ('cbeCoordAlignFn')
           -> ChartBackendEnv
defaultEnv pointAlignFn coordAlignFn = ChartBackendEnv 
  { cbePointAlignFn = pointAlignFn
  , cbeCoordAlignFn = coordAlignFn
  , cbeTransform = identity 
  , cbeFontStyle = def
  , cbeFillStyle = def
  , cbeLineStyle = def
  , cbeClipRegion = Nothing
  }

-- -----------------------------------------------------------------------
-- Rendering Backend Class
-- -----------------------------------------------------------------------

-- | A 'ChartBackend' provides the capability to render a chart somewhere.
--   
--   You have to implement all functions of the interface.
class (Monad m, MonadReader ChartBackendEnv m) => ChartBackend m where
  
  -- | Stroke the outline of the given path using the 
  --   current 'LineStyle'. This function does /not/ perform
  --   alignment operations on the path.
  strokePath :: Path -> m ()
  
  -- | Fill the given path using the current 'FillStyle'.
  --   The given path will be closed prior to filling.
  --   This function does /not/ perform
  --   alignment operations on the path.
  fillPath :: Path -> m ()
  
  -- | Fill the clip region using the current 'FillStyle'.
  fillClip :: m ()
  
  -- | Calculate a 'TextSize' object with rendering information
  --   about the given string without actually rendering it.
  textSize :: String -> m TextSize
  
  -- | Draw a single-line textual label anchored by the baseline (vertical) 
  --   left (horizontal) point. Uses the current 'FontStyle' for drawing.
  drawText :: Point -> String -> m ()
  
  -- | Apply the given transformation in this local
  --   environment when drawing. The given transformation 
  --   is applied after the current transformation. This
  --   means both are combined.
  --   
  --   Use the 'withTransform'' function to correctly update
  --   your environment when implementing this function.
  withTransform :: Matrix -> m a -> m a
  
  -- | Use the given font style in this local
  --   environment when drawing text.
  --   
  --   Use the 'withFontStyle'' function to correctly update
  --   your environment when implementing this function.
  withFontStyle :: FontStyle -> m a -> m a
  
  -- | Use the given fill style in this local
  --   environment when filling paths.
  --   
  --   Use the 'withFillStyle'' function to correctly update
  --   your environment when implementing this function.
  withFillStyle :: FillStyle -> m a -> m a
  
  -- | Use the given line style in this local
  --   environment when stroking paths.
  --   
  --   Use the 'withLineStyle'' function to correctly update
  --   your environment when implementing this function.
  withLineStyle :: LineStyle -> m a -> m a
  
  -- | Use the given clipping rectangle when drawing
  --   in this local environment. The new clipping region
  --   is intersected with the given clip region. You cannot 
  --   escape the clip!
  --   
  --   Use the 'withClipRegion'' function to correctly update
  --   your environment when implementing this function.
  withClipRegion :: Rect -> m a -> m a

-- -----------------------------------------------------------------------
-- Rendering Utility Types
-- -----------------------------------------------------------------------

-- | Text metrics returned by 'textSize'.
data TextSize = TextSize 
  { textSizeWidth    :: Double -- ^ The total width of the text.
  , textSizeAscent   :: Double -- ^ The ascent or space above the baseline.
  , textSizeDescent  :: Double -- ^ The decent or space below the baseline.
  , textSizeYBearing :: Double -- ^ The Y bearing.
  , textSizeHeight   :: Double -- ^ The total height of the text.
  }

-- -----------------------------------------------------------------------
-- Rendering Utility Functions
-- -----------------------------------------------------------------------

-- | Get the current transformation.
getTransform :: ChartBackend m => m Matrix
getTransform = liftM cbeTransform ask

-- | Get the current font style.
getFontStyle :: ChartBackend m => m FontStyle
getFontStyle = liftM cbeFontStyle ask

-- | Get the current fill style.
getFillStyle :: ChartBackend m => m FillStyle
getFillStyle = liftM cbeFillStyle ask

-- | Get the current line style.
getLineStyle :: ChartBackend m => m LineStyle
getLineStyle = liftM cbeLineStyle ask

-- | Get the current clipping region.
--   If no clipping region was set (it is an infinite plane) 
--   'Nothing' is returned.
getClipRegion :: ChartBackend m => m (Maybe Rect)
getClipRegion = liftM cbeClipRegion ask

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
  cbeClipRegion = case cbeClipRegion s of
                    Nothing -> Just clip
                    Just clip' -> case intersectRect clip clip' of
                      Nothing -> Just $ Rect (Point 0 0) (Point 0 0)
                      Just intersection -> Just intersection
  }) m
