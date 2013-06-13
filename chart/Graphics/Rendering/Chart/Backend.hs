
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Rendering.Chart.Backend
  ( CEnv(..)
  , ChartBackend(..)
  ) where

import Data.Colour

import Control.Monad.Reader

import Graphics.Rendering.Chart.Types 
import Graphics.Rendering.Chart.Geometry

-- -----------------------------------------------------------------------
-- Rendering Backend
-- -----------------------------------------------------------------------

-- | The environment present in the CRender Monad.
data CEnv = CEnv {
    -- | An adjustment applied immediately prior to points
    --   being displayed in device coordinates.
    --
    --   When device coordinates correspond to pixels, a cleaner
    --   image is created if this transform rounds to the nearest
    --   pixel. With higher-resolution output, this transform can
    --   just be the identity function.
    cenv_point_alignfn :: Point -> Point,

    -- | A adjustment applied immediately prior to coordinates
    --   being transformed.
    cenv_coord_alignfn :: Point -> Point
}

class (Monad m, MonadReader CEnv m) => ChartBackend m where
  type ChartOutput a :: *
  bNewPath :: m ()
  bClosePath :: m ()
  bMoveTo :: Point -> m ()
  bLineTo :: Point -> m ()
  bRelLineTo :: Point -> m ()
  
  bArc :: Point  -- ^ The center position
       -> Double -- ^ The radius
       -> Double -- ^ Start angle, in radians
       -> Double -- ^ End angle, in radians
       -> m ()
  bArcNegative :: Point  -- ^ The center position
               -> Double -- ^ The radius
               -> Double -- ^ Start angle, in radians
               -> Double -- ^ End angle, in radians
               -> m ()
  
  bTranslate :: Point -> m ()
  bRotate :: Double -> m ()
  
  bStroke :: m ()
  bFill :: m ()
  bFillPreserve :: m ()
  bPaint :: m ()
  
  bLocal :: m a -> m a
  
  bSetSourceColor :: AlphaColour Double -> m ()
  
  bSetFontStyle :: FontStyle -> m ()
  bSetFillStyle :: FillStyle -> m ()
  bSetLineStyle :: LineStyle -> m ()
  bSetClipRegion :: Rect -> m ()
  
  bTextSize :: String -> m RectSize
  bFontExtents :: m FontExtents
  bShowText :: String -> m ()
  
  -- | Draw a single point at the given location.
  bDrawPoint :: PointStyle -- ^ Style to use when rendering the point.
             -> Point      -- ^ Position of the point to render.
             -> m ()

  -- | Recturn the bounding rectangle for a text string positioned
  --   where it would be drawn by drawText
  bTextRect :: HTextAnchor -- ^ Horizontal text anchor.
            -> VTextAnchor -- ^ Vertical text anchor.
            -> Point       -- ^ Anchor point.
            -> String      -- ^ Text to render.
            -> m Rect
  
  -- | Draw a multiline text anchored by one of its corners
  --   or edges, with rotation.
  bDrawTextsR :: HTextAnchor -- ^ Horizontal text anchor.
              -> VTextAnchor -- ^ Vertical text anchor.
              -> Double      -- ^ Rotation angle in degrees.
              -> Point       -- ^ Anchor point to rotate around.
              -> String      -- ^ Text to render.
              -> m ()
  
  runBackend :: m a -> ChartOutput a
  
  -- | Use the given font style in this local
  --   environment when drawing text.
  withFontStyle :: FontStyle -> m a -> m a
  
  -- | Use the given fill style in this local
  --   environment when filling paths.
  withFillStyle :: FillStyle -> m a -> m a
  
  -- | Use the given line style in this local
  --   environment when stroking paths.
  withLineStyle :: LineStyle -> m a -> m a
  
  -- | Use the given clipping rectangle when drawing
  --   in this local environment.
  withClipRegion :: Rect -> m a -> m a