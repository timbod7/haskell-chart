
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Rendering.Chart.Backend
  ( ChartBackendEnv(..)
  , defaultEnv
  
  , ChartBackend(..)
  , getTransform
  , getFillStyle, getFontStyle
  , getLineStyle, getClipRegion
  , withTransform'
  , withFillStyle', withFontStyle'
  , withLineStyle', withClipRegion'
  ) where

import Data.Colour
import Data.Default

import Control.Monad.Reader

import Graphics.Rendering.Chart.Types 
import Graphics.Rendering.Chart.Geometry

-- -----------------------------------------------------------------------
-- Rendering Backend Environment
-- -----------------------------------------------------------------------

-- | The environment present in the CRender Monad.
data ChartBackendEnv = ChartBackendEnv
  -- | An adjustment applied immediately prior to points
  --   being displayed in device coordinates.
  --
  --   When device coordinates correspond to pixels, a cleaner
  --   image is created if this transform rounds to the nearest
  --   pixel. With higher-resolution output, this transform can
  --   just be the identity function.
  { cbePointAlignFn :: Point -> Point

  -- | A adjustment applied immediately prior to coordinates
  --   being transformed.
  , cbeCoordAlignFn :: Point -> Point
  
  , cbeTransform :: Matrix
  , cbeFontStyle :: FontStyle
  , cbeFillStyle :: FillStyle
  , cbeLineStyle :: LineStyle
  , cbeClipRegion :: Maybe Rect
  }

defaultEnv :: (Point -> Point) -> (Point -> Point) -> ChartBackendEnv
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

class (Monad m, MonadReader ChartBackendEnv m) => ChartBackend m where
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
  
  -- | Use the given transformation in this local
  --   environment when drawing.
  withTransform :: Matrix -> m a -> m a
  
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
  withClipRegion :: Maybe Rect -> m a -> m a

-- -----------------------------------------------------------------------
-- Rendering Utility Types
-- -----------------------------------------------------------------------

data TextSize = TextSize 
  { textSizeWidth     :: Double
  , textSizeAscent    :: Double
  , textSizeDescent   :: Double
  , textSizeLinespace :: Double
  }

-- -----------------------------------------------------------------------
-- Rendering Utility Functions
-- -----------------------------------------------------------------------

getTransform :: ChartBackend m => m Matrix
getTransform = liftM cbeTransform ask

getFontStyle :: ChartBackend m => m FontStyle
getFontStyle = liftM cbeFontStyle ask

getFillStyle :: ChartBackend m => m FillStyle
getFillStyle = liftM cbeFillStyle ask

getLineStyle :: ChartBackend m => m LineStyle
getLineStyle = liftM cbeLineStyle ask

getClipRegion :: ChartBackend m => m (Maybe Rect)
getClipRegion = liftM cbeClipRegion ask

withTransform' :: ChartBackend m => Matrix -> m a -> m a
withTransform' t m = local (\s -> s { cbeTransform = t }) m

withFontStyle' :: ChartBackend m => FontStyle -> m a -> m a
withFontStyle' fs m = local (\s -> s { cbeFontStyle = fs }) m

withFillStyle' :: ChartBackend m => FillStyle -> m a -> m a
withFillStyle' fs m = local (\s -> s { cbeFillStyle = fs }) m

withLineStyle' :: ChartBackend m => LineStyle -> m a -> m a
withLineStyle' ls m = local (\s -> s { cbeLineStyle = ls }) m

withClipRegion' :: ChartBackend m => Maybe Rect -> m a -> m a
withClipRegion' clip m = local (\s -> s { cbeClipRegion = clip }) m

