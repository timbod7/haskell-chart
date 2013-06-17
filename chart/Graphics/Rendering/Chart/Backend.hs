
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

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
  
  , foldPath
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
  
  runBackend :: m a -> ChartOutput a
  
  
  -- | Stroke the outline of the given path using the 
  --   current line style. This function does /not/ perform
  --   alignment operations on the path.
  strokePath :: Path -> m ()
  
  -- | Fill the given path using the current fill style.
  --   This function does /not/ perform
  --   alignment operations on the path.
  fillPath :: Path -> m ()
  
  -- | Fill the clip region using the current fill style.
  fillClip :: m ()
  
  -- | Calculate a 'TextSize' object with rendering information
  --   about the given string.
  textSize :: String -> m TextSize
  
  -- | Draw a single-line textual label anchored by the baseline (vertical) 
  --   left (horizontal) point. Uses the current 'FontStyle'.
  drawText :: Point -> String -> m ()
  
  -- | Apply the given transformation in this local
  --   environment when drawing. The given transformation 
  --   is applied to the current transformation.
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
withTransform' t m = local (\s -> s { cbeTransform = (cbeTransform s) * t }) m

withFontStyle' :: ChartBackend m => FontStyle -> m a -> m a
withFontStyle' fs m = local (\s -> s { cbeFontStyle = fs }) m

withFillStyle' :: ChartBackend m => FillStyle -> m a -> m a
withFillStyle' fs m = local (\s -> s { cbeFillStyle = fs }) m

withLineStyle' :: ChartBackend m => LineStyle -> m a -> m a
withLineStyle' ls m = local (\s -> s { cbeLineStyle = ls }) m

withClipRegion' :: ChartBackend m => Maybe Rect -> m a -> m a
withClipRegion' clip m = local (\s -> s { cbeClipRegion = clip }) m

-- | Fold the given path to a monoid structure.
foldPath :: (Monoid m)
         => (Point -> m) -- ^ MoveTo
         -> (Point -> m) -- ^ LineTo
         -> (Point -> Double -> Double -> Double -> m) -- ^ Arc
         -> (Point -> Double -> Double -> Double -> m) -- ^ ArcNeg
         -> Path -- ^ Path to fold
         -> m
foldPath moveTo lineTo arc arcNeg path = case fromPath path of 
  (p:ps) -> let curr = case p of
                  MoveTo p -> moveTo p
                  LineTo p -> lineTo p
                  Arc p r a1 a2 -> arc p r a1 a2
                  ArcNeg p r a1 a2 -> arcNeg p r a1 a2
                rest = foldPath moveTo lineTo arc arcNeg (toPath ps)
            in curr <> rest
  [] -> mempty
