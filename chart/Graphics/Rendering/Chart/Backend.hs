
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides the interface class for 'ChartBackend's.
module Graphics.Rendering.Chart.Backend
  ( -- * Backend Types
    ChartBackendEnv(..)
  , ChartBackend(..)
  , TextSize(..)
  
  -- * Backend Helpers
  , defaultEnv
  , getTransform
  , getFillStyle, getFontStyle
  , getLineStyle, getClipRegion
  
  -- * Line Types
  , LineCap(..)
  , LineJoin(..)
  , LineStyle(..)
  
  , line_width
  , line_color
  , line_dashes
  , line_cap
  , line_join
  
  -- * Fill Types
  , FillStyle(..)

  -- * Font and Text Types
  , FontWeight(..)
  , FontSlant(..)
  , FontStyle(..)

  , defaultFontStyle
  
  , HTextAnchor(..)
  , VTextAnchor(..)

  , font_name
  , font_size
  , font_slant
  , font_weight
  , font_color
  ) where

import Data.Colour
import Data.Default
import Data.Monoid
import Data.Colour.Names
import Data.Accessor.Template

import Control.Monad.Reader

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
  , cbeClipRegion :: Limit Rect
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
  , cbeClipRegion = LMax
  }

-- -----------------------------------------------------------------------
-- Rendering Backend Class
-- -----------------------------------------------------------------------

-- | A 'ChartBackend' provides the capability to render a chart somewhere.
--   
--   You have to implement all functions of the interface.
--   
--   The coordinate system of the backend has its initial origin (0,0)
--   in the top left corner of the drawing plane. The x-axis points 
--   towards the top right corner and the y-axis points towards 
--   the bottom left corner. The unit used by coordinates, the font size,
--   and lengths is the always the same, but depends on the backend.
--   All angles are measured in radians.
--   
--   There are some useful utility functions in the
--   "Graphics.Rendering.Chart.Backend.Utils" module to aid implementors 
--   of backends.
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
  --   Use the 'Graphics.Rendering.Chart.Backend.Utils.withTransform'' 
  --   function to correctly update
  --   your environment when implementing this function.
  withTransform :: Matrix -> m a -> m a
  
  -- | Use the given font style in this local
  --   environment when drawing text.
  --   
  --   An implementing backend is expected to guarentee
  --   to support the following font families: @serif@, @sans-serif@ and @monospace@;
  --   
  --   If the backend is not able to find or load a given font 
  --   it is required to fall back to a custom fail-safe font
  --   and use it instead.
  --   
  --   Use the 'Graphics.Rendering.Chart.Backend.Utils.withFontStyle'' 
  --   function to correctly update
  --   your environment when implementing this function.
  withFontStyle :: FontStyle -> m a -> m a
  
  -- | Use the given fill style in this local
  --   environment when filling paths.
  --   
  --   Use the 'Graphics.Rendering.Chart.Backend.Utils.withFillStyle'' 
  --   function to correctly update
  --   your environment when implementing this function.
  withFillStyle :: FillStyle -> m a -> m a
  
  -- | Use the given line style in this local
  --   environment when stroking paths.
  --   
  --   Use the 'Graphics.Rendering.Chart.Backend.Utils.withLineStyle'' 
  --   function to correctly update
  --   your environment when implementing this function.
  withLineStyle :: LineStyle -> m a -> m a
  
  -- | Use the given clipping rectangle when drawing
  --   in this local environment. The new clipping region
  --   is intersected with the given clip region. You cannot 
  --   escape the clip!
  --   
  --   Use the 'Graphics.Rendering.Chart.Backend.Utils.withClipRegion'' 
  --   function to correctly update
  --   your environment when implementing this function.
  withClipRegion :: Rect -> m a -> m a

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
getClipRegion :: ChartBackend m => m (Limit Rect)
getClipRegion = liftM cbeClipRegion ask

-- -----------------------------------------------------------------------
-- Line Types
-- -----------------------------------------------------------------------

-- | The different supported line ends.
data LineCap = LineCapButt   -- ^ Just cut the line straight.
             | LineCapRound  -- ^ Make a rounded line end.
             | LineCapSquare -- ^ Make a square that ends the line.

-- | The different supported ways to join line ends.
data LineJoin = LineJoinMiter -- ^ Extends the outline until they meet each other.
              | LineJoinRound -- ^ Draw a circle fragment to connet line end.
              | LineJoinBevel -- ^ Like miter, but cuts it off if a certain 
                              --   threshold is exceeded.

-- | Data type for the style of a line.
data LineStyle = LineStyle {
   line_width_  :: Double,
   line_color_  :: AlphaColour Double,
   line_dashes_ :: [Double],
   line_cap_    :: LineCap,
   line_join_   :: LineJoin
}

-- | The default line style.
instance Default LineStyle where
  def = LineStyle 
    { line_width_  = 1
    , line_color_  = opaque black
    , line_dashes_ = []
    , line_cap_    = LineCapButt
    , line_join_   = LineJoinBevel
    }

-- -----------------------------------------------------------------------
-- Font & Text Types
-- -----------------------------------------------------------------------

-- | The possible slants of a font.
data FontSlant = FontSlantNormal  -- ^ Normal font style without slant.
               | FontSlantItalic  -- ^ With a slight slant.
               | FontSlantOblique -- ^ With a greater slant.

-- | The default font slant.
instance Default FontSlant where
  def = FontSlantNormal

-- | The possible weights of a font.
data FontWeight = FontWeightNormal -- ^ Normal font style without weight.
                | FontWeightBold   -- ^ Bold font.

-- | The default font weight.
instance Default FontWeight where
  def = FontWeightNormal

-- | Data type for a font.
data FontStyle = FontStyle {
      font_name_   :: String,
      font_size_   :: Double,
      font_slant_  :: FontSlant,
      font_weight_ :: FontWeight,
      font_color_  :: AlphaColour Double
}

-- | The default font style.
instance Default FontStyle where
  def = FontStyle 
    { font_name_   = "sans-serif"
    , font_size_   = 10
    , font_slant_  = def
    , font_weight_ = def
    , font_color_  = opaque black
    }

{-# DEPRECATED defaultFontStyle  "Use the according Data.Default instance!" #-}
-- | The default font style.
defaultFontStyle :: FontStyle
defaultFontStyle = def

-- | Possible horizontal anchor points for text.
data HTextAnchor = HTA_Left | HTA_Centre | HTA_Right

-- | Possible vertical anchor points for text.
data VTextAnchor = VTA_Top | VTA_Centre | VTA_Bottom | VTA_BaseLine

-- | Text metrics returned by 'textSize'.
data TextSize = TextSize 
  { textSizeWidth    :: Double -- ^ The total width of the text.
  , textSizeAscent   :: Double -- ^ The ascent or space above the baseline.
  , textSizeDescent  :: Double -- ^ The decent or space below the baseline.
  , textSizeYBearing :: Double -- ^ The Y bearing.
  , textSizeHeight   :: Double -- ^ The total height of the text.
  }

-- -----------------------------------------------------------------------
-- Fill Types
-- -----------------------------------------------------------------------

-- | Abstract data type for a fill style.
--
--   The contained Cairo action sets the required fill
--   style in the Cairo rendering state.
newtype FillStyle = FillStyleSolid { fill_colour_ :: AlphaColour Double }

-- | The default fill style.
instance Default FillStyle where
  def = FillStyleSolid
    { fill_colour_ = opaque white
    }

-- -----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.
$( deriveAccessors '' LineStyle )
$( deriveAccessors '' FontStyle )