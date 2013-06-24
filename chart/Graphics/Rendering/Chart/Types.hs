
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}

module Graphics.Rendering.Chart.Types
  ( LineCap(..)
  , LineJoin(..)
  , LineStyle(..)
  
  , FontWeight(..)
  , FontSlant(..)
  , FontStyle(..)
  
  , FillStyle(..)

  , HTextAnchor(..)
  , VTextAnchor(..)
  
  , line_width
  , line_color
  , line_dashes
  , line_cap
  , line_join

  , font_name
  , font_size
  , font_slant
  , font_weight
  , font_color
  ) where

import Data.Colour
import Data.Colour.Names
import Data.Default
import Data.Accessor
import Data.Accessor.Template
import Data.Monoid

import Graphics.Rendering.Chart.Geometry

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

instance Default FontSlant where
  def = FontSlantNormal

-- | The possible weights of a font.
data FontWeight = FontWeightNormal -- ^ Normal font style without weight.
                | FontWeightBold   -- ^ Bold font.

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

instance Default FontStyle where
  def = FontStyle 
    { font_name_   = "sans"
    , font_size_   = 10
    , font_slant_  = def
    , font_weight_ = def
    , font_color_  = opaque black
    }

data HTextAnchor = HTA_Left | HTA_Centre | HTA_Right
data VTextAnchor = VTA_Top | VTA_Centre | VTA_Bottom | VTA_BaseLine

-- -----------------------------------------------------------------------
-- Fill Types
-- -----------------------------------------------------------------------

-- | Abstract data type for a fill style.
--
--   The contained Cairo action sets the required fill
--   style in the Cairo rendering state.
newtype FillStyle = FillStyleSolid { fill_colour_ :: AlphaColour Double }

instance Default FillStyle where
  def = FillStyleSolid
    { fill_colour_ = opaque white
    }

-- -----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.
$( deriveAccessors '' LineStyle )
$( deriveAccessors '' FontStyle )

