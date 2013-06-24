
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}

module Graphics.Rendering.Chart.Types
  ( Path
  , lineTo, moveTo
  , lineTo', moveTo'
  , arc, arcNeg
  , close
  
  , foldPath
  
  , LineCap(..)
  , LineJoin(..)
  , LineStyle(..)
  
  , FontWeight(..)
  , FontSlant(..)
  , FontStyle(..)
  
  , PointShape(..)
  , PointStyle(..)
  
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
-- Path Types
-- -----------------------------------------------------------------------

data Path = MoveTo Point Path 
          | LineTo Point Path
          | Arc Point Double Double Double Path
          | ArcNeg Point Double Double Double Path
          | End 
          | Close

instance Monoid Path where
  mappend p1 p2 = case p1 of
    MoveTo p path -> MoveTo p $ mappend path p2
    LineTo p path -> LineTo p $ mappend path p2
    Arc    p r a1 a2 path -> Arc p r a1 a2 $ mappend path p2
    ArcNeg p r a1 a2 path -> ArcNeg p r a1 a2 $ mappend path p2
    End   -> p2
    Close -> Close
  mempty = End

moveTo :: Point -> Path
moveTo p = MoveTo p mempty

moveTo' :: Double -> Double -> Path
moveTo' x y = moveTo $ Point x y

lineTo :: Point -> Path
lineTo p = LineTo p mempty

lineTo' :: Double -> Double -> Path
lineTo' x y = lineTo $ Point x y

arc :: Point -> Double -> Double -> Double -> Path
arc p r a1 a2 = Arc p r a1 a2 mempty

arcNeg :: Point -> Double -> Double -> Double -> Path
arcNeg p r a1 a2 = ArcNeg p r a1 a2 mempty

close :: Path
close = Close

-- | Fold the given path to a monoid structure.
foldPath :: (Monoid m)
         => (Point -> m) -- ^ MoveTo
         -> (Point -> m) -- ^ LineTo
         -> (Point -> Double -> Double -> Double -> m) -- ^ Arc
         -> (Point -> Double -> Double -> Double -> m) -- ^ ArcNeg
         -> m    -- ^ Close
         -> Path -- ^ Path to fold
         -> m
foldPath moveTo lineTo arc arcNeg close path = 
  let restF = foldPath moveTo lineTo arc arcNeg close
  in case path of 
    MoveTo p rest -> moveTo p <> restF rest
    LineTo p rest -> lineTo p <> restF rest
    Arc    p r a1 a2 rest -> arc    p r a1 a2 <> restF rest
    ArcNeg p r a1 a2 rest -> arcNeg p r a1 a2 <> restF rest
    End   -> mempty
    Close -> close

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
-- Point Types
-- -----------------------------------------------------------------------

data PointShape = PointShapeCircle
                | PointShapePolygon Int Bool -- ^ Number of vertices and is right-side-up?
                | PointShapePlus
                | PointShapeCross
                | PointShapeStar

-- | Abstract data type for the style of a plotted point.
--
--   The contained Cairo action draws a point in the desired
--   style, at the supplied device coordinates.
data PointStyle = PointStyle 
  { point_color_ :: AlphaColour Double
  , point_border_color_ :: AlphaColour Double
  , point_border_width_ :: Double
  , point_radius_ :: Double
  , point_shape_ :: PointShape
  }

instance Default PointStyle where
  def = PointStyle 
    { point_color_        = opaque black
    , point_border_color_ = transparent
    , point_border_width_ = 0
    , point_radius_       = 1
    , point_shape_        = PointShapeCircle
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

