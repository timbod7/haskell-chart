 
module Grahpics.Rendering.Chart.Types
  ( 
  , LineCap(..)
  , LineJoin(..)

  , LineStyle(..)
  , FontStyle(..)
  , PointStyle(..)
  , FillStyle(..)

  , HTextAnchor(..)
  , VTextAnchor(..)
  , FontWeight(..)
  ) where

-- | The different supported line ends.
data LineCap = LineCapButt   -- ^ Just cut the line straight.
             | LineCapRound  -- ^ Make a rounded line end.
             | LineCapSquare -- ^ Make a square that ends the line.

-- | The different supported ways to join line ends.
data LineJoin = LineJoinMiter -- ^ Extends the outline until they meet each other.
              | LineJoinRound -- ^ Draw a circle fragment to connet line end.
              | LineJoinBevel -- ^ Like miter, but cuts it off if a certain 
                              --   threshold is exceeded.

-- | Abstract data type for the style of a plotted point.
--
--   The contained Cairo action draws a point in the desired
--   style, at the supplied device coordinates.
newtype PointStyle = PointStyle (Point -> CRender ())

-- | Data type for the style of a line.
data LineStyle = LineStyle {
   line_width_  :: Double,
   line_color_  :: AlphaColour Double,
   line_dashes_ :: [Double],
   line_cap_    :: LineCap,
   line_join_   :: LineJoin
}

-- | The possible slants of a font.
data FontSlant = FontSlantNormal  -- ^ Normal font style without slant.
               | FontSlantItalic  -- ^ With a slight slant.
               | FontSlantOblique -- ^ With a greater slant.

-- | The possible weights of a font.
data FontWeight = FontWeightNormal -- ^ Normal font style without weight.
                | FontWeightBold   -- ^ Bold font.

-- | Abstract data type for a fill style.
--
--   The contained Cairo action sets the required fill
--   style in the Cairo rendering state.
newtype FillStyle = FillStyleSolid { fill_colour_ :: AlphaColour Double }

-- | Data type for a font.
data FontStyle = FontStyle {
      font_name_   :: String,
      font_size_   :: Double,
      font_slant_  :: FontSlant,
      font_weight_ :: FontWeight,
      font_color_  :: AlphaColour Double
}



data HTextAnchor = HTA_Left | HTA_Centre | HTA_Right
data VTextAnchor = VTA_Top | VTA_Centre | VTA_Bottom | VTA_BaseLine





