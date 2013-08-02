{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Backend.Types where

import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Control.Lens

import Graphics.Rendering.Chart.Geometry

-- -----------------------------------------------------------------------
-- Line Types
-- -----------------------------------------------------------------------

-- | The different supported line ends.
data LineCap = LineCapButt   -- ^ Just cut the line straight.
             | LineCapRound  -- ^ Make a rounded line end.
             | LineCapSquare -- ^ Make a square that ends the line.
             deriving Show

-- | The different supported ways to join line ends.
data LineJoin = LineJoinMiter -- ^ Extends the outline until they meet each other.
              | LineJoinRound -- ^ Draw a circle fragment to connet line end.
              | LineJoinBevel -- ^ Like miter, but cuts it off if a certain 
                              --   threshold is exceeded.
              deriving Show

-- | Data type for the style of a line.
data LineStyle = LineStyle 
  { _line_width  :: Double
  -- ^ The thickness of a line in device units.
  , _line_color  :: AlphaColour Double
  -- ^ The color of a line.
  , _line_dashes :: [Double]
  -- ^ The dash pattern. Every value at a even index gives a dash width and 
  --   every value at a odd index gives a gap width in device units.
  , _line_cap    :: LineCap
  -- ^ How to end a line.
  , _line_join   :: LineJoin
  -- ^ How to connect two lines.
  } deriving Show

-- | The default line style.
instance Default LineStyle where
  def = LineStyle 
    { _line_width  = 1
    , _line_color  = opaque black
    , _line_dashes = []
    , _line_cap    = LineCapButt
    , _line_join   = LineJoinBevel
    }

-- -----------------------------------------------------------------------
-- Font & Text Types
-- -----------------------------------------------------------------------

-- | The possible slants of a font.
data FontSlant = FontSlantNormal  -- ^ Normal font style without slant.
               | FontSlantItalic  -- ^ With a slight slant.
               | FontSlantOblique -- ^ With a greater slant.
               deriving Show

-- | The default font slant.
instance Default FontSlant where
  def = FontSlantNormal

-- | The possible weights of a font.
data FontWeight = FontWeightNormal -- ^ Normal font style without weight.
                | FontWeightBold   -- ^ Bold font.
                deriving Show

-- | The default font weight.
instance Default FontWeight where
  def = FontWeightNormal

-- | Data type for a font.
data FontStyle = FontStyle {
      _font_name   :: String,
      -- ^ The font family or font face to use.
      _font_size   :: Double,
      -- ^ The height of the rendered font in device coordinates.
      _font_slant  :: FontSlant,
      -- ^ The slant to render with.
      _font_weight :: FontWeight,
      -- ^ The weight to render with.
      _font_color  :: AlphaColour Double
      -- ^ The color to render text with.
} deriving Show

-- | The default font style.
instance Default FontStyle where
  def = FontStyle 
    { _font_name   = "sans-serif"
    , _font_size   = 10
    , _font_slant  = def
    , _font_weight = def
    , _font_color  = opaque black
    }

{-# DEPRECATED defaultFontStyle  "Use the according Data.Default instance!" #-}
-- | The default font style.
defaultFontStyle :: FontStyle
defaultFontStyle = def

-- | Possible horizontal anchor points for text.
data HTextAnchor = HTA_Left | HTA_Centre | HTA_Right deriving Show

-- | Possible vertical anchor points for text.
data VTextAnchor = VTA_Top | VTA_Centre | VTA_Bottom | VTA_BaseLine deriving Show

-- | Text metrics returned by 'textSize'.
data TextSize = TextSize 
  { textSizeWidth    :: Double -- ^ The total width of the text.
  , textSizeAscent   :: Double -- ^ The ascent or space above the baseline.
  , textSizeDescent  :: Double -- ^ The decent or space below the baseline.
  , textSizeYBearing :: Double -- ^ The Y bearing.
  , textSizeHeight   :: Double -- ^ The total height of the text.
  } deriving Show

-- -----------------------------------------------------------------------
-- Fill Types
-- -----------------------------------------------------------------------

-- | Abstract data type for a fill style.
--
--   The contained Cairo action sets the required fill
--   style in the Cairo rendering state.
newtype FillStyle = FillStyleSolid { _fill_colour :: AlphaColour Double } deriving Show

-- | The default fill style.
instance Default FillStyle where
  def = FillStyleSolid
    { _fill_colour = opaque white
    }

-------------------------------------------------------------------------

-- | A function to align points for a certain rendering device.
type AlignmentFn = Point -> Point

-- | Holds the point and coordinate alignment function.
data AlignmentFns = AlignmentFns {
  afPointAlignFn :: AlignmentFn,
  -- ^ An adjustment applied immediately prior to points
  --   being displayed in device coordinates.
  --
  --   When device coordinates correspond to pixels, a cleaner
  --   image is created if this transform rounds to the nearest
  --   pixel. With higher-resolution output, this transform can
  --   just be the identity function.
  --   
  --   This is usually used to align prior to stroking.

  -- | A adjustment applied immediately prior to coordinates
  --   being transformed.
  --   
  --   This is usually used to align prior to filling.
  afCoordAlignFn :: AlignmentFn
  }

-- | Alignment to render good on raster based graphics.
bitmapAlignmentFns :: AlignmentFns
bitmapAlignmentFns = AlignmentFns (adjfn 0.5) (adjfn 0.0) 
  where
    adjfn offset (Point x y) = Point (adj x) (adj y)
      where
        adj v = (fromIntegral.round) v +offset

-- | Alignment to render good on vector based graphics.
vectorAlignmentFns :: AlignmentFns
vectorAlignmentFns = AlignmentFns id id

$( makeLenses ''LineStyle )
$( makeLenses ''FontStyle )

