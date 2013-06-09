{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Drawing
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- This module contains basic types and functions used for drawing.
--
-- Note that template haskell is used to derive accessor functions
-- (see 'Data.Accessor') for each field of the following data types:
--
--    * 'LineStyle'
--
--    * 'FontStyle'
--
-- These accessors are not shown in this API documentation.  They have
-- the same name as the field, but with the trailing underscore
-- dropped. Hence for data field f_::F in type D, they have type
--
-- @
--   f :: Data.Accessor.Accessor D F
-- @
--

module Graphics.Rendering.Chart.Drawing(
    preserveCState,

    defaultColorSeq,

    setSourceColor,
    
    LineCap(..),
    LineJoin(..),

    LineStyle(..),
    solidLine,
    dashedLine,

    FillStyle(..),
    defaultPointStyle,
    solidFillStyle,

    FontStyle(..),
    defaultFontStyle,

    PointStyle(..),

    HTextAnchor(..),
    VTextAnchor(..),
    drawText,

    CRender,
    CEnv(..),
    runCRender,
    alignp,
    alignc,

    vectorEnv,
    bitmapEnv,
    
    line_width,
    line_color,
    line_dashes,
    line_cap,
    line_join,

    font_name,
    font_size,
    font_slant,
    font_weight,
    font_color,

    FontWeight(..)
) where

import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo(FontWeight)

import Graphics.Rendering.Chart.Backend.Cairo
                                
import Control.Monad.Reader
import Data.Accessor
import Data.Accessor.Template
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Data.List (unfoldr)

import Graphics.Rendering.Chart.Geometry

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

-- | The reader monad containing context information to control
--   the rendering process.
newtype CRender a = DR (ReaderT CEnv C.Render a)
  deriving (Functor, Monad, MonadReader CEnv)

runCRender :: CRender a -> CEnv -> C.Render a
runCRender (DR m) e = runReaderT m e

c :: C.Render a -> CRender a
c = DR . lift
 
----------------------------------------------------------------------

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
newtype FillStyle = FillStyle (CRender ())

-- | Data type for a font.
data FontStyle = FontStyle {
      font_name_   :: String,
      font_size_   :: Double,
      font_slant_  :: FontSlant,
      font_weight_ :: FontWeight,
      font_color_  :: AlphaColour Double
}

defaultColorSeq :: [AlphaColour Double]
defaultColorSeq = cycle $ map opaque [blue, red, green, yellow, cyan, magenta]

alignp :: Point -> CRender Point
alignp p = do 
    alignfn <- fmap cenv_point_alignfn ask
    return (alignfn p)

alignc :: Point -> CRender Point
alignc p = do 
    alignfn <- fmap cenv_coord_alignfn ask
    return (alignfn p)

data HTextAnchor = HTA_Left | HTA_Centre | HTA_Right
data VTextAnchor = VTA_Top | VTA_Centre | VTA_Bottom | VTA_BaseLine

-- | Function to draw a textual label anchored by one of its corners
--   or edges.
drawText :: HTextAnchor -> VTextAnchor -> Point -> String -> CRender ()
drawText hta vta p s = drawTextR hta vta 0 p s

----------------------------------------------------------------------

solidLine ::
     Double -- ^ Width of line.
  -> AlphaColour Double
  -> LineStyle
solidLine w cl = LineStyle w cl [] LineCapButt LineJoinMiter

dashedLine ::
     Double   -- ^ Width of line.
  -> [Double] -- ^ The dash pattern in device coordinates.
  -> AlphaColour Double
  -> LineStyle
dashedLine w ds cl = LineStyle w cl ds LineCapButt LineJoinMiter

solidFillStyle ::
     AlphaColour Double
  -> FillStyle
solidFillStyle cl = FillStyle fn
   where fn = c $ setSourceColor cl

defaultPointStyle :: PointStyle
defaultPointStyle = filledCircles 1 $ opaque white

defaultFontStyle :: FontStyle
defaultFontStyle = FontStyle {
   font_name_   = "sans",
   font_size_   = 10,
   font_slant_  = FontSlantNormal,
   font_weight_ = FontWeightNormal,
   font_color_  = opaque black
}

bitmapEnv :: CEnv
bitmapEnv = CEnv (adjfn 0.5) (adjfn 0.0)
  where
    adjfn offset (Point x y) = Point (adj x) (adj y)
      where
        adj v = (fromIntegral.round) v +offset

vectorEnv :: CEnv
vectorEnv = CEnv id id

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.
$( deriveAccessors '' LineStyle )
$( deriveAccessors '' FontStyle )

