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

    defaultColorSeq,
    
    solidLine,
    dashedLine,

    defaultPointStyle,
    filledCircles,
    hollowCircles,
    filledPolygon,
    hollowPolygon,
    plusses,
    exes,
    stars,
    
    solidFillStyle,

    defaultFontStyle,
    
    module Graphics.Rendering.Chart.Backend.Cairo,
    module Graphics.Rendering.Chart.Types
) where

import Data.Accessor
import Data.Accessor.Template
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Data.List (unfoldr)

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Backend.Cairo
 
-- -----------------------------------------------------------------------

defaultColorSeq :: [AlphaColour Double]
defaultColorSeq = cycle $ map opaque [blue, red, green, yellow, cyan, magenta]

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

defaultPointStyle :: PointStyle
defaultPointStyle = PointStyle (opaque white) transparent 0 1 PointShapeCircle

filledCircles :: Double             -- ^ Radius of circle.
              -> AlphaColour Double -- ^ Colour.
              -> PointStyle
filledCircles radius cl = 
  PointStyle cl transparent 0 radius PointShapeCircle

hollowCircles :: Double -- ^ Radius of circle.
              -> Double -- ^ Thickness of line.
              -> AlphaColour Double
              -> PointStyle
hollowCircles radius w cl = 
  PointStyle transparent cl w radius PointShapeCircle

hollowPolygon :: Double -- ^ Radius of circle.
              -> Double -- ^ Thickness of line.
              -> Int    -- ^ Number of vertices.
              -> Bool   -- ^ Is right-side-up?
              -> AlphaColour Double
              -> PointStyle
hollowPolygon radius w sides isrot cl = 
  PointStyle transparent cl w radius (PointShapePolygon sides isrot)

filledPolygon :: Double -- ^ Radius of circle.
              -> Int    -- ^ Number of vertices.
              -> Bool   -- ^ Is right-side-up?
              -> AlphaColour Double
              -> PointStyle
filledPolygon radius sides isrot cl = 
  PointStyle cl transparent 0 radius (PointShapePolygon sides isrot)

plusses :: Double -- ^ Radius of circle.
        -> Double -- ^ Thickness of line.
        -> AlphaColour Double
        -> PointStyle
plusses radius w cl = 
  PointStyle transparent cl w radius PointShapePlus

exes :: Double -- ^ Radius of circle.
     -> Double -- ^ Thickness of line.
     -> AlphaColour Double
     -> PointStyle
exes radius w cl =
  PointStyle transparent cl w radius PointShapeCross

stars :: Double -- ^ Radius of circle.
      -> Double -- ^ Thickness of line.
      -> AlphaColour Double
      -> PointStyle
stars radius w cl =
  PointStyle transparent cl w radius PointShapeStar

solidFillStyle :: AlphaColour Double -> FillStyle
solidFillStyle cl = FillStyleSolid cl

defaultFontStyle :: FontStyle
defaultFontStyle = FontStyle {
   font_name_   = "sans",
   font_size_   = 10,
   font_slant_  = FontSlantNormal,
   font_weight_ = FontWeightNormal,
   font_color_  = opaque black
}


