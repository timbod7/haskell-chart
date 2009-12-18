-- |
-- Module     :  Graphics.Rendering.Chart.AreaSpots
-- Copyright  :  (c) Malcolm Wallace 2009
-- License    :  BSD-style (see COPYRIGHT file)
--
-- Area spots are a collection of unconnected filled circles,
-- with x,y position, and an independent z value to be represented
-- by the relative area of the spots.

{-# OPTIONS_GHC -XTemplateHaskell #-}

module Graphics.Rendering.Chart.AreaSpots
  ( AreaSpots(..)
  , defaultAreaSpots
  ) where

import qualified Graphics.Rendering.Cairo as C

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Plot
import Graphics.Rendering.Chart.Axis
import Data.Accessor.Template
import Data.Colour
import Data.Colour.Names

import Control.Monad

-- stuff that belongs in Data.Tuple
fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a


-- | A collection of unconnected spots, with x,y position, and an
--   independent z value to be represented by the area of the spot.
data AreaSpots z x y = AreaSpots
  { area_spots_title_      :: String
  , area_spots_linethick_  :: Double
  , area_spots_linecolour_ :: AlphaColour Double
  , area_spots_fillcolour_ :: AlphaColour Double
  , area_spots_max_radius_ :: Double	-- ^ the largest size of spot
  , area_spots_values_     :: [(x,y,z)]
  }

defaultAreaSpots :: AreaSpots z x y
defaultAreaSpots = AreaSpots
  { area_spots_title_      = ""
  , area_spots_linethick_  = 0.1
  , area_spots_linecolour_ = opaque blue
  , area_spots_fillcolour_ = flip withOpacity 0.2 blue
  , area_spots_max_radius_ = 20  -- in pixels
  , area_spots_values_     = []
  }

instance (PlotValue z) => ToPlot (AreaSpots z) where
    toPlot p = Plot { plot_render_ = renderAreaSpots p
                    , plot_legend_ = [(area_spots_title_ p, renderSpotLegend p)]
                    , plot_all_points_ = ( map fst3 (area_spots_values_ p)
                                         , map snd3 (area_spots_values_ p) )
                    }

renderAreaSpots  :: (PlotValue z) =>
                    AreaSpots z x y -> PointMapFn x y -> CRender ()
renderAreaSpots p pmap = preserveCState $
    forM_ (scaleMax ((area_spots_max_radius_ p)^2)
                    (area_spots_values_ p))
          (\ (x,y,z)-> do
              let radius = sqrt z
              let (CairoPointStyle drawSpotAt)    = filledCircles radius
                                                      (area_spots_fillcolour_ p)
              drawSpotAt (pmap (LValue x, LValue y))
              let (CairoPointStyle drawOutlineAt) = hollowCircles radius
                                                      (area_spots_linethick_ p)
                                                      (area_spots_linecolour_ p)
              drawOutlineAt (pmap (LValue x, LValue y))
          )
  where
    scaleMax :: PlotValue z => Double -> [(x,y,z)] -> [(x,y,Double)]
    scaleMax n points = let largest  = maximum (map (toValue . thd3) points)
                            scale v  = n * toValue v / largest
                        in map (\ (x,y,z) -> (x,y, scale z)) points

renderSpotLegend :: AreaSpots z x y -> Rect -> CRender ()
renderSpotLegend p r@(Rect p1 p2) = preserveCState $ do
    let radius = min (abs (p_y p1 - p_y p2)) (abs (p_x p1 - p_x p2))
        centre = linearInterpolate p1 p2
    let (CairoPointStyle drawSpotAt)    = filledCircles radius
                                            (area_spots_fillcolour_ p)
    drawSpotAt centre
    let (CairoPointStyle drawOutlineAt) = hollowCircles radius
                                            (area_spots_linethick_ p)
                                            (area_spots_linecolour_ p)
    drawOutlineAt centre
  where
    linearInterpolate (Point x0 y0) (Point x1 y1) =
        Point (x0 + abs(x1-x0)/2) (y0 + abs(y1-y0)/2)

-------------------------------------------------------------------------
-- Template haskell to derive Data.Accessor.Accessor
$( deriveAccessors ''AreaSpots )
