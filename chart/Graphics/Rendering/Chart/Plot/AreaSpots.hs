-- |
-- Module     :  Graphics.Rendering.Chart.Plot.AreaSpots
-- Copyright  :  (c) Malcolm Wallace 2009
-- License    :  BSD-style (see COPYRIGHT file)
--
-- Area spots are a collection of unconnected filled circles,
-- with x,y position, and an independent z value to be represented
-- by the relative area of the spots.

{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.AreaSpots
  ( AreaSpots(..)
  , defaultAreaSpots

  , area_spots_title
  , area_spots_linethick
  , area_spots_linecolour
  , area_spots_fillcolour
  , area_spots_max_radius
  , area_spots_values

  , AreaSpots4D(..)
  , defaultAreaSpots4D

  , area_spots_4d_title
  , area_spots_4d_linethick
  , area_spots_4d_palette
  , area_spots_4d_max_radius
  , area_spots_4d_values
  ) where

import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Axis
import Data.Accessor.Template
import Data.Colour
import Data.Colour.Names
import Data.Default.Class

import Control.Monad


-- stuff that belongs in Data.Tuple
fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a

fst4 (a,_,_,_) = a
snd4 (_,a,_,_) = a
thd4 (_,_,a,_) = a
fth4 (_,_,_,a) = a


-- | A collection of unconnected spots, with x,y position, and an
--   independent z value to be represented by the area of the spot.
data AreaSpots z x y = AreaSpots
  { area_spots_title_      :: String
  , area_spots_linethick_  :: Double
  , area_spots_linecolour_ :: AlphaColour Double
  , area_spots_fillcolour_ :: Colour Double
  , area_spots_opacity_    :: Double
  , area_spots_max_radius_ :: Double	-- ^ the largest size of spot
  , area_spots_values_     :: [(x,y,z)]
  }

{-# DEPRECATED defaultAreaSpots "Use the according Data.Default instance!" #-}
defaultAreaSpots :: AreaSpots z x y
defaultAreaSpots = def

instance Default (AreaSpots z x y) where
  def = AreaSpots
    { area_spots_title_      = ""
    , area_spots_linethick_  = 0.1
    , area_spots_linecolour_ = opaque blue
    , area_spots_fillcolour_ = blue
    , area_spots_opacity_    = 0.2
    , area_spots_max_radius_ = 20  -- in pixels
    , area_spots_values_     = []
    }

instance (PlotValue z) => ToPlot (AreaSpots z) where
    toPlot p = Plot { plot_render_ = renderAreaSpots p
                    , plot_legend_ = [(area_spots_title_ p, renderSpotLegend p)]
                    , plot_all_points_ = ( map fst3 (area_spots_values_ p)
                                         , map snd3 (area_spots_values_ p) )
                    }

renderAreaSpots  :: (PlotValue z) => AreaSpots z x y -> PointMapFn x y -> ChartBackend ()
renderAreaSpots p pmap = 
    forM_ (scaleMax ((area_spots_max_radius_ p)^2)
                    (area_spots_values_ p))
          (\ (x,y,z)-> do
              let radius = sqrt z
              let psSpot = filledCircles radius $
                                                    flip withOpacity 
                                                      (area_spots_opacity_ p) $
                                                    area_spots_fillcolour_ p
              drawPoint psSpot (pmap (LValue x, LValue y))
              let psOutline = hollowCircles radius
                                                      (area_spots_linethick_ p)
                                                      (area_spots_linecolour_ p)
              drawPoint psOutline (pmap (LValue x, LValue y))
          )
  where
    scaleMax :: PlotValue z => Double -> [(x,y,z)] -> [(x,y,Double)]
    scaleMax n points = let largest  = maximum (map (toValue . thd3) points)
                            scale v  = n * toValue v / largest
                        in map (\ (x,y,z) -> (x,y, scale z)) points

renderSpotLegend :: AreaSpots z x y -> Rect -> ChartBackend ()
renderSpotLegend p r@(Rect p1 p2) = do
    let radius = min (abs (p_y p1 - p_y p2)) (abs (p_x p1 - p_x p2))
        centre = linearInterpolate p1 p2
    let psSpot    = filledCircles radius $
                                          flip withOpacity 
                                               (area_spots_opacity_ p) $
                                          area_spots_fillcolour_ p
    drawPoint psSpot centre
    let psSpot = hollowCircles radius
                                            (area_spots_linethick_ p)
                                            (area_spots_linecolour_ p)
    drawPoint psSpot centre
  where
    linearInterpolate (Point x0 y0) (Point x1 y1) =
        Point (x0 + abs(x1-x0)/2) (y0 + abs(y1-y0)/2)

-- | A collection of unconnected spots, with x,y position, an
--   independent z value to be represented by the area of the spot,
--   and in addition, a fourth variable t to be represented by a colour
--   from a given palette.  (A linear transfer function from t to palette
--   is assumed.)
data AreaSpots4D z t x y = AreaSpots4D
  { area_spots_4d_title_      :: String
  , area_spots_4d_linethick_  :: Double
  , area_spots_4d_palette_    :: [Colour Double]
  , area_spots_4d_opacity_    :: Double
  , area_spots_4d_max_radius_ :: Double	-- ^ the largest size of spot
  , area_spots_4d_values_     :: [(x,y,z,t)]
  }

{-# DEPRECATED defaultAreaSpots4D "Use the according Data.Default instance!" #-}
defaultAreaSpots4D :: AreaSpots4D z t x y
defaultAreaSpots4D = def

instance Default (AreaSpots4D z t x y) where
  def = AreaSpots4D
    { area_spots_4d_title_      = ""
    , area_spots_4d_linethick_  = 0.1
    , area_spots_4d_palette_    = [ blue, green, yellow, orange, red ]
    , area_spots_4d_opacity_    = 0.2
    , area_spots_4d_max_radius_ = 20  -- in pixels
    , area_spots_4d_values_     = []
    }

instance (PlotValue z, PlotValue t, Show t) => ToPlot (AreaSpots4D z t) where
    toPlot p = Plot { plot_render_ = renderAreaSpots4D p
                    , plot_legend_ = [ (area_spots_4d_title_ p
                                       , renderSpotLegend4D p) ]
                    , plot_all_points_ = ( map fst4 (area_spots_4d_values_ p)
                                         , map snd4 (area_spots_4d_values_ p) )
                    }

renderAreaSpots4D  :: (PlotValue z, PlotValue t, Show t) =>
                      AreaSpots4D z t x y -> PointMapFn x y -> ChartBackend ()
renderAreaSpots4D p pmap = 
    forM_ (scaleMax ((area_spots_4d_max_radius_ p)^2)
                    (length (area_spots_4d_palette_ p))
                    (area_spots_4d_values_ p))
          (\ (x,y,z,t)-> do
              let radius  = sqrt z
              let colour  = (area_spots_4d_palette_ p) !! t 
              let psSpot
                    = filledCircles radius $
                          flip withOpacity (area_spots_4d_opacity_ p) $ colour
              drawPoint psSpot (pmap (LValue x, LValue y))
              let psOutline
                    = hollowCircles radius (area_spots_4d_linethick_ p)
                                           (opaque colour)
              drawPoint psOutline (pmap (LValue x, LValue y))
          )
  where
    scaleMax :: (PlotValue z, PlotValue t, Show t) =>
                Double -> Int -> [(x,y,z,t)] -> [(x,y,Double,Int)]
    scaleMax n c points = let largest  = maximum (map (toValue . thd4) points)
                              scale v  = n * toValue v / largest
                              colVals  = map (toValue . fth4) points
                              colMin   = minimum colVals
                              colMax   = maximum colVals
                              select t = min (c-1) $ 
                                         truncate ( fromIntegral c
                                                       * (toValue t-colMin)
                                                       / (colMax-colMin))
                          in map (\ (x,y,z,t) -> (x,y, scale z, select t))
                                 points

renderSpotLegend4D :: AreaSpots4D z t x y -> Rect -> ChartBackend ()
renderSpotLegend4D p r@(Rect p1 p2) = do
    let radius = min (abs (p_y p1 - p_y p2)) (abs (p_x p1 - p_x p2))
        centre = linearInterpolate p1 p2
    let psSpot    = filledCircles radius $
                                          flip withOpacity
                                               (area_spots_4d_opacity_ p) $
                                          head $ area_spots_4d_palette_ p
    drawPoint psSpot centre
    let psOutline = hollowCircles radius
                                            (area_spots_4d_linethick_ p)
                                            (opaque $
                                             head (area_spots_4d_palette_ p))
    drawPoint psOutline centre
  where
    linearInterpolate (Point x0 y0) (Point x1 y1) =
        Point (x0 + abs(x1-x0)/2) (y0 + abs(y1-y0)/2)

-------------------------------------------------------------------------
-- Template haskell to derive Data.Accessor.Accessor
$( deriveAccessors ''AreaSpots )
$( deriveAccessors ''AreaSpots4D )
