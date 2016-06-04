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

  , area_spots_title
  , area_spots_linethick
  , area_spots_linecolour
  , area_spots_fillcolour
  , area_spots_opacity
  , area_spots_max_radius
  , area_spots_values

  , AreaSpots4D(..)

  , area_spots_4d_title
  , area_spots_4d_linethick
  , area_spots_4d_palette
  , area_spots_4d_opacity
  , area_spots_4d_max_radius
  , area_spots_4d_values
  ) where

import Graphics.Rendering.Chart.Geometry hiding (scale, x0, y0)
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Axis
import Control.Lens
import Data.Colour hiding (over)
import Data.Colour.Names
import Data.Default.Class

import Control.Monad

-- | A collection of unconnected spots, with x,y position, and an
--   independent z value to be represented by the area of the spot.
data AreaSpots z x y = AreaSpots
  { _area_spots_title      :: String
  , _area_spots_linethick  :: Double
  , _area_spots_linecolour :: AlphaColour Double
  , _area_spots_fillcolour :: Colour Double
  , _area_spots_opacity    :: Double
  , _area_spots_max_radius :: Double   -- ^ the largest size of spot
  , _area_spots_values     :: [(x,y,z)]
  }

instance Default (AreaSpots z x y) where
  def = AreaSpots
    { _area_spots_title      = ""
    , _area_spots_linethick  = 0.1
    , _area_spots_linecolour = opaque blue
    , _area_spots_fillcolour = blue
    , _area_spots_opacity    = 0.2
    , _area_spots_max_radius = 20  -- in pixels
    , _area_spots_values     = []
    }

instance (PlotValue z) => ToPlot (AreaSpots z) where
    toPlot p = Plot { _plot_render = renderAreaSpots p
                    , _plot_legend = [(_area_spots_title p, renderSpotLegend p)]
                    , _plot_all_points = ( map (^._1) (_area_spots_values p)
                                         , map (^._2) (_area_spots_values p) )
                    }

renderAreaSpots  :: (PlotValue z) => AreaSpots z x y -> PointMapFn x y -> BackendProgram ()
renderAreaSpots p pmap = 
    forM_ (scaleMax (_area_spots_max_radius p^(2::Integer))
                    (_area_spots_values p))
          (\ (x,y,z)-> do
              let radius = sqrt z
              let psSpot = filledCircles radius $
                                                    flip withOpacity 
                                                      (_area_spots_opacity p) $
                                                    _area_spots_fillcolour p
              drawPoint psSpot (pmap (LValue x, LValue y))
              let psOutline = hollowCircles radius
                                                      (_area_spots_linethick p)
                                                      (_area_spots_linecolour p)
              drawPoint psOutline (pmap (LValue x, LValue y))
          )
  where
    scaleMax :: PlotValue z => Double -> [(x,y,z)] -> [(x,y,Double)]
    scaleMax n points = let largest  = maximum (map (^._3.to toValue) points)
                            scale v  = n * toValue v / largest
                        in over (mapped._3) scale points

renderSpotLegend :: AreaSpots z x y -> Rect -> BackendProgram ()
renderSpotLegend p (Rect p1 p2) = do
    let radius = min (abs (p_y p1 - p_y p2)) (abs (p_x p1 - p_x p2))
        centre = linearInterpolate p1 p2
        psSpot = filledCircles radius $ withOpacity 
                                        (_area_spots_fillcolour p)
                                        (_area_spots_opacity p)
        psOutline = hollowCircles radius (_area_spots_linethick p)
                                         (_area_spots_linecolour p)
    drawPoint psSpot centre
    drawPoint psOutline centre
  where
    linearInterpolate (Point x0 y0) (Point x1 y1) =
        Point (x0 + abs(x1-x0)/2) (y0 + abs(y1-y0)/2)

-- | A collection of unconnected spots, with x,y position, an
--   independent z value to be represented by the area of the spot,
--   and in addition, a fourth variable t to be represented by a colour
--   from a given palette.  (A linear transfer function from t to palette
--   is assumed.)
data AreaSpots4D z t x y = AreaSpots4D
  { _area_spots_4d_title      :: String
  , _area_spots_4d_linethick  :: Double
  , _area_spots_4d_palette    :: [Colour Double]
  , _area_spots_4d_opacity    :: Double
  , _area_spots_4d_max_radius :: Double        -- ^ the largest size of spot
  , _area_spots_4d_values     :: [(x,y,z,t)]
  }

instance Default (AreaSpots4D z t x y) where
  def = AreaSpots4D
    { _area_spots_4d_title      = ""
    , _area_spots_4d_linethick  = 0.1
    , _area_spots_4d_palette    = [ blue, green, yellow, orange, red ]
    , _area_spots_4d_opacity    = 0.2
    , _area_spots_4d_max_radius = 20  -- in pixels
    , _area_spots_4d_values     = []
    }

instance (PlotValue z, PlotValue t, Show t) => ToPlot (AreaSpots4D z t) where
    toPlot p = Plot { _plot_render = renderAreaSpots4D p
                    , _plot_legend = [ (_area_spots_4d_title p
                                       , renderSpotLegend4D p) ]
                    , _plot_all_points = ( map (^._1) (_area_spots_4d_values p)
                                         , map (^._2) (_area_spots_4d_values p) )
                    }

renderAreaSpots4D  :: (PlotValue z, PlotValue t, Show t) =>
                      AreaSpots4D z t x y -> PointMapFn x y -> BackendProgram ()
renderAreaSpots4D p pmap = 
    forM_ (scaleMax (_area_spots_4d_max_radius p^(2::Integer))
                    (length (_area_spots_4d_palette p))
                    (_area_spots_4d_values p))
          (\ (x,y,z,t)-> do
              let radius  = sqrt z
              let colour  = _area_spots_4d_palette p !! t 
              let psSpot
                    = filledCircles radius $
                          withOpacity colour (_area_spots_4d_opacity p)
              drawPoint psSpot (pmap (LValue x, LValue y))
              let psOutline
                    = hollowCircles radius (_area_spots_4d_linethick p)
                                           (opaque colour)
              drawPoint psOutline (pmap (LValue x, LValue y))
          )
  where
    scaleMax :: (PlotValue z, PlotValue t, Show t) =>
                Double -> Int -> [(x,y,z,t)] -> [(x,y,Double,Int)]
    scaleMax n c points = let largest  = maximum (map (^._3.to toValue) points)
                              scale v  = n * toValue v / largest
                              colVals  = map (^._4.to toValue) points
                              colMin   = minimum colVals
                              colMax   = maximum colVals
                              select t = min (c-1) $ 
                                         truncate ( fromIntegral c
                                                       * (toValue t-colMin)
                                                       / (colMax-colMin))
                          in map (\ (x,y,z,t) -> (x,y, scale z, select t))
                                 points

renderSpotLegend4D :: AreaSpots4D z t x y -> Rect -> BackendProgram ()
renderSpotLegend4D p (Rect p1 p2) = do
    let radius = min (abs (p_y p1 - p_y p2)) (abs (p_x p1 - p_x p2))
        centre = linearInterpolate p1 p2
        palCol = head $ _area_spots_4d_palette p
        psSpot = filledCircles radius $ withOpacity palCol
                                        (_area_spots_4d_opacity p)
        psOutline = hollowCircles radius (_area_spots_4d_linethick p)
                                         (opaque palCol)
    drawPoint psSpot centre
    drawPoint psOutline centre
  where
    linearInterpolate (Point x0 y0) (Point x1 y1) =
        Point (x0 + abs(x1-x0)/2) (y0 + abs(y1-y0)/2)

$( makeLenses ''AreaSpots )
$( makeLenses ''AreaSpots4D )
