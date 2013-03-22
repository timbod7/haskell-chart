-- |
-- Module     :  Graphics.Rendering.Chart.Plot.AreaSpots
-- Copyright  :  (c) Malcolm Wallace 2009
-- License    :  BSD-style (see COPYRIGHT file)
--
-- Area spots are a collection of unconnected filled circles,
-- with x,y position, and an independent z value to be represented
-- by the relative area of the spots.

{-# OPTIONS_GHC -XTemplateHaskell #-}

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

import qualified Graphics.Rendering.Cairo as C

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Axis
import Control.Lens
import Data.Colour
import Data.Colour.Names

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
  { _area_spots_title      :: String
  , _area_spots_linethick  :: Double
  , _area_spots_linecolour :: AlphaColour Double
  , _area_spots_fillcolour :: Colour Double
  , _area_spots_opacity    :: Double
  , _area_spots_max_radius :: Double	-- ^ the largest size of spot
  , _area_spots_values     :: [(x,y,z)]
  }

defaultAreaSpots :: AreaSpots z x y
defaultAreaSpots = AreaSpots
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
                    , _plot_all_points = ( map fst3 (_area_spots_values p)
                                         , map snd3 (_area_spots_values p) )
                    }

renderAreaSpots  :: (PlotValue z) =>
                    AreaSpots z x y -> PointMapFn x y -> CRender ()
renderAreaSpots p pmap = preserveCState $
    forM_ (scaleMax ((_area_spots_max_radius p)^2)
                    (_area_spots_values p))
          (\ (x,y,z)-> do
              let radius = sqrt z
              let (CairoPointStyle drawSpotAt)    = filledCircles radius $
                                                    flip withOpacity
                                                      (_area_spots_opacity p) $
                                                    _area_spots_fillcolour p
              drawSpotAt (pmap (LValue x, LValue y))
              let (CairoPointStyle drawOutlineAt) = hollowCircles radius
                                                      (_area_spots_linethick p)
                                                      (_area_spots_linecolour p)
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
    let (CairoPointStyle drawSpotAt)    = filledCircles radius $
                                          flip withOpacity
                                               (_area_spots_opacity p) $
                                          _area_spots_fillcolour p
    drawSpotAt centre
    let (CairoPointStyle drawOutlineAt) = hollowCircles radius
                                            (_area_spots_linethick p)
                                            (_area_spots_linecolour p)
    drawOutlineAt centre
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
  , _area_spots_4d_max_radius :: Double	-- ^ the largest size of spot
  , _area_spots_4d_values     :: [(x,y,z,t)]
  }

defaultAreaSpots4D :: AreaSpots4D z t x y
defaultAreaSpots4D = AreaSpots4D
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
                    , _plot_all_points = ( map fst4 (_area_spots_4d_values p)
                                         , map snd4 (_area_spots_4d_values p) )
                    }

renderAreaSpots4D  :: (PlotValue z, PlotValue t, Show t) =>
                      AreaSpots4D z t x y -> PointMapFn x y -> CRender ()
renderAreaSpots4D p pmap = preserveCState $
    forM_ (scaleMax ((_area_spots_4d_max_radius p)^2)
                    (length (_area_spots_4d_palette p))
                    (_area_spots_4d_values p))
          (\ (x,y,z,t)-> do
              let radius  = sqrt z
              let colour  = (_area_spots_4d_palette p) !! t
              let (CairoPointStyle drawSpotAt)
                    = filledCircles radius $
                          flip withOpacity (_area_spots_4d_opacity p) $ colour
              drawSpotAt (pmap (LValue x, LValue y))
              let (CairoPointStyle drawOutlineAt)
                    = hollowCircles radius (_area_spots_4d_linethick p)
                                           (opaque colour)
              drawOutlineAt (pmap (LValue x, LValue y))
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

renderSpotLegend4D :: AreaSpots4D z t x y -> Rect -> CRender ()
renderSpotLegend4D p r@(Rect p1 p2) = preserveCState $ do
    let radius = min (abs (p_y p1 - p_y p2)) (abs (p_x p1 - p_x p2))
        centre = linearInterpolate p1 p2
    let (CairoPointStyle drawSpotAt)    = filledCircles radius $
                                          flip withOpacity
                                               (_area_spots_4d_opacity p) $
                                          head $ _area_spots_4d_palette p
    drawSpotAt centre
    let (CairoPointStyle drawOutlineAt) = hollowCircles radius
                                            (_area_spots_4d_linethick p)
                                            (opaque $
                                             head (_area_spots_4d_palette p))
    drawOutlineAt centre
  where
    linearInterpolate (Point x0 y0) (Point x1 y1) =
        Point (x0 + abs(x1-x0)/2) (y0 + abs(y1-y0)/2)

-------------------------------------------------------------------------
-- Template haskell to derive Data.Accessor.Accessor
$( makeLenses ''AreaSpots )
$( makeLenses ''AreaSpots4D )
