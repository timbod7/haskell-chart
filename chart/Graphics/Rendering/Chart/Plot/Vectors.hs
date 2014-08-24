-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Vectors
-- Copyright   :  (c) Anton Vorontsov <anton@enomsg.org> 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Vector plots
--
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Vectors(
    PlotVectors(..),
    VectorStyle(..),
    plotVectorField,
    plot_vectors_mapf,
    plot_vectors_grid,
    plot_vectors_title,
    plot_vectors_style,
    plot_vectors_scale,
    plot_vectors_values,
    vector_line_style,
    vector_head_style,
) where

import Control.Lens
import Control.Monad
import Control.Applicative
import Data.Tuple
import Data.Colour hiding (over)
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Plot.Types

data VectorStyle = VectorStyle
    { _vector_line_style :: LineStyle
    , _vector_head_style :: PointStyle
    }

$( makeLenses ''VectorStyle )

data PlotVectors x y = PlotVectors
    { _plot_vectors_title        :: String
    , _plot_vectors_style        :: VectorStyle
    -- | Set to 1 (default) to normalize the length of vectors to a space
    --   between them (so that the vectors never overlap on the graph).
    --   Set to 0 to disable any scaling.
    --   Values in between 0 and 1 are also permitted to adjust scaling.
    , _plot_vectors_scale        :: Double
    -- | Provide a square-tiled regular grid.
    , _plot_vectors_grid         :: [(x,y)]
    -- | Provide a vector field (R^2 -> R^2) function.
    , _plot_vectors_mapf         :: (x,y) -> (x,y)
    -- | Provide a prepared list of (start,vector) pairs.
    , _plot_vectors_values       :: [((x,y),(x,y))]
    }

$( makeLenses ''PlotVectors )

mapGrid :: (PlotValue y, PlotValue x)
        => [(x,y)] -> ((x,y) -> (x,y)) -> [((x,y),(x,y))]
mapGrid grid f = zip grid (f <$> grid)

plotVectorField :: (PlotValue x, PlotValue y) => PlotVectors x y -> Plot x y
plotVectorField pv = Plot
    { _plot_render     = renderPlotVectors pv
    , _plot_legend     = [(_plot_vectors_title pv, renderPlotLegendVectors pv)]
    , _plot_all_points = (map fst pts, map snd pts)
    }
  where
    pvals = _plot_vectors_values pv
    mvals = mapGrid (_plot_vectors_grid pv) (_plot_vectors_mapf pv)
    pts = concatMap (\(a,b) -> [a,b]) (pvals ++ mvals)

renderPlotVectors :: (PlotValue x, PlotValue y)
                  => PlotVectors x y -> PointMapFn x y -> ChartBackend ()
renderPlotVectors pv pmap = do
    let pvals   = _plot_vectors_values pv
        mvals   = mapGrid (_plot_vectors_grid pv) (_plot_vectors_mapf pv)
        trans   = translateToStart <$> (pvals ++ mvals)
        pvecs   = filter (\v -> vlen' v > 0) $ over both (mapXY pmap) <$> trans
        mgrid   = take 2 $ fst <$> pvecs
        maxLen  = maximum $ vlen' <$> pvecs
        spacing = (!!1) $ (vlen <$> zipWith psub mgrid (reverse mgrid)) ++ [maxLen]
        sfactor = spacing/maxLen                  -- Non-adjusted scale factor
        afactor = sfactor + (1 - sfactor)*(1 - _plot_vectors_scale pv)
        tails   = pscale afactor <$> pvecs          -- Paths of arrows' tails
        angles  = (vangle . psub' . swap) <$> pvecs -- Angles of the arrows
        centers = snd <$> tails                     -- Where to draw arrow heads
    mapM_ (drawTail radius) tails
    zipWithM_ (drawArrowHead radius) centers angles
  where
    psub' = uncurry psub
    vlen' = vlen . psub'
    pvs = _plot_vectors_style pv
    radius = _point_radius $ _vector_head_style pvs
    hs angle = _vector_head_style pvs & point_shape
                  %~ (\(PointShapeArrowHead a) -> PointShapeArrowHead $ a+angle)
    translateToStart (s@(x,y),(vx,vy)) = (s,(tr x vx,tr y vy))
      where tr p t = fromValue $ toValue p + toValue t
    pscale w v@(s,_) = (s,translateP (vscale w . psub' $ swap v) s)
    drawTail r v = withLineStyle (_vector_line_style pvs) $
        strokePointPath $ (^..each) v'
      where
        v'  = pscale (1-(3/2)*r/l) v
        l   = vlen' v
    drawArrowHead r (Point x y) theta =
        withTranslation (Point (-r*cos theta) (-r*sin theta))
                        (drawPoint (hs theta) (Point x y))

renderPlotLegendVectors :: (PlotValue x, PlotValue y)
                        => PlotVectors x y -> Rect -> ChartBackend ()
renderPlotLegendVectors pv (Rect p1 p2) = do
    let y = (p_y p1 + p_y p2)/2
        pv' = plot_vectors_grid .~ []
            $ plot_vectors_values .~ [((fromValue $ p_x p1, fromValue y),
                                       (fromValue $ p_x p2, fromValue 0))]
            $ pv
    renderPlotVectors pv' pmap
  where
    pmap (LValue x,LValue y) = Point (toValue x) (toValue y)
    pmap _ = Point 0 0

instance Default VectorStyle where
  def = VectorStyle
    { _vector_line_style = (solidLine lw $ opaque blue)
                              { _line_cap = LineCapSquare }
    , _vector_head_style = PointStyle (opaque red) transparent lw (2*lw)
                                      (PointShapeArrowHead 0)
    } where lw = 2

instance Default (PlotVectors x y) where
  def = PlotVectors
    { _plot_vectors_title        = ""
    , _plot_vectors_style        = def
    , _plot_vectors_scale        = 1
    , _plot_vectors_grid         = []
    , _plot_vectors_mapf         = id
    , _plot_vectors_values       = []
    }
