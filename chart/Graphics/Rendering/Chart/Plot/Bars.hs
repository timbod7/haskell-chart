-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Bars
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Bar Charts
--
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Bars(
    PlotBars(..),
    defaultPlotBars,
    PlotBarsStyle(..),
    PlotBarsSpacing(..),
    PlotBarsAlignment(..),
    BarsPlotValue(..),

    plotBars,
    plot_bars_style,
    plot_bars_item_styles,
    plot_bars_titles,
    plot_bars_spacing,
    plot_bars_alignment,
    plot_bars_reference,
    plot_bars_singleton_width,
    plot_bars_values,

) where

import Control.Lens
import Control.Monad
import Data.List(nub,sort)
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Axis
import Data.Colour (opaque)
import Data.Colour.Names (black, blue)
import Data.Colour.SRGB (sRGB)
import Data.Default.Class

class PlotValue a => BarsPlotValue a where
    barsReference :: a
    barsAdd       :: a -> a -> a

instance BarsPlotValue Double where
    barsReference = 0
    barsAdd       = (+)
instance BarsPlotValue Int where
    barsReference = 0
    barsAdd       = (+)

data PlotBarsStyle
    = BarsStacked   -- ^ Bars for a fixed x are stacked vertically
                    --   on top of each other.
    | BarsClustered -- ^ Bars for a fixed x are put horizontally
                    --   beside each other.
     deriving (Show)

data PlotBarsSpacing
    = BarsFixWidth Double       -- ^ All bars have the same width in pixels.
    | BarsFixGap Double Double  -- ^ (BarsFixGap g mw) means make the gaps between
                                --   the bars equal to g, but with a minimum bar width
                                --   of mw
     deriving (Show)

-- | How bars for a given (x,[y]) are aligned with respect to screen
--   coordinate corresponding to x (deviceX).
data PlotBarsAlignment = BarsLeft      -- ^ The left edge of bars is at deviceX
                       | BarsCentered  -- ^ Bars are centered around deviceX
                       | BarsRight     -- ^ The right edge of bars is at deviceX
     deriving (Show)

-- | Value describing how to plot a set of bars.
--   Note that the input data is typed [(x,[y])], ie for each x value
--   we plot several y values. Typically the size of each [y] list would
--   be the same.
data PlotBars x y = PlotBars {
   -- | This value specifies whether each value from [y] should be
   --   shown beside or above the previous value.
   _plot_bars_style           :: PlotBarsStyle,

   -- | The style in which to draw each element of [y]. A fill style
   --   is required, and if a linestyle is given, each bar will be
   --   outlined.
   _plot_bars_item_styles     :: [ (FillStyle,Maybe LineStyle) ],

   -- | The title of each element of [y]. These will be shown in the legend.
   _plot_bars_titles          :: [String],

   -- | This value controls how the widths of the bars are
   --   calculated. Either the widths of the bars, or the gaps between
   --   them can be fixed.
   _plot_bars_spacing         :: PlotBarsSpacing,

   -- | This value controls how bars for a fixed x are aligned with
   --   respect to the device coordinate corresponding to x.
   _plot_bars_alignment       :: PlotBarsAlignment,

   -- | The starting level for the chart (normally 0).
   _plot_bars_reference       :: y,

   _plot_bars_singleton_width :: Double,

   -- | The actual points to be plotted.
   _plot_bars_values          :: [ (x,[y]) ]
}

{-# DEPRECATED defaultPlotBars "Use the according Data.Default instance!" #-}
defaultPlotBars :: BarsPlotValue y => PlotBars x y
defaultPlotBars = def

instance BarsPlotValue y => Default (PlotBars x y) where
  def = PlotBars
    { _plot_bars_style           = BarsClustered
    , _plot_bars_item_styles     = cycle istyles
    , _plot_bars_titles          = []
    , _plot_bars_spacing         = BarsFixGap 10 2
    , _plot_bars_alignment       = BarsCentered
    , _plot_bars_values          = []
    , _plot_bars_singleton_width = 20
    , _plot_bars_reference       = barsReference
    }
    where
      istyles   = map mkstyle defaultColorSeq
      mkstyle c = (solidFillStyle c, Just (solidLine 1.0 $ opaque black))

plotBars :: (BarsPlotValue y) => PlotBars x y -> Plot x y
plotBars p = Plot {
        _plot_render     = renderPlotBars p,
        _plot_legend     = zip (_plot_bars_titles p)
                               (map renderPlotLegendBars
                                    (_plot_bars_item_styles p)),
        _plot_all_points = allBarPoints p
    }

renderPlotBars :: (BarsPlotValue y) => PlotBars x y -> PointMapFn x y -> ChartBackend ()
renderPlotBars p pmap = case (_plot_bars_style p) of
      BarsClustered -> forM_ vals clusteredBars
      BarsStacked   -> forM_ vals stackedBars
  where
    clusteredBars (x,ys) = do
       forM_ (zip3 [0,1..] ys styles) $ \(i, y, (fstyle,_)) -> do
           withFillStyle fstyle $ do
             p <- alignFillPath (barPath (offset i) x yref0 y)
             fillPath p
       forM_ (zip3 [0,1..] ys styles) $ \(i, y, (_,mlstyle)) -> do
           whenJust mlstyle $ \lstyle -> do
             withLineStyle lstyle $ do
               p <- alignStrokePath (barPath (offset i) x yref0 y)
               strokePath p

    offset = case (_plot_bars_alignment p) of
      BarsLeft     -> \i -> fromIntegral i * width
      BarsRight    -> \i -> fromIntegral (i-nys) * width
      BarsCentered -> \i -> fromIntegral (2*i-nys) * width/2

    stackedBars (x,ys) = do
       let y2s = zip (yref0:stack ys) (stack ys)
       let ofs = case (_plot_bars_alignment p) of {
         BarsLeft     -> 0          ;
         BarsRight    -> (-width)   ;
         BarsCentered -> (-width/2)
         }
       forM_ (zip y2s styles) $ \((y0,y1), (fstyle,_)) -> do
           withFillStyle fstyle $ do
             p <- alignFillPath (barPath ofs x y0 y1)
             fillPath p
       forM_ (zip y2s styles) $ \((y0,y1), (_,mlstyle)) -> do
           whenJust mlstyle $ \lstyle -> do
              withLineStyle lstyle $ do
                p <- alignStrokePath (barPath ofs x y0 y1)
                strokePath p

    barPath xos x y0 y1 = do
      let (Point x' y') = pmap' (x,y1)
      let (Point _ y0') = pmap' (x,y0)
      rectPath (Rect (Point (x'+xos) y0') (Point (x'+xos+width) y'))

    yref0 = _plot_bars_reference p
    vals  = _plot_bars_values p
    width = case _plot_bars_spacing p of
        BarsFixGap gap minw -> let w = max (minXInterval - gap) minw in
            case (_plot_bars_style p) of
                BarsClustered -> w / fromIntegral nys
                BarsStacked -> w
        BarsFixWidth width -> width
    styles = _plot_bars_item_styles p

    minXInterval = let diffs = zipWith (-) (tail mxs) mxs
                   in if null diffs
                        then _plot_bars_singleton_width p
                        else minimum diffs
      where
        xs  = fst (allBarPoints p)
        mxs = nub $ sort $ map mapX xs

    nys    = maximum [ length ys | (x,ys) <- vals ]

    pmap'  = mapXY pmap
    mapX x = p_x (pmap' (x,barsReference))

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) f = f a
whenJust _        _ = return ()

allBarPoints :: (BarsPlotValue y) => PlotBars x y -> ([x],[y])
allBarPoints p = case (_plot_bars_style p) of
    BarsClustered -> ( [x| (x,_) <- pts], y0:concat [ys| (_,ys) <- pts] )
    BarsStacked   -> ( [x| (x,_) <- pts], y0:concat [stack ys | (_,ys) <- pts] )
  where
    pts = _plot_bars_values p
    y0  = _plot_bars_reference p

stack :: (BarsPlotValue y) => [y] -> [y]
stack ys = scanl1 barsAdd ys


renderPlotLegendBars :: (FillStyle,Maybe LineStyle) -> Rect -> ChartBackend ()
renderPlotLegendBars (fstyle,mlstyle) r@(Rect p1 p2) = do
  withFillStyle fstyle $ do
    fillPath (rectPath r)

$( makeLenses ''PlotBars )
