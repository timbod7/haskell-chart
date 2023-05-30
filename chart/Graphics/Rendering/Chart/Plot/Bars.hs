-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Bars
-- Copyright   :  (c) Tim Docker 2006, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Bar Charts
--
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Graphics.Rendering.Chart.Plot.Bars(
    PlotBars(..),
    PlotBarsStyle(..),
    PlotBarsSpacing(..),
    PlotBarsAlignment(..),
    BarsPlotValue(..),
    BarHorizAnchor(..),
    BarVertAnchor(..),

    plotBars,
    plot_bars_style,
    plot_bars_item_styles,
    plot_bars_titles,
    plot_bars_spacing,
    plot_bars_alignment,
    plot_bars_reference,
    plot_bars_singleton_width,
    plot_bars_values,
    plot_bars_values_with_labels,
    plot_bars_label_bar_hanchor,
    plot_bars_label_bar_vanchor,
    plot_bars_label_text_hanchor,
    plot_bars_label_text_vanchor,
    plot_bars_label_angle,
    plot_bars_label_style,
    plot_bars_label_offset,

    addLabels
) where

import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Colour (opaque)
import Data.Colour.Names (black)
import Data.Default.Class
import Data.List(nub,sort)
import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry hiding (x0, y0)
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Utils

class PlotValue a => BarsPlotValue a where
    barsIsNull    :: a -> Bool
    barsReference :: [a] -> a
    barsAdd       :: a -> a -> a

instance BarsPlotValue Double where
    barsIsNull a  = a == 0.0
    barsReference = const 0
    barsAdd       = (+)

instance BarsPlotValue Int where
    barsIsNull a  = a == 0
    barsReference = const 0
    barsAdd       = (+)

instance BarsPlotValue LogValue where
    barsIsNull (LogValue a) = a == 0.0
    barsReference as        =
      10.0 ^^ (floor (log10 $ minimum $ filter (/= 0.0) as) :: Integer)
    barsAdd                 = (+)

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

data BarHorizAnchor
    = BHA_Left
    | BHA_Centre
    | BHA_Right
     deriving (Show)

data BarVertAnchor
    = BVA_Bottom
    | BVA_Centre
    | BVA_Top
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

   -- | The starting level for the chart, a function of some statistic
   --   (normally the lowest value or just const 0).
   _plot_bars_reference       :: [y] -> y,

   _plot_bars_singleton_width :: Double,

   -- | The actual points to be plotted, and their labels
   _plot_bars_values_with_labels :: [(x, [(y, String)])],

   -- | The point on the bar to horizontally anchor the label to
   _plot_bars_label_bar_hanchor :: BarHorizAnchor,

   -- | The point on the bar to vertically anchor the label to
   _plot_bars_label_bar_vanchor  :: BarVertAnchor,

    -- | The anchor point on the label.
   _plot_bars_label_text_hanchor :: HTextAnchor,

    -- | The anchor point on the label.
   _plot_bars_label_text_vanchor :: VTextAnchor,

   -- | Angle, in degrees, to rotate the label about the anchor point.
   _plot_bars_label_angle   :: Double,

   -- | The style to use for the label.
   _plot_bars_label_style   :: FontStyle,

   -- | The offset from the anchor point to display the label at.
   _plot_bars_label_offset  :: Vector
}

instance BarsPlotValue y => Default (PlotBars x y) where
  def = PlotBars
    { _plot_bars_style              = BarsClustered
    , _plot_bars_item_styles        = cycle istyles
    , _plot_bars_titles             = []
    , _plot_bars_spacing            = BarsFixGap 10 2
    , _plot_bars_alignment          = BarsCentered
    , _plot_bars_values_with_labels = []
    , _plot_bars_singleton_width    = 20
    , _plot_bars_reference          = barsReference
    , _plot_bars_label_bar_hanchor  = BHA_Centre
    , _plot_bars_label_bar_vanchor  = BVA_Top
    , _plot_bars_label_text_hanchor = HTA_Centre
    , _plot_bars_label_text_vanchor = VTA_Bottom
    , _plot_bars_label_angle        = 0
    , _plot_bars_label_style        = def
    , _plot_bars_label_offset       = Vector 0 0
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

renderPlotBars :: (BarsPlotValue y) => PlotBars x y -> PointMapFn x y -> BackendProgram ()
renderPlotBars p pmap = case _plot_bars_style p of
      BarsClustered -> forM_ vals clusteredBars
      BarsStacked   -> forM_ vals stackedBars
  where
    clusteredBars (x,ys) = do
       let offset i = case _plot_bars_alignment p of
             BarsLeft     -> fromIntegral i * width
             BarsRight    -> fromIntegral (i-nys) * width
             BarsCentered -> fromIntegral (2*i-nys) * width/2
       forM_ (zip3 [0,1..] ys styles) $ \(i, (y, _), (fstyle,_)) ->
           unless (barsIsNull y) $
           withFillStyle fstyle $
             alignFillPath (barPath (offset i) x yref0 y)
             >>= fillPath
       forM_ (zip3 [0,1..] ys styles) $ \(i, (y, _), (_,mlstyle)) ->
           unless (barsIsNull y) $
           whenJust mlstyle $ \lstyle ->
             withLineStyle lstyle $
               alignStrokePath (barPath (offset i) x yref0 y)
               >>= strokePath
       withFontStyle (_plot_bars_label_style p) $
           forM_ (zip [0,1..] ys) $ \(i, (y, txt)) ->
             unless (null txt) $ do
               let h = _plot_bars_label_bar_hanchor p
               let v = _plot_bars_label_bar_vanchor p
               let pt = rectCorner h v (barRect (offset i) x yref0 y)
               drawTextR
                  (_plot_bars_label_text_hanchor p)
                  (_plot_bars_label_text_vanchor p)
                  (_plot_bars_label_angle p)
                  (pvadd pt $ _plot_bars_label_offset p)
                  txt

    stackedBars (x,ys) = do
       let ys' = map fst ys
       let lbls = map snd ys
       let y2s = zip (yref0:stack ys') (stack ys')
       let ofs = case _plot_bars_alignment p of
             BarsLeft     -> 0
             BarsRight    -> -width
             BarsCentered -> -(width/2)
       forM_ (zip y2s styles) $ \((y0,y1), (fstyle,_)) ->
           unless (y0 == y1) $
           withFillStyle fstyle $
             alignFillPath (barPath ofs x y0 y1)
             >>= fillPath
       forM_ (zip y2s styles) $ \((y0,y1), (_,mlstyle)) ->
           unless (y0 == y1) $
           whenJust mlstyle $ \lstyle ->
              withLineStyle lstyle $
                alignStrokePath (barPath ofs x y0 y1)
                >>= strokePath
       withFontStyle (_plot_bars_label_style p) $
           forM_ (zip y2s lbls) $ \((y0, y1), txt) ->
             unless (null txt) $ do
               let h = _plot_bars_label_bar_hanchor p
               let v = _plot_bars_label_bar_vanchor p
               let pt = rectCorner h v (barRect ofs x y0 y1)
               drawTextR
                  (_plot_bars_label_text_hanchor p)
                  (_plot_bars_label_text_vanchor p)
                  (_plot_bars_label_angle p)
                  (pvadd pt $ _plot_bars_label_offset p)
                  txt

    barRect xos x y0 y1 = Rect (Point (x'+xos) y0') (Point (x'+xos+width) y') where
      Point x' y' = pmap' (x,y1)
      Point _ y0' = pmap' (x,y0)

    barPath xos x y0 y1 = rectPath $ barRect xos x y0 y1

    rectCorner h v (Rect (Point x0 y0) (Point x1 y1)) = Point x' y' where
        x' = case h of
                  BHA_Left   -> x0
                  BHA_Right  -> x1
                  BHA_Centre -> (x0 + x1) / 2
        y' = case v of
                  BVA_Bottom -> y0
                  BVA_Top    -> y1
                  BVA_Centre -> (y0 + y1) / 2

    yref0 = _plot_bars_reference p lowerVals
    vals  = _plot_bars_values_with_labels p
    lowerVals = case _plot_bars_style p of
                  BarsClustered -> concatMap (map fst . snd) vals
                  BarsStacked   -> map (fst . head . snd) vals
    width = case _plot_bars_spacing p of
        BarsFixGap gap minw -> let w = max (minXInterval - gap) minw in
            case _plot_bars_style p of
                BarsClustered -> w / fromIntegral nys
                BarsStacked -> w
        BarsFixWidth width' -> width'
    styles = _plot_bars_item_styles p

    minXInterval = let diffs = zipWith (-) (tail mxs) mxs
                   in if null diffs
                        then _plot_bars_singleton_width p
                        else minimum diffs
      where
        xs  = fst (allBarPoints p)
        mxs = nub $ sort $ map mapX xs

    nys    = maximum [ length ys | (_,ys) <- vals ]

    pmap'  = mapXY pmap
    mapX x = p_x (pmap' (x, yref0))

-- Provided for backward compat. Note that this does not satisfy the lens laws, as it discards/overwrites the labels.
plot_bars_values :: Lens' (PlotBars x y) [(x, [y])]
plot_bars_values = lens getter setter
  where
    getter = mapYs fst . _plot_bars_values_with_labels
    setter pb vals' = pb { _plot_bars_values_with_labels = mapYs (, "") vals' }
    mapYs :: (a -> b) -> [(c, [a])] -> [(c, [b])]
    mapYs f = map (over _2 $ map f)

-- Helper function for adding labels to a data series
addLabels :: Show y => [(x, [y])] -> [(x, [(y, String)])]
addLabels = map . second $ map (\x -> (x, show x))

allBarPoints :: (BarsPlotValue y) => PlotBars x y -> ([x],[y])
allBarPoints p = case _plot_bars_style p of
    BarsClustered ->
      let ys = concatMap snd pts in
      ( xs, f0 ys:ys )
    BarsStacked   ->
      let ys = map snd pts in
      ( xs, f0 (map head ys):concatMap stack ys)
  where
    pts = view plot_bars_values  p
    xs  = map fst pts
    f0  = _plot_bars_reference p

stack :: (BarsPlotValue y) => [y] -> [y]
stack = scanl1 barsAdd

renderPlotLegendBars :: (FillStyle,Maybe LineStyle) -> Rect -> BackendProgram ()
renderPlotLegendBars (fstyle,_) r =
  withFillStyle fstyle $
    fillPath (rectPath r)

$( makeLenses ''PlotBars )
