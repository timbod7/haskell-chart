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
    plotHBars,

    plot_bars_style,
    plot_bars_item_styles,
    plot_bars_titles,
    plot_bars_spacing,
    plot_bars_alignment,
    plot_bars_singleton_width,
    plot_bars_label_bar_hanchor,
    plot_bars_label_bar_vanchor,
    plot_bars_label_text_hanchor,
    plot_bars_label_text_vanchor,
    plot_bars_label_angle,
    plot_bars_label_style,
    plot_bars_label_offset,

    plot_bars_values,

    plot_bars_settings,
    plot_bars_values_with_labels,

    addLabels
) where

import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Colour (opaque)
import Data.Colour.Names (black)
import Data.Default.Class
import Data.Tuple(swap)
import Data.List(nub,sort)
import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry hiding (x0, y0)
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Utils
class PlotValue a => BarsPlotValue a where
    barsIsNull    :: a -> Bool
    -- | The starting level for the chart, a function of some statistic
    --   (normally the lowest value or just const 0).
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
data BarsSettings = BarsSettings {
   -- | This value specifies whether each value from [y] should be
   --   shown beside or above the previous value.
   _bars_settings_style           :: PlotBarsStyle,

   -- | The style in which to draw each element of [y]. A fill style
   --   is required, and if a linestyle is given, each bar will be
   --   outlined.
   _bars_settings_item_styles     :: [ (FillStyle,Maybe LineStyle) ],

   -- | This value controls how the widths of the bars are
   --   calculated. Either the widths of the bars, or the gaps between
   --   them can be fixed.
   _bars_settings_spacing         :: PlotBarsSpacing,

   -- | This value controls how bars for a fixed x are aligned with
   --   respect to the device coordinate corresponding to x.
   _bars_settings_alignment       :: PlotBarsAlignment,

   _bars_settings_singleton_width :: Double,

   -- | The point on the bar to horizontally anchor the label to
   _bars_settings_label_bar_hanchor :: BarHorizAnchor,

   -- | The point on the bar to vertically anchor the label to
   _bars_settings_label_bar_vanchor  :: BarVertAnchor,

    -- | The anchor point on the label.
   _bars_settings_label_text_hanchor :: HTextAnchor,

    -- | The anchor point on the label.
   _bars_settings_label_text_vanchor :: VTextAnchor,

   -- | Angle, in degrees, to rotate the label about the anchor point.
   _bars_settings_label_angle   :: Double,

   -- | The style to use for the label.
   _bars_settings_label_style   :: FontStyle,

   -- | The offset from the anchor point to display the label at.
   _bars_settings_label_offset  :: Vector
}
instance Default BarsSettings where
  def = BarsSettings
    { _bars_settings_style              = BarsClustered
    , _bars_settings_item_styles        = cycle istyles
    , _bars_settings_spacing            = BarsFixGap 10 2
    , _bars_settings_alignment          = BarsCentered
    , _bars_settings_singleton_width    = 20
    , _bars_settings_label_bar_hanchor  = BHA_Centre
    , _bars_settings_label_bar_vanchor  = BVA_Top
    , _bars_settings_label_text_hanchor = HTA_Centre
    , _bars_settings_label_text_vanchor = VTA_Bottom
    , _bars_settings_label_angle        = 0
    , _bars_settings_label_style        = def
    , _bars_settings_label_offset       = Vector 0 0
    }
    where
      istyles   = map mkstyle defaultColorSeq
      mkstyle c = (solidFillStyle c, Just (solidLine 1.0 $ opaque black))
data PlotBars x y = PlotBars {
   _plot_bars_settings :: BarsSettings,
   -- | The title of each element of [y]. These will be shown in the legend.
   _plot_bars_titles :: [String],
   -- | The actual points to be plotted, and their labels
   _plot_bars_values_with_labels :: [(x, [(y, String)])]
}
instance Default (PlotBars x y) where
  def = PlotBars
    { _plot_bars_settings = def
    , _plot_bars_titles = []
    , _plot_bars_values_with_labels = []
    }

plotBars :: (BarsPlotValue y) => PlotBars x y -> Plot x y
plotBars p = Plot {
        _plot_render = \pmap -> renderBars (_plot_bars_settings p) vals yref0
                                           (barRect pmap) (mapX pmap),
        _plot_legend     = zip (_plot_bars_titles p)
                               (map renderPlotLegendBars
                                    (_bars_settings_item_styles $ _plot_bars_settings p)),
        _plot_all_points = allBarPoints p
    }
  where
    vals = _plot_bars_values_with_labels p

    barRect pmap xos width x y0 y1 = Rect (Point (x'+xos) y0') (Point (x'+xos+width) y') where
      Point x' y' = mapXY pmap (x,y1)
      Point _ y0' = mapXY pmap (x,y0)

    yref0 = barsReference $ case _bars_settings_style $ _plot_bars_settings p of
              BarsClustered -> concatMap (map fst . snd) vals
              BarsStacked   -> map (fst . head . snd) vals

    mapX pmap x = p_x (mapXY pmap (x, yref0))

plotHBars :: (BarsPlotValue x) => PlotBars y x -> Plot x y
plotHBars p = Plot {
        _plot_render = \pmap -> renderBars (_plot_bars_settings p) vals xref0
                                           (barRect pmap) (mapY pmap),
        _plot_legend     = zip (_plot_bars_titles p)
                               (map renderPlotLegendBars
                                    (_bars_settings_item_styles $ _plot_bars_settings p)),
        _plot_all_points = swap $ allBarPoints p
    }
  where
    vals = _plot_bars_values_with_labels p

    barRect pmap yos height y x0 x1 = Rect (Point x0' (y'+yos)) (Point x' (y'+yos+height)) where
      Point x' y' = mapXY pmap (x1,y)
      Point x0' _ = mapXY pmap (x0,y)

    xref0 = barsReference $ case _bars_settings_style $ _plot_bars_settings p of
                  BarsClustered -> concatMap (map fst . snd) vals
                  BarsStacked   -> map (fst . head . snd) vals

    mapY pmap y = p_y (mapXY pmap (xref0, y))

renderBars :: (BarsPlotValue v) =>
              BarsSettings
           -> [(k, [(v, String)])]
           -> v
           -> (Double -> Double -> k -> v -> v -> Rect)
           -> (k -> Double)
           -> BackendProgram ()
renderBars p vals vref0 r mapk = case _bars_settings_style p of
      BarsClustered -> forM_ vals clusteredBars
      BarsStacked   -> forM_ vals stackedBars
  where
    clusteredBars (k,vs) = do
       let offset i = case _bars_settings_alignment p of
             BarsLeft     -> fromIntegral i * bsize
             BarsRight    -> fromIntegral (i-nvs) * bsize
             BarsCentered -> fromIntegral (2*i-nvs) * bsize/2
       forM_ (zip3 [0,1..] vs styles) $ \(i, (v, _), (fstyle,_)) ->
           unless (barsIsNull v) $
           withFillStyle fstyle $
             alignFillPath (rectPath $ r (offset i) bsize k vref0 v)
             >>= fillPath
       forM_ (zip3 [0,1..] vs styles) $ \(i, (v, _), (_,mlstyle)) ->
           unless (barsIsNull v) $
           whenJust mlstyle $ \lstyle ->
             withLineStyle lstyle $
               alignStrokePath (rectPath $ r (offset i) bsize k vref0 v)
               >>= strokePath
       withFontStyle (_bars_settings_label_style p) $
           forM_ (zip [0,1..] vs) $ \(i, (v, txt)) ->
             unless (null txt) $ do
               let ha = _bars_settings_label_bar_hanchor p
               let va = _bars_settings_label_bar_vanchor p
               let pt = rectCorner ha va (r (offset i) bsize k vref0 v)
               drawTextR
                  (_bars_settings_label_text_hanchor p)
                  (_bars_settings_label_text_vanchor p)
                  (_bars_settings_label_angle p)
                  (pvadd pt $ _bars_settings_label_offset p)
                  txt

    stackedBars (x,ys) = do
       let (ys', lbls) = unzip ys
       let y2s = zip (vref0:stack ys') (stack ys')
       let ofs = case _bars_settings_alignment p of
             BarsLeft     -> 0
             BarsRight    -> -bsize
             BarsCentered -> -(bsize/2)
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
       withFontStyle (_bars_settings_label_style p) $
           forM_ (zip y2s lbls) $ \((y0, y1), txt) ->
             unless (null txt) $ do
               let ha = _bars_settings_label_bar_hanchor p
               let va = _bars_settings_label_bar_vanchor p
               let pt = rectCorner ha va (r ofs bsize x y0 y1)
               drawTextR
                  (_bars_settings_label_text_hanchor p)
                  (_bars_settings_label_text_vanchor p)
                  (_bars_settings_label_angle p)
                  (pvadd pt $ _bars_settings_label_offset p)
                  txt

    styles = _bars_settings_item_styles p

    barPath os k v0 v1 = rectPath $ r os bsize k v0 v1

    bsize = case _bars_settings_spacing p of
        BarsFixGap gap minw -> let w = max (minKInterval - gap) minw in
            case _bars_settings_style p of
                BarsClustered -> w / fromIntegral nvs
                BarsStacked -> w
        BarsFixWidth width' -> width'

    minKInterval = let diffs = zipWith (-) (tail mks) mks
                   in if null diffs
                        then _bars_settings_singleton_width p
                        else minimum diffs
      where
        mks = nub $ sort $ map (mapk . fst) vals

    nvs = maximum $ map (length . snd) vals

rectCorner :: BarHorizAnchor -> BarVertAnchor -> Rect -> Point
rectCorner h v (Rect (Point x0 y0) (Point x1 y1)) = Point x' y' where
    x' = case h of
              BHA_Left   -> x0
              BHA_Right  -> x1
              BHA_Centre -> (x0 + x1) / 2
    y' = case v of
              BVA_Bottom -> y0
              BVA_Top    -> y1
              BVA_Centre -> (y0 + y1) / 2

-- Helper function for printing bar values as labels
addLabels :: Show y => [(x, [y])] -> [(x, [(y, String)])]
addLabels = map . second $ map (\y -> (y, show y))

allBarPoints :: (BarsPlotValue y) => PlotBars x y -> ([x],[y])
allBarPoints (PlotBars p _ vals) = case _bars_settings_style p of
    BarsClustered ->
      let ys = concatMap (map fst) yls in
      ( xs, barsReference ys:ys )
    BarsStacked   ->
      let ys = map (map fst) yls in
      ( xs, barsReference (map head ys):concatMap stack ys)
  where (xs, yls) = unzip vals

stack :: (BarsPlotValue y) => [y] -> [y]
stack = scanl1 barsAdd

renderPlotLegendBars :: (FillStyle,Maybe LineStyle) -> Rect -> BackendProgram ()
renderPlotLegendBars (fstyle,_) r =
  withFillStyle fstyle $
    fillPath (rectPath r)

$( makeLenses ''BarsSettings )
$( makeLenses ''PlotBars )

-- Lens provided for backward compat.

-- Note that this one does not satisfy the lens laws, as it discards/overwrites the labels.
plot_bars_values :: Lens' (PlotBars x y) [(x, [y])]
plot_bars_values = lens getter setter
  where
    getter = mapYs fst . _plot_bars_values_with_labels
    setter pb vals' = pb { _plot_bars_values_with_labels = mapYs (, "") vals' }
    mapYs :: (a -> b) -> [(c, [a])] -> [(c, [b])]
    mapYs f = map (over _2 $ map f)

plot_bars_style :: Lens' (PlotBars x y) PlotBarsStyle
plot_bars_style = plot_bars_settings . bars_settings_style

plot_bars_item_styles :: Lens' (PlotBars x y) [(FillStyle, Maybe LineStyle)]
plot_bars_item_styles = plot_bars_settings . bars_settings_item_styles

plot_bars_spacing :: Lens' (PlotBars x y) PlotBarsSpacing
plot_bars_spacing = plot_bars_settings . bars_settings_spacing

plot_bars_alignment :: Lens' (PlotBars x y) PlotBarsAlignment
plot_bars_alignment =  plot_bars_settings . bars_settings_alignment

plot_bars_singleton_width :: Lens' (PlotBars x y) Double
plot_bars_singleton_width = plot_bars_settings . bars_settings_singleton_width

plot_bars_label_bar_hanchor :: Lens' (PlotBars x y) BarHorizAnchor
plot_bars_label_bar_hanchor = plot_bars_settings . bars_settings_label_bar_hanchor

plot_bars_label_bar_vanchor :: Lens' (PlotBars x y) BarVertAnchor
plot_bars_label_bar_vanchor = plot_bars_settings . bars_settings_label_bar_vanchor

plot_bars_label_text_hanchor :: Lens' (PlotBars x y) HTextAnchor
plot_bars_label_text_hanchor = plot_bars_settings . bars_settings_label_text_hanchor

plot_bars_label_text_vanchor :: Lens' (PlotBars x y) VTextAnchor
plot_bars_label_text_vanchor = plot_bars_settings . bars_settings_label_text_vanchor

plot_bars_label_angle :: Lens' (PlotBars x y) Double
plot_bars_label_angle = plot_bars_settings . bars_settings_label_angle

plot_bars_label_style :: Lens' (PlotBars x y) FontStyle
plot_bars_label_style = plot_bars_settings . bars_settings_label_style

plot_bars_label_offset :: Lens' (PlotBars x y) Vector
plot_bars_label_offset = plot_bars_settings . bars_settings_label_offset
