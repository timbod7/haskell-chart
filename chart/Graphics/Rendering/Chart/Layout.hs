-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Layout
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- This module glues together axes and plots to actually create a renderable
-- for a chart.
--
-- Note that template haskell is used to derive accessor functions
-- (see 'Data.Accessor') for each field of the following data types:
--
--     * 'Layout1'
-- 
--     * 'StackedLayouts'
--
--     * 'LayoutAxis'
--
-- These accessors are not shown in this API documentation.  They have
-- the same name as the field, but with the trailing underscore
-- dropped. Hence for data field f_::F in type D, they have type
--
-- @
--   f :: Data.Accessor.Accessor D F
-- @
--
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Rendering.Chart.Layout(
    Layout1(..),
    LayoutAxis(..),
    Layout1Pick(..),
    StackedLayouts(..),
    StackedLayout(..),
    MAxisFn,

    defaultLayout1,
    layout1ToRenderable,
    linkAxes,
    independentAxes,

    updateAllAxesStyles,
    setLayout1Foreground,

    defaultLayoutAxis,
    laxis_title_style,
    laxis_title,
    laxis_style,
    laxis_visible,
    laxis_generate,
    laxis_override,
    laxis_reverse,

    layout1_background,
    layout1_plot_background,
    layout1_title,
    layout1_title_style,
    layout1_left_axis,
    layout1_right_axis,
    layout1_top_axis,
    layout1_bottom_axis,
    layout1_yaxes_control,
    layout1_margin,
    layout1_plots,
    layout1_legend,
    layout1_grid_last,

    defaultStackedLayouts,
    slayouts_layouts,
    slayouts_compress_xlabels,
    slayouts_compress_legend,

    renderStackedLayouts,
  ) where

import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Utils
import Graphics.Rendering.Chart.Plot
import Graphics.Rendering.Chart.Legend
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Grid
import Control.Monad
import Control.Monad.Reader (local)
import Control.Lens
import Data.Colour
import Data.Colour.Names (white)
import Data.Default.Class

-- | A @MAxisFn@ is a function that generates an (optional) axis
--   given the points plotted against that axis.
type MAxisFn t = [t] -> Maybe (AxisData t)

data LayoutAxis x = LayoutAxis {
   _laxis_title_style :: FontStyle,
   _laxis_title       :: String,
   _laxis_style       :: AxisStyle,

   -- | Function that determines whether an axis should be visible,
   --   based upon the points plotted on this axis. The default value
   --   is 'not.null'.
   _laxis_visible     :: [x] -> Bool,

   -- | Function that generates the axis data, based upon the
   --   points plotted. The default value is 'autoAxis'.
   _laxis_generate    :: AxisFn x,

   -- | Function that can be used to override the generated axis data.
   --   The default value is 'id'.
   _laxis_override    :: AxisData x -> AxisData x,

   -- | True if left to right (bottom to top) is to show descending values.
   _laxis_reverse     :: Bool

}

-- | A Layout1 value is a single plot area, with optional: axes on
--   each of the 4 sides; title at the top; legend at the bottom. It's
--   parameterised by the types of values to be plotted on the horizonal
--   and vertical axes.
data Layout1 x y = Layout1 {

    _layout1_background      :: FillStyle,
    _layout1_plot_background :: Maybe FillStyle,

    _layout1_title           :: String,
    _layout1_title_style     :: FontStyle,

    _layout1_bottom_axis     :: LayoutAxis x,
    _layout1_top_axis        :: LayoutAxis x,
    _layout1_left_axis       :: LayoutAxis y,
    _layout1_right_axis      :: LayoutAxis y,

    -- | Function to map points from the left/right plot
    --   to the left/right axes. The default value is 'id'.
    _layout1_yaxes_control   :: ([y],[y]) -> ([y],[y]),

    _layout1_margin          :: Double,
    _layout1_plots           :: [Either (Plot x y) (Plot x y)],
    _layout1_legend          :: Maybe LegendStyle,

    -- | True if the grid is to be rendered on top of the Plots.
    _layout1_grid_last       :: Bool
}

instance (Ord x, Ord y) => ToRenderable (Layout1 x y) where
  toRenderable = setPickFn nullPickFn . layout1ToRenderable

data Layout1Pick x y = L1P_Legend String
                     | L1P_Title String
                     | L1P_BottomAxisTitle String
                     | L1P_TopAxisTitle String
                     | L1P_LeftAxisTitle String
                     | L1P_RightAxisTitle String
                     | L1P_PlotArea x y y
                     | L1P_BottomAxis x
                     | L1P_TopAxis x
                     | L1P_LeftAxis y
                     | L1P_RightAxis y
    deriving (Show)

type LegendItem = (String,Rect -> ChartBackend ())

-- | A layout with it's y type hidded, so that it can be stacked
-- with other layouts (with differing y types)
data StackedLayout x = forall y . Ord y => StackedLayout (Layout1 x y)

-- | A holder for a set of vertically stacked layouts
data StackedLayouts x = StackedLayouts {
      _slayouts_layouts :: [StackedLayout x],
      _slayouts_compress_xlabels :: Bool,
      _slayouts_compress_legend :: Bool
}

{-# DEPRECATED defaultStackedLayouts  "Use the according Data.Default instance!" #-}
defaultStackedLayouts :: StackedLayouts x
defaultStackedLayouts = def

instance Default (StackedLayouts x) where
  def = StackedLayouts [] True True

-- | Render several layouts with the same x-axis type and range,
--   vertically stacked so that their origins and x-values are aligned.
--
-- The legends from all the charts may be optionally combined, and shown
-- once on the bottom chart.   The x labels may be optionally removed so that
-- they are only shown once.
renderStackedLayouts :: (Ord x) => StackedLayouts x -> Renderable ()
renderStackedLayouts (StackedLayouts{_slayouts_layouts=[]}) = emptyRenderable
renderStackedLayouts slp@(StackedLayouts{_slayouts_layouts=sls@(sl1:_)}) = gridToRenderable g
  where
    g = fullOverlayUnder (fillBackground bg emptyRenderable)
      $ foldr (above.mkGrid) nullt (zip sls [0,1..])
      
    mkGrid ((StackedLayout l),i)
        = (noPickFn $ layout1TitleToRenderable l)
          `wideAbove`
          (addMarginsToGrid (lm,lm,lm,lm) $ mkPlotArea baxis taxis)
          `aboveWide`
          (if showLegend then noPickFn $ renderLegend l legenditems else emptyRenderable)

      where
        legenditems = case (_slayouts_compress_legend slp,isBottomPlot) of
            (False,_) -> getLegendItems l
            (True,True) -> alllegendItems
            (True,False) -> ([],[])

        mkPlotArea bx tx = fmap (mapPickFn (const ()))
                         $ layout1PlotAreaToGrid l{_layout1_bottom_axis=bx,_layout1_top_axis=tx}

        showLegend = not (null (fst legenditems)) || not (null (snd legenditems))

        isTopPlot = i == 0
        isBottomPlot = i == length sls -1

        lm = _layout1_margin l

        baxis = mkAxis (_layout1_bottom_axis l) (isBottomPlot || not (_slayouts_compress_xlabels slp))
        taxis = mkAxis (_layout1_top_axis l) (isTopPlot || not (_slayouts_compress_xlabels slp))

        mkAxis a showLabels = a{
            _laxis_generate=const (_laxis_generate a all_xvals),
            _laxis_override= if showLabels then id else \ad -> ad{_axis_labels=[]}
        }

    bg = (\(StackedLayout l) -> _layout1_background l) sl1
    
    all_xvals = concatMap (\(StackedLayout l) -> getLayout1XVals l) sls

    alllegendItems = (concatMap (fst.legendItems) sls, concatMap (snd.legendItems) sls)
    legendItems (StackedLayout l) = (getLegendItems l)
    
    noPickFn :: Renderable a -> Renderable ()
    noPickFn = mapPickFn (const ())

addMarginsToGrid :: (Double,Double,Double,Double) -> Grid (Renderable a)
                 -> Grid (Renderable a)
addMarginsToGrid (t,b,l,r) g = aboveN [
     besideN [er, ts, er],
     besideN [ls, g,  rs],
     besideN [er, bs, er]
  ]
  where
    er = empty
    ts = tval $ spacer (0,t)
    ls = tval $ spacer (l,0)
    bs = tval $ spacer (0,b)
    rs = tval $ spacer (r,0)

layout1ToRenderable :: (Ord x, Ord y) =>
                       Layout1 x y -> Renderable (Layout1Pick x y)
layout1ToRenderable l = 
  fillBackground (_layout1_background l) $ gridToRenderable (layout1ToGrid l)

layout1ToGrid :: (Ord x, Ord y) =>
                 Layout1 x y -> Grid (Renderable (Layout1Pick x y))
layout1ToGrid l = aboveN
       [  tval $ layout1TitleToRenderable l
       ,  weights (1,1) $ tval $ gridToRenderable $
              addMarginsToGrid (lm,lm,lm,lm) (layout1PlotAreaToGrid l)
       ,  tval $ layout1LegendsToRenderable l
       ]
  where
    lm = _layout1_margin l

layout1TitleToRenderable :: (Ord x, Ord y) => Layout1 x y
                                           -> Renderable (Layout1Pick x y)
layout1TitleToRenderable l | null (_layout1_title l) = emptyRenderable
layout1TitleToRenderable l = addMargins (lm/2,0,0,0)
                                        (mapPickFn L1P_Title title)
  where
    title = label (_layout1_title_style l) HTA_Centre VTA_Centre
                  (_layout1_title l)
    lm    = _layout1_margin l

getLayout1XVals :: Layout1 x y -> [x]
getLayout1XVals l = concatMap (fst._plot_all_points.deEither) (_layout1_plots l)
  where
    deEither (Left x)  = x
    deEither (Right x) = x


getLegendItems :: Layout1 x y -> ([LegendItem],[LegendItem])
getLegendItems l = (
    concat [ _plot_legend p | (Left p ) <- (_layout1_plots l) ],
    concat [ _plot_legend p | (Right p) <- (_layout1_plots l) ]
    )

renderLegend :: Layout1 x y -> ([LegendItem],[LegendItem]) -> Renderable (Layout1Pick x y)
renderLegend l (lefts,rights) = gridToRenderable g
  where
    g      = besideN [ tval $ mkLegend lefts
                     , weights (1,1) $ tval $ emptyRenderable
                     , tval $ mkLegend rights ]

    lm     = _layout1_margin l

    mkLegend vals = case (_layout1_legend l) of
        Nothing -> emptyRenderable
        Just ls ->  case filter ((/="").fst) vals of
            []  -> emptyRenderable ;
            lvs -> addMargins (0,lm,lm,lm) $
                       mapPickFn L1P_Legend $ legendToRenderable (Legend ls lvs)

layout1LegendsToRenderable :: (Ord x, Ord y) =>
                              Layout1 x y -> Renderable (Layout1Pick x y)
layout1LegendsToRenderable l = renderLegend l (getLegendItems l)

layout1PlotAreaToGrid :: forall x y. (Ord x, Ord y) =>
                          Layout1 x y -> Grid (Renderable (Layout1Pick x y))
layout1PlotAreaToGrid l = layer2 `overlay` layer1
  where
    layer1 = aboveN
         [ besideN [er,     er,  er,    er   ]
         , besideN [er,     er,  er,    er   ]
         , besideN [er,     er,  er,    weights (1,1) plots ]
         ]

    layer2 = aboveN
         [ besideN [er,     er,  er,    ttitle, er,    er,  er       ]
         , besideN [er,     er,  tl,    taxis,  tr,    er,  er       ]
         , besideN [ltitle, lam, laxis, er,     raxis, ram, rtitle   ]
         , besideN [er,     er,  bl,    baxis,  br,    er,  er       ]
         , besideN [er,     er,  er,    btitle, er,    er,  er       ]
         ]
    
    (ttitle,_) = atitle HTA_Centre VTA_Bottom   0 _layout1_top_axis    L1P_TopAxisTitle   
    (btitle,_) = atitle HTA_Centre VTA_Top      0 _layout1_bottom_axis L1P_BottomAxisTitle
    (ltitle,lam) = atitle HTA_Right  VTA_Centre 270 _layout1_left_axis   L1P_LeftAxisTitle
    (rtitle,ram) = atitle HTA_Left   VTA_Centre 270 _layout1_right_axis  L1P_RightAxisTitle

    er = tval $ emptyRenderable
    
    atitle :: HTextAnchor -> VTextAnchor 
            -> Double 
            -> (Layout1 x y -> LayoutAxis z) 
            -> (String -> Layout1Pick x y) 
            -> (Grid (Renderable (Layout1Pick x y)), Grid (Renderable (Layout1Pick x y)))
    atitle ha va rot af pf = if ttext == "" then (er,er) else (label,gap)
      where
        label = tval $ mapPickFn pf $ rlabel tstyle ha va rot ttext
        gap = tval $ spacer (_layout1_margin l,0)
        tstyle = _laxis_title_style (af l)
        ttext  = _laxis_title       (af l)

    plots = tval $ mfill (_layout1_plot_background l) $ plotsToRenderable l
      where
        mfill Nothing   = id
        mfill (Just fs) = fillBackground fs

    (ba,la,ta,ra) = getAxes l
    baxis = tval $ maybe emptyRenderable
                         (mapPickFn L1P_BottomAxis . axisToRenderable) ba
    taxis = tval $ maybe emptyRenderable
                         (mapPickFn L1P_TopAxis .    axisToRenderable) ta
    laxis = tval $ maybe emptyRenderable
                         (mapPickFn L1P_LeftAxis .   axisToRenderable) la
    raxis = tval $ maybe emptyRenderable
                         (mapPickFn L1P_RightAxis .  axisToRenderable) ra

    tl = tval $ axesSpacer fst ta fst la
    bl = tval $ axesSpacer fst ba snd la
    tr = tval $ axesSpacer snd ta fst ra
    br = tval $ axesSpacer snd ba snd ra

plotsToRenderable :: Layout1 x y -> Renderable (Layout1Pick x y)
plotsToRenderable l = Renderable {
        minsize = return (0,0),
        render  = renderPlots l
    }

renderPlots :: Layout1 x y -> RectSize -> ChartBackend (PickFn (Layout1Pick x y))
renderPlots l sz@(w,h) = do
    when (not (_layout1_grid_last l)) renderGrids
    withClipRegion (Rect (Point 0 0) (Point w h)) $ do
      mapM_ rPlot (_layout1_plots l)
    when (_layout1_grid_last l) renderGrids
    return pickfn

  where
    (bAxis,lAxis,tAxis,rAxis) = getAxes l

    rPlot (Left  p) = rPlot1 bAxis lAxis p
    rPlot (Right p) = rPlot1 bAxis rAxis p

    xr = (0, w)
    yr = (h, 0)
    reverse rev (a,b) = if rev then (b,a) else (a,b)

    rPlot1 (Just (AxisT _ xs xrev xaxis)) (Just (AxisT _ ys yrev yaxis)) p =
      let 
          xr1 = reverse xrev xr
          yr1 = reverse yrev yr
          yrange = if yrev then (0, h) else (h, 0)
          pmfn (x,y) = Point (mapv xr1 (_axis_viewport xaxis xr1) x)
                             (mapv yr1 (_axis_viewport yaxis yr1) y)
          mapv (min,max) _ LMin       = min
          mapv (min,max) _ LMax       = max
          mapv _         f (LValue v) = f v
	  in _plot_render p pmfn
    rPlot1 _ _ _ = return ()

    pickfn (Point x y) = do  -- Maybe monad
        xat <- mxat
        (yat1,yat2) <- myats
        return (L1P_PlotArea (mapx xat x) (mapy yat1 y)  (mapy yat2 y))
      where
        mxat = case (bAxis,tAxis) of
            (Just at,_)       -> Just at
            (_,Just at)       -> Just at
            (Nothing,Nothing) -> Nothing
        myats = case (lAxis,rAxis) of
            (Just at,Nothing)   -> Just (at,at)
            (Nothing,Just at)   -> Just (at,at)
            (Just at1,Just at2) -> Just (at1,at2)
            (Nothing,Nothing)   -> Nothing
        mapx (AxisT _ _ rev ad) x = _axis_tropweiv ad (reverse rev xr) x
        mapy (AxisT _ _ rev ad) y = _axis_tropweiv ad (reverse rev yr) y

    renderGrids = do
      maybeM () (renderAxisGrid sz) tAxis
      maybeM () (renderAxisGrid sz) bAxis
      maybeM () (renderAxisGrid sz) lAxis
      maybeM () (renderAxisGrid sz) rAxis

axesSpacer f1 a1 f2 a2 = embedRenderable $ do
    oh1 <- maybeM (0,0) axisOverhang a1
    oh2 <- maybeM (0,0) axisOverhang a2
    return (spacer (f1 oh1, f2 oh2))

getAxes :: Layout1 x y ->
           (Maybe (AxisT x), Maybe (AxisT y), Maybe (AxisT x), Maybe (AxisT y))
getAxes l = (bAxis,lAxis,tAxis,rAxis)
  where
    (xvals0,xvals1,yvals0,yvals1) = allPlottedValues (_layout1_plots l)
    xvals                         = xvals0 ++ xvals1
    (yvals0',yvals1')             = _layout1_yaxes_control l (yvals0,yvals1)

    bAxis = mkAxis E_Bottom (_layout1_bottom_axis l) xvals
    tAxis = mkAxis E_Top    (_layout1_top_axis l)    xvals
    lAxis = mkAxis E_Left   (_layout1_left_axis l)  yvals0'
    rAxis = mkAxis E_Right  (_layout1_right_axis l) yvals1'

    mkAxis t laxis vals = case _laxis_visible laxis vals of
        False -> Nothing
        True  -> Just (AxisT t style rev adata)
      where
        style = _laxis_style laxis
        rev   = _laxis_reverse laxis
        adata = (_laxis_override laxis) (_laxis_generate laxis vals)

allPlottedValues :: [(Either (Plot x y) (Plot x' y'))]
                    -> ( [x], [x'], [y], [y'] )
allPlottedValues plots = (xvals0,xvals1,yvals0,yvals1)
  where
    xvals0 = [ x | (Left p)  <- plots, x <- fst $ _plot_all_points p]
    yvals0 = [ y | (Left p)  <- plots, y <- snd $ _plot_all_points p]
    xvals1 = [ x | (Right p) <- plots, x <- fst $ _plot_all_points p]
    yvals1 = [ y | (Right p) <- plots, y <- snd $ _plot_all_points p]

{-# DEPRECATED defaultLayout1  "Use the according Data.Default instance!" #-}
defaultLayout1 :: (PlotValue x,PlotValue y) => Layout1 x y
defaultLayout1 = def

instance (PlotValue x, PlotValue y) => Default (Layout1 x y) where
  def = Layout1 
    { _layout1_background      = solidFillStyle $ opaque white
    , _layout1_plot_background = Nothing

    , _layout1_title           = ""
    , _layout1_title_style     = def { _font_size   = 15
                                     , _font_weight = FontWeightBold }

    , _layout1_top_axis        = def {_laxis_visible = const False}
    , _layout1_bottom_axis     = def
    , _layout1_left_axis       = def
    , _layout1_right_axis      = def

    , _layout1_yaxes_control   = id

    , _layout1_margin          = 10
    , _layout1_plots           = []
    , _layout1_legend          = Just def
    , _layout1_grid_last       = False
    }

{-# DEPRECATED defaultLayoutAxis "Use the according Data.Default instance!" #-}
defaultLayoutAxis :: PlotValue t => LayoutAxis t
defaultLayoutAxis = def

instance PlotValue t => Default (LayoutAxis t) where
  def = LayoutAxis
    { _laxis_title_style = def { _font_size=10 }
    , _laxis_title       = ""
    , _laxis_style       = def
    , _laxis_visible     = not.null
    , _laxis_generate    = autoAxis
    , _laxis_override    = id
    , _laxis_reverse     = False
    }

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.
$( makeLenses ''Layout1 )
$( makeLenses ''LayoutAxis )
$( makeLenses ''StackedLayouts )

-- | Helper to update all axis styles on a Layout1 simultaneously.
updateAllAxesStyles :: (AxisStyle -> AxisStyle) -> Layout1 x y -> Layout1 x y
updateAllAxesStyles uf = (layout1_top_axis    . laxis_style %~ uf) .
                         (layout1_bottom_axis . laxis_style %~ uf) .
                         (layout1_left_axis   . laxis_style %~ uf) .
                         (layout1_right_axis  . laxis_style %~ uf)

-- | Helper to set the forground color uniformly on a Layout1.
setLayout1Foreground :: AlphaColour Double -> Layout1 x y -> Layout1 x y
setLayout1Foreground fg =
    updateAllAxesStyles  ( (axis_line_style  . line_color .~ fg)
                         . (axis_label_style . font_color .~ fg))
    . (layout1_title_style . font_color .~ fg)
    . (layout1_legend %~ fmap (legend_label_style .> font_color .~ fg))


linkAxes :: ([a], [a]) -> ([a], [a])
linkAxes        (ys1,ys2) = (ys1++ys2,ys1++ys2)

independentAxes :: (a, b) -> (a, b)
independentAxes (ys1,ys2) = (ys1,ys2)
