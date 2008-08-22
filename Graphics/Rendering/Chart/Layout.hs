-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Layout
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Layout where

import qualified Graphics.Rendering.Cairo as C

import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Plot
import Graphics.Rendering.Chart.Legend
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Grid
import Control.Monad
import Control.Monad.Reader (local)
import Data.Accessor

data AxisPair x = IndependentAxes (Axis x) (Axis x)
                | LinkedAxes AxisMode (Axis x)

data AxisMode = AM_First | AM_Second | AM_Both | AM_Both'

-- | A Layout1 value is a single plot area, with optional: axes on
-- each of the 4 sides; title at the top; legend at the bottom.
data Layout1 x y = Layout1 {

    layout1_background_ :: CairoFillStyle,

    layout1_title_ :: String,
    layout1_title_style_ :: CairoFontStyle,

    layout1_horizontal_axis_ :: Axis x,
    layout1_horizontal_axis_mode_ :: AxisMode,
    layout1_vertical_axes_ :: AxisPair y,

    layout1_left_axis_title_ :: (CairoFontStyle,String),
    layout1_right_axis_title_ :: (CairoFontStyle,String),
    layout1_bottom_axis_title_ :: (CairoFontStyle,String),
    layout1_top_axis_title_ :: (CairoFontStyle,String),

    layout1_margin_ :: Double,
    layout1_plots_ :: [(String,Either (Plot x y) (Plot x y))],
    layout1_legend_ :: Maybe(LegendStyle),
    layout1_grid_last_ :: Bool
}

-- | Accessor for field layout1_background_
layout1_background = accessor (\v->layout1_background_ v) (\a v -> v{layout1_background_=a})

-- | Accessor for field layout1_title_
layout1_title = accessor (\v->layout1_title_ v) (\a v -> v{layout1_title_=a})

-- | Accessor for field layout1_title_style_
layout1_title_style = accessor (\v->layout1_title_style_ v) (\a v -> v{layout1_title_style_=a})

-- | Accessor for field layout1_horizontal_axis_
layout1_horizontal_axis = accessor (\v->layout1_horizontal_axis_ v) (\a v -> v{layout1_horizontal_axis_=a})

-- | Accessor for field layout1_horizontal_axis_mode_
layout1_horizontal_axis_mode = accessor (\v->layout1_horizontal_axis_mode_ v) (\a v -> v{layout1_horizontal_axis_mode_=a})

-- | Accessor for field layout1_vertical_axes_
layout1_vertical_axes = accessor (\v->layout1_vertical_axes_ v) (\a v -> v{layout1_vertical_axes_=a})

-- | Accessor for field layout1_left_axis_title_
layout1_left_axis_title = accessor (\v->layout1_left_axis_title_ v) (\a v -> v{layout1_left_axis_title_=a})

-- | Accessor for field layout1_right_axis_title_
layout1_right_axis_title = accessor (\v->layout1_right_axis_title_ v) (\a v -> v{layout1_right_axis_title_=a})

-- | Accessor for field layout1_bottom_axis_title_
layout1_bottom_axis_title = accessor (\v->layout1_bottom_axis_title_ v) (\a v -> v{layout1_bottom_axis_title_=a})

-- | Accessor for field layout1_top_axis_title_
layout1_top_axis_title = accessor (\v->layout1_top_axis_title_ v) (\a v -> v{layout1_top_axis_title_=a})

-- | Accessor for field layout1_margin_
layout1_margin = accessor (\v->layout1_margin_ v) (\a v -> v{layout1_margin_=a})

-- | Accessor for field layout1_plots_
layout1_plots = accessor (\v->layout1_plots_ v) (\a v -> v{layout1_plots_=a})

-- | Accessor for field layout1_legend_
layout1_legend = accessor (\v->layout1_legend_ v) (\a v -> v{layout1_legend_=a})

-- | Accessor for field layout1_grid_last_
layout1_grid_last = accessor (\v->layout1_grid_last_ v) (\a v -> v{layout1_grid_last_=a})


instance (Ord x, Ord y) => ToRenderable (Layout1 x y) where
    toRenderable = layout1ToRenderable

layout1ToRenderable l =
   fillBackground (layout1_background_ l) (
       renderGrid $ aboveN [
          tval $ addMargins (lm/2,0,0,0) () title,
          weights (1,1) $ tval $ addMargins (lm,lm,lm,lm) () plotArea,
          tval $ renderGrid $ besideN [ tval $ mkLegend lefts, tval $ emptyRenderable, tval $ mkLegend rights ]
       ] )
  where
    lefts xs = [x | Left x <- xs]
    rights xs = [x | Right x <- xs]
    distrib (a,Left b) = Left (a,b)
    distrib (a,Right b) = Right (a,b)
    lm = layout1_margin_ l

    title = label (layout1_title_style_ l) HTA_Centre VTA_Centre (layout1_title_ l)

    mkLegend side = case (layout1_legend_ l) of
        Nothing -> emptyRenderable
        (Just ls) -> case [(s,p) | (s,p) <- side (map distrib (layout1_plots_ l)), not (null s)] of
 	    [] -> emptyRenderable
	    ps -> addMargins (0,lm,lm,0) () (toRenderable (Legend True ls ps))

    layer1 = aboveN [
         besideN [er,        er,    er   ],
         besideN [er,        er,    er   ],
         besideN [er,        er,    weights (1,1) plots ]
         ]

    layer2 = aboveN [
         besideN [er,     er,    ttitle, er,    er       ],
         besideN [er,     tl,    taxis,  tr,    er       ],
         besideN [ltitle, laxis, er,     raxis, rtitle   ],
         besideN [er,     bl,    baxis,  br,    er       ],
         besideN [er,     er,    btitle, er,    er       ]
         ]

    plotArea = renderGrid (layer2 `overlay` layer1)
    ttitle = atitle HTA_Centre VTA_Bottom  0 layout1_top_axis_title_
    btitle = atitle HTA_Centre VTA_Top     0 layout1_bottom_axis_title_
    ltitle = atitle HTA_Right  VTA_Centre 90 layout1_left_axis_title_
    rtitle = atitle HTA_Left   VTA_Centre 90 layout1_right_axis_title_

    er = tval $ emptyRenderable

    atitle ha va rot af = if ttext == "" then er else tval $ rlabel tstyle ha va rot ttext
      where
        (tstyle,ttext) = af l

    plots = tval $Renderable {
        minsize=return (0,0),
        render= renderPlots l
    }

    (ba,la,ta,ra) = getAxes l
    baxis = tval $ maybe emptyRenderable toRenderable ba
    taxis = tval $ maybe emptyRenderable toRenderable ta
    laxis = tval $ maybe emptyRenderable toRenderable la
    raxis = tval $ maybe emptyRenderable toRenderable ra

    tl = tval $ axesSpacer fst ta fst la
    bl = tval $ axesSpacer fst ba snd la
    tr = tval $ axesSpacer snd ta fst ra
    br = tval $ axesSpacer snd ba snd ra


renderPlots l sz@(w,h) = preserveCState $ do
    -- render the plots
    setClipRegion (Point 0 0) (Point w h)

    when (not (layout1_grid_last_ l)) renderGrids
    local (const vectorEnv) $ do
      mapM_ rPlot (layout1_plots_ l)
    when (layout1_grid_last_ l) renderGrids
    return (const ())

  where
    (bAxis,lAxis,tAxis,rAxis) = getAxes l

    rPlot (_,Left p) = rPlot1 bAxis lAxis p
    rPlot (_,Right p) = rPlot1 bAxis rAxis p

    rPlot1 (Just (AxisT _ _ xaxis)) (Just (AxisT _ _ yaxis)) p = 
	let xrange = (0, w)
	    yrange = (h, 0)
	    pmfn (x,y) = Point (axis_viewport_ xaxis xrange x) (axis_viewport_ yaxis yrange y)
	in plot_render_ p pmfn
    rPlot1 _ _ _ = return ()

    renderGrids = do
      maybeM () (renderAxisGrid sz) tAxis
      maybeM () (renderAxisGrid sz) bAxis
      maybeM () (renderAxisGrid sz) lAxis
      maybeM () (renderAxisGrid sz) rAxis

axesSpacer f1 a1 f2 a2 = embedRenderable $ do
    oh1 <- maybeM (0,0) axisOverhang a1
    oh2 <- maybeM (0,0) axisOverhang a2
    return (spacer (f1 oh1, f2 oh2))

getAxes :: Layout1 x y -> (Maybe (AxisT x), Maybe (AxisT y), Maybe (AxisT x), Maybe (AxisT y))
getAxes l = (bAxis,lAxis,tAxis,rAxis)
  where 
    (xvals0,xvals1,yvals0,yvals1) = allPlottedValues (layout1_plots_ l)
    (bAxis,tAxis) = mkLinked E_Bottom E_Top (layout1_horizontal_axis_mode_ l) (layout1_horizontal_axis_ l) (xvals0++xvals1)
    (lAxis,rAxis) = case (layout1_vertical_axes_ l) of
        IndependentAxes a1 a2 -> (mk E_Left (axis_style_ a1) (axis_data_ a1 yvals0),
                                  mk E_Right (axis_style_ a2) (axis_data_ a2 yvals1))

        LinkedAxes am a -> mkLinked E_Left E_Right am a (yvals0++yvals1)

    mkLinked t1 t2 AM_First  a vs = (mk t1 (axis_style_ a) (axis_data_ a vs), Nothing)
    mkLinked t1 t2 AM_Second a vs = (Nothing, mk t2 (axis_style_ a) (axis_data_ a vs))
    mkLinked t1 t2 AM_Both a vs = (mk t1 as ad, mk t2 as ad)
      where
        as = axis_style_ a
        ad = axis_data_ a vs
    mkLinked t1 t2 AM_Both' a vs = (mk t1 as ad, mk t2 as ad2)
      where
        as = axis_style_ a
        ad = axis_data_ a vs
        ad2 = (axis_data_ a vs){axis_labels_=[],axis_grid_=[]}

    mk t as ad = Just (AxisT t as ad)

allPlottedValues :: [(String,Either (Plot x y) (Plot x' y'))] -> ( [x], [x'], [y], [y'] )
allPlottedValues plots = (xvals0,xvals1,yvals0,yvals1)
  where
    xvals0 = [ x | (_, Left p) <- plots, (x,_) <- plot_all_points_ p]
    yvals0 = [ y | (_, Left p) <- plots, (_,y) <- plot_all_points_ p]
    xvals1 = [ x | (_, Right p) <- plots, (x,_) <- plot_all_points_ p]
    yvals1 = [ y | (_, Right p) <- plots, (_,y) <- plot_all_points_ p]

defaultLayout1 = Layout1 {
    layout1_background_ = solidFillStyle white,

    layout1_title_ = "",
    layout1_title_style_ = defaultFontStyle{font_size_=15, font_weight_=C.FontWeightBold},

    layout1_horizontal_axis_ = (Axis defaultAxisStyle autoScaledAxis),
    layout1_horizontal_axis_mode_ = AM_Both,
    layout1_vertical_axes_ = LinkedAxes AM_Both (Axis defaultAxisStyle autoScaledAxis),

    layout1_left_axis_title_ = (defaultFontStyle{font_size_=10},""),
    layout1_right_axis_title_ = (defaultFontStyle{font_size_=10},""),
    layout1_bottom_axis_title_ = (defaultFontStyle{font_size_=10},""),
    layout1_top_axis_title_ = (defaultFontStyle{font_size_=10},""),

    layout1_margin_ = 10,
    layout1_plots_ = [],
    layout1_legend_ = Just defaultLegendStyle,
    layout1_grid_last_ = False
}

