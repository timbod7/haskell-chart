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

{-# OPTIONS_GHC -XTemplateHaskell #-}

module Graphics.Rendering.Chart.Layout(
    Layout1(..),
    LayoutAxis(..),
    MAxisFn,

    defaultLayout1,

    mAxis,
    noAxis,

    updateAllAxesStyles,
    updateXAxesData,
    updateYAxesData,
    setForeground,

    laxis_title_style,
    laxis_title,
    laxis_style,
    laxis_data,

    layout1_background,
    layout1_title,
    layout1_title_style,
    layout1_left_axis,
    layout1_right_axis,
    layout1_top_axis,
    layout1_bottom_axis,
    layout1_margin,
    layout1_plots,
    layout1_legend,
    layout1_grid_last,
  ) where

import qualified Graphics.Rendering.Cairo as C

import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Plot
import Graphics.Rendering.Chart.Legend
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Grid
import Control.Monad
import Control.Monad.Reader (local)
import Data.Accessor.Template
import Data.Accessor

-- | A @MAxisFn@ is a function that generates an (optional) axis
-- given the points plotted against that axis.
type MAxisFn t = [t] -> Maybe (AxisData t)

data LayoutAxis x = LayoutAxis {
   laxis_title_style_ :: CairoFontStyle,
   laxis_title_ :: String,
   laxis_style_ :: AxisStyle,
   laxis_data_ :: MAxisFn x
}

-- | A Layout1 value is a single plot area, with optional: axes on
-- each of the 4 sides; title at the top; legend at the bottom. It's
-- parameterised by the types of values to be plotted the horizonal
-- and vertical axes.
data Layout1 x y = Layout1 {

    layout1_background_ :: CairoFillStyle,

    layout1_title_ :: String,
    layout1_title_style_ :: CairoFontStyle,

    layout1_bottom_axis_ :: LayoutAxis x,
    layout1_top_axis_ :: LayoutAxis x,
    layout1_left_axis_ :: LayoutAxis y,
    layout1_right_axis_ :: LayoutAxis y,

    layout1_margin_ :: Double,
    layout1_plots_ :: [(String,Either (Plot x y) (Plot x y))],
    layout1_legend_ :: Maybe(LegendStyle),
    layout1_grid_last_ :: Bool
}

instance (Ord x, Ord y) => ToRenderable (Layout1 x y) where
    toRenderable = layout1ToRenderable


layout1ToRenderable :: (Ord x, Ord y) => Layout1 x y -> Renderable ()
layout1ToRenderable l =
   fillBackground (layout1_background_ l) (
       gridToRenderable $ aboveN [
          tval $ addMargins (lm/2,0,0,0) title,
          weights (1,1) $ tval $ addMargins (lm,lm,lm,lm) plotArea,
          tval $ gridToRenderable (besideN [ tval $ mkLegend lefts, tval $ emptyRenderable, tval $ mkLegend rights ])
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
	    ps -> addMargins (0,lm,lm,0) (toRenderable (Legend True ls ps))

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

    plotArea = gridToRenderable (layer2 `overlay` layer1)
    ttitle = atitle HTA_Centre VTA_Bottom  0 layout1_top_axis_
    btitle = atitle HTA_Centre VTA_Top     0 layout1_bottom_axis_
    ltitle = atitle HTA_Right  VTA_Centre 90 layout1_left_axis_
    rtitle = atitle HTA_Left   VTA_Centre 90 layout1_right_axis_

    er = tval $ emptyRenderable

    atitle ha va rot af = if ttext == "" then er else tval $ rlabel tstyle ha va rot ttext
      where
        tstyle = laxis_title_style_ (af l)
        ttext = laxis_title_ (af l)

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
    return (const Nothing)

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
    xvals = xvals0 ++ xvals1
    yvals = yvals0 ++ yvals1

    linkYAxes = (null yvals0) || (null yvals1)

    bAxis = mkAxis E_Bottom (layout1_bottom_axis_ l) xvals
    tAxis = mkAxis E_Top (layout1_top_axis_ l) xvals
    lAxis = mkAxis E_Left (layout1_left_axis_ l) ys
      where
        ys = if linkYAxes then yvals else yvals0
    rAxis = mkAxis E_Right (layout1_right_axis_ l) ys
      where
        ys = if linkYAxes then yvals else yvals1

    mkAxis t laxis vals = do
        adata <- laxis_data_ laxis vals
        return (AxisT t (laxis_style_ laxis) adata)

allPlottedValues :: [(String,Either (Plot x y) (Plot x' y'))] -> ( [x], [x'], [y], [y'] )
allPlottedValues plots = (xvals0,xvals1,yvals0,yvals1)
  where
    xvals0 = [ x | (_, Left p) <- plots, (x,_) <- plot_all_points_ p]
    yvals0 = [ y | (_, Left p) <- plots, (_,y) <- plot_all_points_ p]
    xvals1 = [ x | (_, Right p) <- plots, (x,_) <- plot_all_points_ p]
    yvals1 = [ y | (_, Right p) <- plots, (_,y) <- plot_all_points_ p]

defaultLayout1 :: (PlotValue x,PlotValue y) => Layout1 x y
defaultLayout1 = Layout1 {
    layout1_background_ = solidFillStyle white,

    layout1_title_ = "",
    layout1_title_style_ = defaultFontStyle{font_size_=15, font_weight_=C.FontWeightBold},

    layout1_top_axis_ = defaultLayoutAxis,
    layout1_bottom_axis_ = defaultLayoutAxis,
    layout1_left_axis_ = defaultLayoutAxis,
    layout1_right_axis_ = defaultLayoutAxis,

    layout1_margin_ = 10,
    layout1_plots_ = [],
    layout1_legend_ = Just defaultLegendStyle,
    layout1_grid_last_ = False
}

defaultLayoutAxis :: PlotValue t => LayoutAxis t
defaultLayoutAxis = LayoutAxis {
   laxis_title_style_ = defaultFontStyle{font_size_=10},
   laxis_title_ = "",
   laxis_style_ = defaultAxisStyle,
   laxis_data_ = mAxis autoAxis
}

mAxis :: PlotValue t => AxisFn t -> MAxisFn t
mAxis axisfn [] = Nothing
mAxis axisfn ps = Just (axisfn ps)

noAxis :: PlotValue t => LayoutAxis t
noAxis =  LayoutAxis {
   laxis_title_style_ = defaultFontStyle{font_size_=10},
   laxis_title_ = "",
   laxis_style_ = defaultAxisStyle,
   laxis_data_ = const Nothing
}

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor for each field
$( deriveAccessors ''Layout1 )
$( deriveAccessors ''LayoutAxis )

updateAllAxesStyles :: (AxisStyle -> AxisStyle) -> Layout1 x y -> Layout1 x y
updateAllAxesStyles uf = (layout1_top_axis .> laxis_style ^: uf) .
                         (layout1_bottom_axis .> laxis_style ^: uf) .
                         (layout1_left_axis .> laxis_style ^: uf) .
                         (layout1_right_axis .> laxis_style ^: uf)

updateXAxesData :: (MAxisFn x -> MAxisFn x) -> Layout1 x y -> Layout1 x y
updateXAxesData uf = (layout1_top_axis .> laxis_data ^: uf) .
                     (layout1_bottom_axis .> laxis_data ^: uf)

updateYAxesData :: (MAxisFn y -> MAxisFn y) -> Layout1 x y -> Layout1 x y
updateYAxesData uf = (layout1_left_axis .> laxis_data ^: uf) .
                     (layout1_right_axis .> laxis_data ^: uf)
                         

setForeground :: Color -> Layout1 x y -> Layout1 x y
setForeground fg = updateAllAxesStyles  (
                       (axis_line_style .> line_color ^= fg).
                       (axis_label_style .> font_color ^= fg)
                       )
                 . (layout1_title_style .> font_color ^= fg)
                 . (layout1_legend ^: fmap (legend_label_style .> font_color ^= fg))


