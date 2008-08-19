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
import Graphics.Rendering.Chart.Table
import Control.Monad
import Control.Monad.Reader (local)

-- | The side of an horizontal axis
data HAxis = HA_Top | HA_Bottom deriving (Eq)

-- | The side of a vertical axis
data VAxis = VA_Left | VA_Right deriving (Eq)

-- | A Layout1 value is a single plot area, with optional: axes on
-- each of the 4 sides; title at the top; legend at the bottom.
data Layout1 x y y' = Layout1 {
    layout1_background :: CairoFillStyle,
    layout1_title :: String,
    layout1_title_style :: CairoFontStyle,
    layout1_horizontal_axes :: AxesFn x x,
    layout1_vertical_axes :: AxesFn y y',
    layout1_margin :: Double,
    layout1_plots :: [(String,Either (Plot x y) (Plot x y'))],
    layout1_legend :: Maybe(LegendStyle),
    layout1_grid_last :: Bool
}

instance (Ord x, Ord y, Ord y') => ToRenderable (Layout1 x y y') where
    toRenderable = layout1ToRenderable

layout1ToRenderable l =
   fillBackground (layout1_background l) (
       renderTable $ aboveN [
          tval $ addMargins (lm/2,0,0,0) () title,
          weights (1,1) $ tval $ addMargins (lm,lm,lm,lm) () plotArea,
          tval $ renderTable $ besideN [ tval $ mkLegend lefts, tval $ emptyRenderable, tval $ mkLegend rights ]
       ] )
  where
    lefts xs = [x | Left x <- xs]
    rights xs = [x | Right x <- xs]
    distrib (a,Left b) = Left (a,b)
    distrib (a,Right b) = Right (a,b)
    lm = layout1_margin l

    title = label (layout1_title_style l) HTA_Centre VTA_Centre (layout1_title l)

    mkLegend side = case (layout1_legend l) of
        Nothing -> emptyRenderable
        (Just ls) -> case [(s,p) | (s,p) <- side (map distrib (layout1_plots l)), not (null s)] of
 	    [] -> emptyRenderable
	    ps -> addMargins (0,lm,lm,0) () (toRenderable (Legend True ls ps))

    layer1 = aboveN [
         besideN [er,        er,    er   ],
         besideN [er,        er,    er   ],
         besideN [er,        er,    weights (1,1) plots ]
         ]

    layer2 = aboveN [
         besideN [er,        er,    atitle ta, er,    er       ],
         besideN [er,        tl,    taxis,     tr,    er       ],
         besideN [atitle la, laxis, er,        raxis, atitle ra],
         besideN [er,        bl,    baxis,     br,    er       ],
         besideN [er,        er,    atitle ba, er,    er       ]
         ]

    plotArea = renderTable (layer2 `overlay` layer1)

    er = tval $ emptyRenderable

    atitle Nothing = er
    atitle (Just (AxisT e a)) | axis_title a == "" = er
                              | otherwise = tval $ rlabel (axis_title_style a) ha va rot (axis_title a)
      where (ha,va,rot) = case e of E_Top -> (HTA_Centre,VTA_Bottom,0)
                                    E_Bottom -> (HTA_Centre,VTA_Top,0)
                                    E_Left -> (HTA_Right,VTA_Centre,90)
                                    E_Right -> (HTA_Left,VTA_Centre,90)

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

    when (not (layout1_grid_last l)) renderGrids
    local (const vectorEnv) $ do
      mapM_ rPlot (layout1_plots l)
    when (layout1_grid_last l) renderGrids
    return (const ())

  where
    (bAxis,lAxis,tAxis,rAxis) = getAxes l

    rPlot (_,Left p) = rPlot1 bAxis lAxis p
    rPlot (_,Right p) = rPlot1 bAxis rAxis p

    rPlot1 (Just (AxisT _ xaxis)) (Just (AxisT _ yaxis)) p = 
	let xrange = (0, w)
	    yrange = (h, 0)
	    pmfn (x,y) = Point (axis_viewport xaxis xrange x) (axis_viewport yaxis yrange y)
	in plot_render p pmfn
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

getAxes :: Layout1 x y y' -> (Maybe (AxisT x), Maybe (AxisT y), Maybe (AxisT x), Maybe (AxisT y'))
getAxes l = (mk E_Bottom bAxis, mk E_Left lAxis,
	     mk E_Top tAxis, mk E_Right rAxis)
  where 
    (xvals0,xvals1,yvals0,yvals1) = allPlottedValues (layout1_plots l)
    (bAxis,tAxis) = layout1_horizontal_axes l xvals0 xvals1
    (lAxis,rAxis) = layout1_vertical_axes l yvals0 yvals1
    mk _ Nothing = Nothing
    mk at (Just a) = Just (AxisT at a)


allPlottedValues :: [(String,Either (Plot x y) (Plot x' y'))] -> ( [x], [x'], [y], [y'] )
allPlottedValues plots = (xvals0,xvals1,yvals0,yvals1)
  where
    xvals0 = [ x | (_, Left p) <- plots, (x,_) <- plot_all_points p]
    yvals0 = [ y | (_, Left p) <- plots, (_,y) <- plot_all_points p]
    xvals1 = [ x | (_, Right p) <- plots, (x,_) <- plot_all_points p]
    yvals1 = [ y | (_, Right p) <- plots, (_,y) <- plot_all_points p]

defaultLayout1 = Layout1 {
    layout1_background = solidFillStyle white,
    layout1_title = "",
    layout1_title_style = defaultFontStyle{font_size=15, font_weight=C.FontWeightBold},
    layout1_horizontal_axes = linkedAxes (autoScaledAxis defaultAxis),
    layout1_vertical_axes = linkedAxes (autoScaledAxis defaultAxis),
    layout1_margin = 10,
    layout1_plots = [],
    layout1_legend = Just defaultLegendStyle,
    layout1_grid_last = False
}

