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
import Graphics.Rendering.Chart.Renderable
import Control.Monad

-- | The side of an horizontal axis
data HAxis = HA_Top | HA_Bottom deriving (Eq)

-- | The side of a vertical axis
data VAxis = VA_Left | VA_Right deriving (Eq)

-- | A Layout1 value is a single plot area, with optional: axes on
-- each of the 4 sides; title at the top; legend at the bottom.
data Layout1 = Layout1 {
    layout1_background :: CairoFillStyle,
    layout1_title :: String,
    layout1_title_style :: CairoFontStyle,
    layout1_horizontal_axes :: AxesFn,
    layout1_vertical_axes :: AxesFn,
    layout1_margin :: Double,
    layout1_plots :: [(String,HAxis,VAxis,Plot)],
    layout1_legend :: Maybe(LegendStyle)
}

instance ToRenderable Layout1 where
    toRenderable = layout1ToRenderable

layout1ToRenderable l =
   fillBackground (layout1_background l) (
       vertical [
       (0, addMargins (lm/2,0,0,0)    title),
       (1, addMargins (lm,lm,lm,lm) plotArea),
       (0, horizontal [ (0,mkLegend VA_Left),(1,emptyRenderable),(0, mkLegend VA_Right) ] )
       ]
     )
  where
    lm = layout1_margin l

    title = label (layout1_title_style l) HTA_Centre VTA_Centre (layout1_title l)

    mkLegend va = case (layout1_legend l) of
        Nothing -> emptyRenderable
        (Just ls) -> case [(s,p) | (s,_,va',p) <- layout1_plots l, va' == va, not (null s)] of
 	    [] -> emptyRenderable
	    ps -> addMargins (0,lm,lm,0) (toRenderable (Legend True ls ps))

    plotArea = grid [0,0,1,0,0] [0,0,1,0,0]
       [ [er,            er,        (1,atitle ta), er,        er       ],
         [er,            (1,tl),    (1,taxis),     (1,tr),    er       ],
         [(1,atitle la), (1,laxis), (0,plots),     (1,raxis), (1,atitle ra)],
         [er,            (1,bl),    (1,baxis),     (1,br),    er       ],
         [er,            er,        (1,atitle ba), er,        er       ] ]

    atitle Nothing = emptyRenderable
    atitle (Just (AxisT e a)) = rlabel (axis_title_style a) ha va rot (axis_title a)
      where (ha,va,rot) = case e of E_Top -> (HTA_Centre,VTA_Bottom,0)
                                    E_Bottom -> (HTA_Centre,VTA_Top,0)
                                    E_Left -> (HTA_Right,VTA_Centre,90)
                                    E_Right -> (HTA_Left,VTA_Centre,90)

    plots = Renderable {
        minsize=return (0,0),
        render=renderPlots l
    }

    (ba,la,ta,ra) = getAxes l
    baxis = maybe emptyRenderable toRenderable ba
    taxis = maybe emptyRenderable toRenderable ta
    laxis = maybe emptyRenderable toRenderable la
    raxis = maybe emptyRenderable toRenderable ra

    tl = axesSpacer fst ta fst la
    bl = axesSpacer fst ba snd la
    tr = axesSpacer snd ta fst ra
    br = axesSpacer snd ba snd ra

    er = (0,emptyRenderable)

renderPlots l r@(Rect p1 p2) = preserveCState $ do
    -- render the plots
    setClipRegion p1 p2 
    mapM_ (rPlot r) (layout1_plots l)

    -- render the axes grids
    maybeM () (renderAxisGrid r) tAxis
    maybeM () (renderAxisGrid r) bAxis
    maybeM () (renderAxisGrid r) lAxis
    maybeM () (renderAxisGrid r) rAxis

  where
    (bAxis,lAxis,tAxis,rAxis) = getAxes l

    rPlot :: Rect -> (String,HAxis,VAxis,Plot) -> CRender ()
    rPlot rect (_,ha,va,p) = 
        let mxaxis = case ha of HA_Bottom -> bAxis
				HA_Top    -> tAxis
	    myaxis = case va of VA_Left   -> lAxis
				VA_Right  -> rAxis
        in rPlot1 rect mxaxis myaxis p
	      
    rPlot1 :: Rect -> Maybe AxisT -> Maybe AxisT -> Plot -> CRender ()
    rPlot1 (Rect dc1 dc2) (Just (AxisT _ xaxis)) (Just (AxisT _ yaxis)) p = 
	let xrange = (p_x dc1, p_x dc2)
	    yrange = (p_y dc2, p_y dc1)
	    pmfn (Point x y) = Point (axis_viewport xaxis xrange x) (axis_viewport yaxis yrange y)
	in plot_render p pmfn
    rPlot1 _ _ _ _ = return ()

axesSpacer f1 a1 f2 a2 = embedRenderable $ do
    oh1 <- maybeM (0,0) axisOverhang a1
    oh2 <- maybeM (0,0) axisOverhang a2
    return (spacer (f1 oh1, f2 oh2))

maybeM v = maybe (return v)

getAxes :: Layout1 -> (Maybe AxisT, Maybe AxisT, Maybe AxisT, Maybe AxisT)
getAxes l = (mk E_Bottom bAxis, mk E_Left lAxis,
	     mk E_Top tAxis, mk E_Right rAxis)
  where 
    (xvals0,xvals1,yvals0,yvals1) = allPlottedValues (layout1_plots l)
    (bAxis,tAxis) = layout1_horizontal_axes l xvals0 xvals1
    (lAxis,rAxis) = layout1_vertical_axes l yvals0 yvals1
    mk _ Nothing = Nothing
    mk at (Just a) = Just (AxisT at a)


allPlottedValues :: [(String,HAxis,VAxis,Plot)] -> ( [Double], [Double], [Double], [Double] )
allPlottedValues plots = (xvals0,xvals1,yvals0,yvals1)
  where
    pts = concat [ [ (ha,va,pt)| pt <- plot_all_points p] | (_,ha,va,p) <- plots ]
    xvals0 = [ (p_x pt) | (HA_Bottom,_,pt) <- pts  ]
    xvals1 = [ (p_x pt) | (HA_Top,_,pt) <- pts  ]
    yvals0 = [ (p_y pt) | (_,VA_Left,pt) <- pts  ]
    yvals1 = [ (p_y pt) | (_,VA_Right,pt) <- pts  ]


defaultLayout1 = Layout1 {
    layout1_background = solidFillStyle white,
    layout1_title = "",
    layout1_title_style = fontStyle "sans" 15 C.FontSlantNormal C.FontWeightBold,
    layout1_horizontal_axes = linkedAxes (autoScaledAxis defaultAxis),
    layout1_vertical_axes = linkedAxes (autoScaledAxis defaultAxis),
    layout1_margin = 10,
    layout1_plots = [],
    layout1_legend = Just defaultLegendStyle
}

