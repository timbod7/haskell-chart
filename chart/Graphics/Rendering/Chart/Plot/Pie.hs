-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Pie
-- Copyright   :  (c) Tim Docker 2008, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- A  basic pie chart.
--
-- Pie charts are handled different to other plots, in that they
-- have their own layout, and can't be composed with other plots. A
-- pie chart is rendered with code in the following form:
--
-- @
-- values :: [PieItem]
-- values = [...]
-- layout :: PieLayout
-- layout = pie_plot ^: pie_data ^= values
--        $ def
-- renderable = toRenderable layout
-- @
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Pie(
    PieLayout(..),
    PieChart(..),
    PieItem(..),
    defaultPieLayout,
    defaultPieChart,
    defaultPieItem,
    
    pieToRenderable,
    pieChartToRenderable,

    pie_title,
    pie_title_style,
    pie_plot,
    pie_background,
    pie_margin,
    pie_data,
    pie_colors,
    pie_label_style,
    pie_label_line_style,
    pie_start_angle,
    pitem_label,
    pitem_offset,
    pitem_value,

) where
-- original code thanks to Neal Alexander

-- see ../Drawing.hs for why we do not use hiding (moveTo) for
-- lens < 4
import Control.Lens
import Data.Colour
import Data.Colour.Names (white)
import Data.Monoid
import Data.Default.Class
import Control.Monad

import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Grid

data PieLayout = PieLayout {
   _pie_title       :: String,
   _pie_title_style :: FontStyle,
   _pie_plot        :: PieChart,
   _pie_background  :: FillStyle,
   _pie_margin      :: Double
}

data PieChart = PieChart {
   _pie_data             :: [PieItem],
   _pie_colors           :: [AlphaColour Double],
   _pie_label_style      :: FontStyle,
   _pie_label_line_style :: LineStyle, 
   _pie_start_angle      :: Double

}

data PieItem = PieItem {
   _pitem_label  :: String,
   _pitem_offset :: Double,
   _pitem_value  :: Double
}

{-# DEPRECATED defaultPieChart  "Use the according Data.Default instance!" #-}
defaultPieChart :: PieChart
defaultPieChart = def

instance Default PieChart where
  def = PieChart 
    { _pie_data             = []
    , _pie_colors           = defaultColorSeq
    , _pie_label_style      = def
    , _pie_label_line_style = solidLine 1 $ opaque black
    , _pie_start_angle      = 0
    }

{-# DEPRECATED defaultPieItem  "Use the according Data.Default instance!" #-}
defaultPieItem :: PieItem
defaultPieItem = def

instance Default PieItem where
  def = PieItem "" 0 0

{-# DEPRECATED defaultPieLayout  "Use the according Data.Default instance!" #-}
defaultPieLayout :: PieLayout
defaultPieLayout = def

instance Default PieLayout where
  def = PieLayout 
    { _pie_background  = solidFillStyle $ opaque white
    , _pie_title       = ""
    , _pie_title_style = def { _font_size   = 15
                             , _font_weight = FontWeightBold }
    , _pie_plot        = def
    , _pie_margin      = 10
    }

instance ToRenderable PieLayout where
  toRenderable = setPickFn nullPickFn . pieToRenderable

pieChartToRenderable :: PieChart -> Renderable (PickFn a)
pieChartToRenderable p = Renderable { minsize = minsizePie p
                                    , render  = renderPie p
                                    }

instance ToRenderable PieChart where
  toRenderable = setPickFn nullPickFn . pieChartToRenderable

pieToRenderable :: PieLayout -> Renderable (PickFn a)
pieToRenderable p = fillBackground (_pie_background p) (
       gridToRenderable $ aboveN
         [ tval $ addMargins (lm/2,0,0,0) (setPickFn nullPickFn title)
         , weights (1,1) $ tval $ addMargins (lm,lm,lm,lm)
                                             (pieChartToRenderable $ _pie_plot p)
         ] )
      where
        title = label (_pie_title_style p) HTA_Centre VTA_Top (_pie_title p)
        lm    = _pie_margin p

extraSpace :: PieChart -> ChartBackend (Double, Double)
extraSpace p = do
    textSizes <- mapM (textDimension . _pitem_label) (_pie_data p)
    let maxw  = foldr (max.fst) 0 textSizes
    let maxh  = foldr (max.snd) 0 textSizes
    let maxo  = foldr (max._pitem_offset) 0 (_pie_data p)
    let extra = label_rgap + label_rlength + maxo
    return (extra + maxw, extra + maxh )

minsizePie :: PieChart -> ChartBackend (Double, Double)
minsizePie p = do
    (extraw,extrah) <- extraSpace p
    return (extraw * 2, extrah * 2)

renderPie :: PieChart -> (Double, Double) -> ChartBackend (PickFn a)
renderPie p (w,h) = do
    (extraw,extrah) <- extraSpace p
    -- let (w,h)  = (p_x p2 - p_x p1, p_y p2 - p_y p1)
    -- let center = Point (p_x p1 + w/2)  (p_y p1 + h/2)
    --
    let center = Point (w/2) (h/2)
    let radius = min (w - 2*extraw) (h - 2*extrah) / 2

    foldM_ (paint center radius) (_pie_start_angle p)
           (zip (_pie_colors p) content)
    return nullPickFn
 
    where
        -- p1 = Point 0 0 
        -- p2 = Point w h 
        content = let total = sum (map _pitem_value (_pie_data p))
                  in [ pitem{_pitem_value=_pitem_value pitem/total}
                     | pitem <- _pie_data p ]

        paint :: Point -> Double -> Double -> (AlphaColour Double, PieItem)
              -> ChartBackend Double
        paint center radius a1 (color,pitem) = do
            let ax     = 360.0 * _pitem_value pitem
            let a2     = a1 + (ax / 2)
            let a3     = a1 + ax
            let offset = _pitem_offset pitem

            pieSlice (ray a2 offset) a1 a3 color
            pieLabel (_pitem_label pitem) a2 offset

            return a3

            where
                pieLabel :: String -> Double -> Double -> ChartBackend ()
                pieLabel name angle offset = 
                    withFontStyle (_pie_label_style p) $ 
                      withLineStyle (_pie_label_line_style p) $ do
                        let p1 = ray angle (radius+label_rgap+label_rlength+offset)
                        p1a <- alignStrokePoint p1
                        (tw,_) <- textDimension name
                        let (offset',anchor) = if angle < 90 || angle > 270 
                                              then ((0+),HTA_Left)
                                              else ((0-),HTA_Right)
                        p0 <- alignStrokePoint $ ray angle (radius + label_rgap+offset)
                        strokePath $ moveTo p0
                                  <> lineTo p1a
                                  <> lineTo' (p_x p1a + offset' (tw + label_rgap)) (p_y p1a)

                        let p2 = p1 `pvadd` Vector (offset' label_rgap) 0
                        drawTextA anchor VTA_Bottom p2 name

                pieSlice :: Point -> Double -> Double -> AlphaColour Double -> ChartBackend ()
                pieSlice (Point x y) arc1 arc2 pColor = do
                    let path = arc' x y radius (radian arc1) (radian arc2)
                            <> lineTo' x y
                            <> lineTo' x y
                            <> close

                    withFillStyle (FillStyleSolid pColor) $ 
                      fillPath path
                    withLineStyle (def { _line_color = withOpacity white 0.1 }) $ 
                      strokePath path

                ray :: Double -> Double -> Point
                ray angle r = Point x' y'
                  where
                    x'   = x + (cos' * x'')
                    y'   = y + (sin' * x'')
                    cos' = (cos . radian) angle
                    sin' = (sin . radian) angle
                    -- TODO: is x'' defined in this way to try and avoid
                    --       numerical rounding?
                    x''  = (x + r) - x
                    x    = p_x center
                    y    = p_y center

                radian = (*(pi / 180.0))

label_rgap, label_rlength :: Double
label_rgap = 5
label_rlength = 15

$( makeLenses ''PieLayout )
$( makeLenses ''PieChart )
$( makeLenses ''PieItem )
