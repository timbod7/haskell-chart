-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Pie
-- Copyright   :  (c) Tim Docker 2008
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
--        $ defaultPieLayout
-- renderable = toRenderable layout
-- @
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}

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

import Data.List
import Data.Bits
import Data.Accessor.Template
import Data.Colour
import Data.Colour.Names (black, white)
import Data.Monoid
import Data.Default
import Control.Monad

import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Legend
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Plot.Types

data PieLayout = PieLayout {
   pie_title_       :: String,
   pie_title_style_ :: FontStyle,
   pie_plot_        :: PieChart,
   pie_background_  :: FillStyle,
   pie_margin_      :: Double
}

data PieChart = PieChart {
   pie_data_             :: [PieItem],
   pie_colors_           :: [AlphaColour Double],
   pie_label_style_      :: FontStyle,
   pie_label_line_style_ :: LineStyle, 
   pie_start_angle_      :: Double

}

data PieItem = PieItem {
   pitem_label_  :: String,
   pitem_offset_ :: Double,
   pitem_value_  :: Double
}

{-# DEPRECATED defaultPieChart  "Use the according Data.Default instance!" #-}
defaultPieChart :: PieChart
defaultPieChart = def

instance Default PieChart where
  def = PieChart 
    { pie_data_             = []
    , pie_colors_           = defaultColorSeq
    , pie_label_style_      = def
    , pie_label_line_style_ = solidLine 1 $ opaque black
    , pie_start_angle_      = 0
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
    { pie_background_  = solidFillStyle $ opaque white
    , pie_title_       = ""
    , pie_title_style_ = def { font_size_   = 15
                             , font_weight_ = FontWeightBold }
    , pie_plot_        = defaultPieChart
    , pie_margin_      = 10
    }

pieChartToRenderable :: (ChartBackend m) => PieChart -> Renderable m ()
pieChartToRenderable p = Renderable { minsize = minsizePie p
                                    , render  = renderPie p
                                    }

pieToRenderable :: (ChartBackend m) => PieLayout -> Renderable m ()
pieToRenderable p = fillBackground (pie_background_ p) (
       gridToRenderable $ aboveN
         [ tval $ addMargins (lm/2,0,0,0) (setPickFn nullPickFn title)
         , weights (1,1) $ tval $ addMargins (lm,lm,lm,lm)
                                             (pieChartToRenderable $ pie_plot_ p)
         ] )
      where
        title = label (pie_title_style_ p) HTA_Centre VTA_Top (pie_title_ p)
        lm    = pie_margin_ p

extraSpace :: (ChartBackend m) => PieChart -> m (Double, Double)
extraSpace p = do
    textSizes <- mapM textDimension (map pitem_label_ (pie_data_ p))
    let maxw  = foldr (max.fst) 0 textSizes
    let maxh  = foldr (max.snd) 0 textSizes
    let maxo  = foldr (max.pitem_offset_) 0 (pie_data_ p)
    let extra = label_rgap + label_rlength + maxo
    return (extra + maxw, extra + maxh )

minsizePie :: (ChartBackend m) => PieChart -> m (Double, Double)
minsizePie p = do
    (extraw,extrah) <- extraSpace p
    return (extraw * 2, extrah * 2)

renderPie :: (ChartBackend m) => PieChart -> (Double, Double) -> m (PickFn a)
renderPie p (w,h) = do
    (extraw,extrah) <- extraSpace p
    let (w,h)  = (p_x p2 - p_x p1, p_y p2 - p_y p1)
    let center = Point (p_x p1 + w/2)  (p_y p1 + h/2)
    let radius = (min (w - 2*extraw) (h - 2*extrah)) / 2

    foldM_ (paint center radius) (pie_start_angle_ p)
           (zip (pie_colors_ p) content)
    return nullPickFn
 
    where
        p1 = Point 0 0 
        p2 = Point w h 
        content = let total = sum (map pitem_value_ (pie_data_ p))
                  in [ pi{pitem_value_=pitem_value_ pi/total}
                     | pi <- pie_data_ p ]

        paint :: (ChartBackend m) 
              => Point -> Double -> Double -> (AlphaColour Double, PieItem)
              -> m Double
        paint center radius a1 (color,pitem) = do
            let ax     = 360.0 * (pitem_value_ pitem)
            let a2     = a1 + (ax / 2)
            let a3     = a1 + ax
            let offset = pitem_offset_ pitem

            pieSlice (ray a2 offset) a1 a3 color
            pieLabel (pitem_label_ pitem) a2 offset

            return a3

            where
                pieLabel :: (ChartBackend m) => String -> Double -> Double -> m ()
                pieLabel name angle offset = do
                    withFontStyle (pie_label_style_ p) $ do
                      withLineStyle (pie_label_line_style_ p) $ do
                        let p1 = ray angle (radius+label_rgap+label_rlength+offset)
                        p1a <- alignp $ p1
                        (tw,th) <- textDimension name
                        let (offset',anchor) = if angle < 90 || angle > 270 
                                              then ((0+),HTA_Left)
                                              else ((0-),HTA_Right)
                        p0 <- alignp $ ray angle (radius + label_rgap+offset)
                        strokePath $ moveTo p0
                                  <> lineTo p1a
                                  <> lineTo' (p_x p1a + (offset' (tw + label_rgap))) (p_y p1a)

                        let p2 = p1 `pvadd` (Vector (offset' label_rgap) 0)
                        drawTextA anchor VTA_Bottom p2 name

                pieSlice :: (ChartBackend m) => Point -> Double -> Double -> AlphaColour Double
                            -> m ()
                pieSlice (Point x y) a1 a2 color = do
                    let path = arc' x y radius (radian a1) (radian a2)
                            <> lineTo' x y
                            <> lineTo' x y
                            <> close

                    withFillStyle (FillStyleSolid color) $ do
                      fillPath path
                    ls <- getLineStyle
                    withLineStyle (ls { line_color_ = withOpacity white 0.1 }) $ do
                      strokePath path

                ray :: Double -> Double -> Point
                ray angle r = Point x' y'
                  where
                    x'   = x + (cos' * x'')
                    y'   = y + (sin' * x'')
                    cos' = (cos . radian) angle
                    sin' = (sin . radian) angle
                    x''  = ((x + r) - x)
                    x    = p_x center
                    y    = p_y center

                radian = (*(pi / 180.0))


label_rgap = 5
label_rlength = 15

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.
$( deriveAccessors ''PieLayout )
$( deriveAccessors ''PieChart )
$( deriveAccessors ''PieItem )
