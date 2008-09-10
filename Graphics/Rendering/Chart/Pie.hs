{-# OPTIONS_GHC -XTemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Layout
-- Copyright   :  (c) Tim Docker 2008
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- A  basic pie chart.
--
-- Note that template haskell is used to derive accessor functions
-- (see 'Data.Accessor') for each field of the following data types:
--
--     * 'PieLayout'
--
--     * 'PieChart'
--
--     * 'PieItem'
--
-- These accessors are not shown in this API documentation.  They have
-- the same name as the field, but with the trailing underscore
-- dropped. Hence for data field f_::F in type D, they have type
--
-- @
--   f :: Data.Accessor.Accessor D F
-- @
--


module Graphics.Rendering.Chart.Pie(
    PieLayout(..),
    PieChart(..),
    PieItem(..),
    defaultPieLayout,
    defaultPieChart,
    defaultPieItem,

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

import qualified Graphics.Rendering.Cairo as C

import Data.List
import Data.Bits
import Data.Accessor.Template
import Control.Monad 

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Legend
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Grid

data PieLayout = PieLayout {
   pie_title_ :: String,
   pie_title_style_ :: CairoFontStyle,
   pie_plot_ :: PieChart,
   pie_background_ :: CairoFillStyle,
   pie_margin_ :: Double
}

data PieChart = PieChart {
   pie_data_  :: [PieItem],
   pie_colors_ :: [Color],
   pie_label_style_ :: CairoFontStyle,
   pie_label_line_style_ :: CairoLineStyle, 
   pie_start_angle_ :: Double

}

data PieItem = PieItem {
   pitem_label_ :: String,
   pitem_offset_ :: Double,
   pitem_value_ :: Double
}

defaultPieChart = PieChart {
    pie_data_ = [], 
    pie_colors_ = defaultColorSeq,
    pie_label_style_ = defaultFontStyle,
    pie_label_line_style_ = solidLine 1 black,
    pie_start_angle_ = 0
}

defaultPieItem = PieItem "" 0 0

defaultPieLayout = PieLayout {
    pie_background_ = solidFillStyle white,
    pie_title_ = "",
    pie_title_style_ = defaultFontStyle{font_size_=15, font_weight_=C.FontWeightBold},
    pie_plot_ = defaultPieChart,
    pie_margin_ = 10
}

instance ToRenderable PieLayout where
    toRenderable p = fillBackground (pie_background_ p) (
       renderGrid $ aboveN [
       tval $ addMargins (lm/2,0,0,0) title,
       weights (1,1) $ tval $ addMargins (lm,lm,lm,lm) (toRenderable $ pie_plot_ p)
       ] )
      where
        title = label (pie_title_style_ p) HTA_Centre VTA_Top (pie_title_ p)
        lm = pie_margin_ p

instance ToRenderable PieChart where
    toRenderable p = Renderable {
      minsize=minsizePie p,
      render=renderPie p
    }

extraSpace p = do
    textSizes <- mapM textSize (map pitem_label_ (pie_data_ p))
    let maxw = foldr (max.fst) 0 textSizes
    let maxh = foldr (max.snd) 0 textSizes
    let maxo = foldr (max.pitem_offset_) 0 (pie_data_ p)
    let extra = label_rgap + label_rlength + maxo
    return (extra + maxw, extra + maxh )

minsizePie p = do
    (extraw,extrah) <- extraSpace p
    return (extraw * 2, extrah * 2)

renderPie p (w,h) = do
    (extraw,extrah) <- extraSpace p
    let (w,h) = (p_x p2 - p_x p1, p_y p2 - p_y p1)
    let center = Point (p_x p1 + w/2)  (p_y p1 + h/2)
    let radius = (min (w - 2*extraw) (h - 2*extrah)) / 2

    foldM_ (paint center radius) (pie_start_angle_ p) (zip (pie_colors_ p) content)
    return (const Nothing)
 
    where
        p1 = Point 0 0 
        p2 = Point w h 
        content = let total = sum (map pitem_value_ (pie_data_ p))
                  in [ pi{pitem_value_=pitem_value_ pi/total} | pi <- pie_data_ p ]

        paint :: Point -> Double -> Double -> (Color,PieItem) -> CRender Double
        paint center radius a1 (color,pitem) = do
            let ax = 360.0 * (pitem_value_ pitem)
            let a2 = a1 + (ax / 2)
            let a3 = a1 + ax
            let offset = pitem_offset_ pitem

            pieSlice (ray a2 offset) a1 a3 color
            pieLabel (pitem_label_ pitem) a2 offset

            return a3

            where
                pieLabel :: String -> Double -> Double -> CRender ()
                pieLabel name angle offset = do
                    setFontStyle (pie_label_style_ p)
                    setLineStyle (pie_label_line_style_ p)

                    moveTo (ray angle (radius + label_rgap+offset))
                    let p1 = (ray angle (radius + label_rgap + label_rlength+offset))
                    lineTo p1
                    (tw,th) <- textSize name
                    let (offset,anchor) = if angle < 90 || angle > 270 
                                          then ((0+),HTA_Left)
                                          else ((0-),HTA_Right)
                    c $ C.relLineTo (offset (tw + label_rgap))  0
                    c $ C.stroke

                    let p2 = p1 `pvadd` (Vector (offset label_rgap) 0)
                    drawText anchor VTA_Bottom p2 name

                pieSlice :: Point -> Double -> Double -> Color -> CRender ()
                pieSlice (Point x y) a1 a2 color = c $ do 
                    C.newPath
                    C.arc x y radius (radian a1) (radian a2)
                    C.lineTo x y
                    C.lineTo x y
                    C.closePath

                    setSourceColor color
                    C.fillPreserve
                    C.setSourceRGBA 1 1 1 0.1

                    C.stroke

                ray :: Double -> Double -> Point
                ray angle r = Point x' y'
                  where
                    x' = x + (cos' * x'')
                    y' = y + (sin' * x'')
                    cos' = (cos . radian) angle
                    sin' = (sin . radian) angle
                    x''  = ((x + r) - x)
                    x    = p_x center
                    y    = p_y center

                radian = (*(pi / 180.0))


label_rgap = 5
label_rlength = 15

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor for each field
$( deriveAccessors ''PieLayout )
$( deriveAccessors ''PieChart )
$( deriveAccessors ''PieItem )

