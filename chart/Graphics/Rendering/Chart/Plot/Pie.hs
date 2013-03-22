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

{-# OPTIONS_GHC -XTemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Pie(
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
import Control.Lens hiding (moveTo)
import Data.Colour
import Data.Colour.Names (black, white)
import Control.Monad

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Legend
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Plot.Types

data PieLayout = PieLayout {
   _pie_title       :: String,
   _pie_title_style :: CairoFontStyle,
   _pie_plot        :: PieChart,
   _pie_background  :: CairoFillStyle,
   _pie_margin      :: Double
}

data PieChart = PieChart {
   _pie_data             :: [PieItem],
   _pie_colors           :: [AlphaColour Double],
   _pie_label_style      :: CairoFontStyle,
   _pie_label_line_style :: CairoLineStyle,
   _pie_start_angle      :: Double

}

data PieItem = PieItem {
   _pitem_label  :: String,
   _pitem_offset :: Double,
   _pitem_value  :: Double
}

defaultPieChart :: PieChart
defaultPieChart = PieChart {
    _pie_data             = [],
    _pie_colors           = defaultColorSeq,
    _pie_label_style      = defaultFontStyle,
    _pie_label_line_style = solidLine 1 $ opaque black,
    _pie_start_angle      = 0
}

defaultPieItem :: PieItem
defaultPieItem = PieItem "" 0 0

defaultPieLayout :: PieLayout
defaultPieLayout = PieLayout {
    _pie_background  = solidFillStyle $ opaque white,
    _pie_title       = "",
    _pie_title_style = defaultFontStyle{ _font_size   = 15
                                       , _font_weight = C.FontWeightBold },
    _pie_plot        = defaultPieChart,
    _pie_margin      = 10
}

instance ToRenderable PieLayout where
    toRenderable p = fillBackground (_pie_background p) (
       gridToRenderable $ aboveN
         [ tval $ addMargins (lm/2,0,0,0) (setPickFn nullPickFn title)
         , weights (1,1) $ tval $ addMargins (lm,lm,lm,lm)
                                             (toRenderable $ _pie_plot p)
         ] )
      where
        title = label (_pie_title_style p) HTA_Centre VTA_Top (_pie_title p)
        lm    = _pie_margin p

instance ToRenderable PieChart where
    toRenderable p = Renderable {
      minsize = minsizePie p,
      render  = renderPie p
    }

extraSpace p = do
    textSizes <- mapM textSize (map _pitem_label (_pie_data p))
    let maxw  = foldr (max.fst) 0 textSizes
    let maxh  = foldr (max.snd) 0 textSizes
    let maxo  = foldr (max._pitem_offset) 0 (_pie_data p)
    let extra = label_rgap + label_rlength + maxo
    return (extra + maxw, extra + maxh )

minsizePie p = do
    (extraw,extrah) <- extraSpace p
    return (extraw * 2, extrah * 2)

renderPie p (w,h) = do
    (extraw,extrah) <- extraSpace p
    let (w,h)  = (p_x p2 - p_x p1, p_y p2 - p_y p1)
    let center = Point (p_x p1 + w/2)  (p_y p1 + h/2)
    let radius = (min (w - 2*extraw) (h - 2*extrah)) / 2

    foldM_ (paint center radius) (_pie_start_angle p)
           (zip (_pie_colors p) content)
    return nullPickFn

    where
        p1 = Point 0 0
        p2 = Point w h
        content = let total = sum (map _pitem_value (_pie_data p))
                  in [ pi{_pitem_value = _pitem_value pi/total}
                     | pi <- _pie_data p ]

        paint :: Point -> Double -> Double -> (AlphaColour Double, PieItem)
                 -> CRender Double
        paint center radius a1 (color,pitem) = do
            let ax     = 360.0 * (_pitem_value pitem)
            let a2     = a1 + (ax / 2)
            let a3     = a1 + ax
            let offset = _pitem_offset pitem

            pieSlice (ray a2 offset) a1 a3 color
            pieLabel (_pitem_label pitem) a2 offset

            return a3

            where
                pieLabel :: String -> Double -> Double -> CRender ()
                pieLabel name angle offset = do
                    setFontStyle (_pie_label_style p)
                    setLineStyle (_pie_label_line_style p)

                    moveTo (ray angle (radius + label_rgap+offset))
                    let p1 = ray angle (radius+label_rgap+label_rlength+offset)
                    lineTo p1
                    (tw,th) <- textSize name
                    let (offset,anchor) = if angle < 90 || angle > 270
                                          then ((0+),HTA_Left)
                                          else ((0-),HTA_Right)
                    c $ C.relLineTo (offset (tw + label_rgap)) 0
                    c $ C.stroke

                    let p2 = p1 `pvadd` (Vector (offset label_rgap) 0)
                    drawText anchor VTA_Bottom p2 name

                pieSlice :: Point -> Double -> Double -> AlphaColour Double
                            -> CRender ()
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
$( makeLenses ''PieLayout )
$( makeLenses ''PieChart )
$( makeLenses ''PieItem )
