module Graphics.Rendering.Chart.Pie where
-- original code thanks to Neal Alexander

import qualified Graphics.Rendering.Cairo as C

import Data.List
import Data.Bits
import Control.Monad 

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Renderable

data PieLayout = PieLayout {
   pie_title :: String,
   pie_title_style :: CairoFontStyle,
   pie_plot :: PieChart,
   pie_background :: CairoFillStyle,
   pie_margin :: Double
}

data PieChart = PieChart {
   pie_data  :: [PieItem],
   pie_colors :: [Color],
   pie_label_style :: CairoFontStyle,
   pie_label_line_style :: CairoLineStyle, 
   pie_start_angle :: Double

}

data PieItem = PieItem {
   pitem_label :: String,
   pitem_offset :: Double,
   pitem_value :: Double
}

defaultPieChart = PieChart {
    pie_data = [], 
    pie_colors = defaultColorSeq,
    pie_label_style = defaultFontStyle,
    pie_label_line_style = solidLine 1 black,
    pie_start_angle = 0
}

defaultPieItem = PieItem "" 0 0

defaultPieLayout = PieLayout {
    pie_background = solidFillStyle white,
    pie_title = "",
    pie_title_style = defaultFontStyle{font_size=15, font_weight=C.FontWeightBold},
    pie_plot = defaultPieChart,
    pie_margin = 10
}

instance ToRenderable PieLayout where
    toRenderable p = fillBackground (pie_background p) (
       vertical [
       (0, addMargins (lm/2,0,0,0)  title),
       (1, addMargins (lm,lm,lm,lm) (toRenderable $ pie_plot p))
       ] )
      where
        title = label (pie_title_style p) HTA_Centre VTA_Top (pie_title p)
        lm = pie_margin p

instance ToRenderable PieChart where
    toRenderable p = Renderable {
      minsize=minsizePie p,
      render=renderPie p
    }

extraSpace p = do
    textSizes <- mapM textSize (map pitem_label (pie_data p))
    let maxw = foldr (max.fst) 0 textSizes
    let maxh = foldr (max.snd) 0 textSizes
    let maxo = foldr (max.pitem_offset) 0 (pie_data p)
    let extra = label_rgap + label_rlength + maxo
    return (extra + maxw, extra + maxh )

minsizePie p = do
    (extraw,extrah) <- extraSpace p
    return (extraw * 2, extrah * 2)

renderPie p (Rect p1 p2) = do
    (extraw,extrah) <- extraSpace p
    let (w,h) = (p_x p2 - p_x p1, p_y p2 - p_y p1)
    let center = Point (p_x p1 + w/2)  (p_y p1 + h/2)
    let radius = (min (w - 2*extraw) (h - 2*extrah)) / 2

    foldM_ (paint center radius) (pie_start_angle p) (zip (pie_colors p) content)
 
    where
        content = let total = sum (map pitem_value (pie_data p))
                  in [ pi{pitem_value=pitem_value pi/total} | pi <- pie_data p ]

        paint :: Point -> Double -> Double -> (Color,PieItem) -> CRender Double
        paint center radius a1 (color,pitem) = do
            let ax = 360.0 * (pitem_value pitem)
            let a2 = a1 + (ax / 2)
            let a3 = a1 + ax
            let offset = pitem_offset pitem

            pieSlice (ray a2 offset) a1 a3 color
            pieLabel (pitem_label pitem) a2 offset

            return a3

            where
                pieLabel :: String -> Double -> Double -> CRender ()
                pieLabel name angle offset = do
                    setFontStyle (pie_label_style p)
                    setLineStyle (pie_label_line_style p)

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
