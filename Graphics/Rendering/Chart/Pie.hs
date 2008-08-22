module Graphics.Rendering.Chart.Pie where
-- original code thanks to Neal Alexander

import qualified Graphics.Rendering.Cairo as C

import Data.List
import Data.Bits
import Data.Accessor
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

-- | Accessor for field pie_title_
pie_title = accessor (\v->pie_title_ v) (\a v -> v{pie_title_=a})

-- | Accessor for field pie_title_style_
pie_title_style = accessor (\v->pie_title_style_ v) (\a v -> v{pie_title_style_=a})

-- | Accessor for field pie_plot_
pie_plot = accessor (\v->pie_plot_ v) (\a v -> v{pie_plot_=a})

-- | Accessor for field pie_background_
pie_background = accessor (\v->pie_background_ v) (\a v -> v{pie_background_=a})

-- | Accessor for field pie_margin_
pie_margin = accessor (\v->pie_margin_ v) (\a v -> v{pie_margin_=a})


data PieChart = PieChart {
   pie_data_  :: [PieItem],
   pie_colors_ :: [Color],
   pie_label_style_ :: CairoFontStyle,
   pie_label_line_style_ :: CairoLineStyle, 
   pie_start_angle_ :: Double

}

-- | Accessor for field pie_data_
pie_data = accessor (\v->pie_data_ v) (\a v -> v{pie_data_=a})

-- | Accessor for field pie_colors_
pie_colors = accessor (\v->pie_colors_ v) (\a v -> v{pie_colors_=a})

-- | Accessor for field pie_label_style_
pie_label_style = accessor (\v->pie_label_style_ v) (\a v -> v{pie_label_style_=a})

-- | Accessor for field pie_label_line_style_
pie_label_line_style = accessor (\v->pie_label_line_style_ v) (\a v -> v{pie_label_line_style_=a})

-- | Accessor for field pie_start_angle_
pie_start_angle = accessor (\v->pie_start_angle_ v) (\a v -> v{pie_start_angle_=a})

data PieItem = PieItem {
   pitem_label_ :: String,
   pitem_offset_ :: Double,
   pitem_value_ :: Double
}

-- | Accessor for field pitem_label_
pitem_label = accessor (\v->pitem_label_ v) (\a v -> v{pitem_label_=a})

-- | Accessor for field pitem_offset_
pitem_offset = accessor (\v->pitem_offset_ v) (\a v -> v{pitem_offset_=a})

-- | Accessor for field pitem_value_
pitem_value = accessor (\v->pitem_value_ v) (\a v -> v{pitem_value_=a})

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
       tval $ addMargins (lm/2,0,0,0) () title,
       weights (1,1) $ tval $ addMargins (lm,lm,lm,lm) () (toRenderable $ pie_plot_ p)
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
    return (const ())
 
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
