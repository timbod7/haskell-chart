module Graphics.Rendering.Chart.Pie where
-- original code thanks to Neal Alexander

import qualified Graphics.Rendering.Cairo as C

import Data.List
import Data.Bits
import Data.Foldable 
import Control.Monad 
import Debug.Trace

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
    pie_start_angle = 0
}

defaultPieItem = PieItem "" 0 0

defaultPieLayout = PieLayout {
    pie_background = solidFillStyle white,
    pie_title = "",
    pie_title_style = fontStyle "sans" 15 C.FontSlantNormal C.FontWeightBold,
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

minsizePie p = return (100,100)
renderPie p rect = do
    setFontStyle (pie_label_style p)                 

    c $ aspect size
    c $ C.moveTo `uncurry` center  
    foldM_ paint (pie_start_angle p) (zip (pie_colors p) content)
 
    where
        color = 2 :: Int
        content = let total = Data.List.sum (map pitem_value (pie_data p))
                  in [ (pitem_value pi/total,pitem_label pi)
                       | pi <- (pie_data p) ]
        size = let (Rect p1 p2) = rect in (p_x p2 - p_x p1, p_y p2 - p_y p1)
        center = (0.5, 0.55)
        radius = 0.40
        radian = (*(pi / 180.0))

        ray angle n = (x', y')
            where
                x' = x + (cos' * x'')
                y' = y + (sin' * x'')

                cos' = (cos . radian) angle
                sin' = (sin . radian) angle

                x''  = ((x + radius + n) - x)
                x    = fst center
                y    = snd center

        aspect (w,h) | w == h = uncurry C.scale (w,h)
                     | w >  h = do
                        C.translate ((w - h) / 2) 0
                        C.scale h h
                     | h >  w = do
                        C.translate 0 ((h - w) / 2)
                        C.scale w w

        paint a1 (color,i@(percent, name)) = do
            let ax = 360.0 * percent
            let a2 = a1 + ax

            pieSlice center a1 a2 color
            pieLabel i (a1 + (ax / 2)) 

            return a2

            where
                
                pieLabel (percent, name) angle = c $ do
                    C.setSourceRGBA 0.0 0.0 0.0 0.8
                    C.setLineWidth 0.002          
                    C.setLineJoin C.LineJoinBevel


                    label (ray angle 0.05) $ name

                    where

                        label (x,y) str = do

                            dot (ray angle 0.02) >> C.lineTo x y

                            scaleText >> align >> C.showText str
                            C.stroke

                            where
                                dot (x,y) = do
                                    C.moveTo x y
                                {-- C.arc x y 0.0025 0 (pi*2)
                                    C.closePath

                                    C.setSourceRGB 0.5 0.5 0.5
                                    C.fillPreserve
                                    C.setSourceRGB 0.5 0.5 0.5

                                    C.stroke
                                    C.moveTo x y
                                    C.setSourceRGB 0.5 0.5 0.5 --}

                                scaleText | percent < 3.0 = C.setFontSize 0.030
                                          | percent < 6.0 = C.setFontSize 0.035
                                          | otherwise     = C.setFontSize 0.042

                                align = 
                                    (underline . pad . C.textExtentsXadvance) =<< C.textExtents str

                                    where
                                        pad = (+0.005)
                                        underline n | is'left   = C.relLineTo (negate n) 0 >> C.relMoveTo 0 bump 
                                                    | otherwise = C.relLineTo n 0          >> C.moveTo (pad x) (bump + y)
                                            where
                                                is'left = angle > 90 && angle < 270

                                                bump | angle < 180 = -0.0030 --(C.textExtentsHeight ext)
                                                     | otherwise   = -0.0030 -- (-0.001)


                pieSlice (x,y) a1 a2 color = c $ do 

                    C.arc x y radius (radian a1) (radian a2)
                    C.lineTo x y
                    C.lineTo x y
                    C.closePath

                    setSourceColor color
                    C.fillPreserve
                    C.setSourceRGBA 1 1 1 0.1

                    C.stroke
