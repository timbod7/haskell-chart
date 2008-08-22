-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Legend
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Legend where

import qualified Graphics.Rendering.Cairo as C
import Control.Monad
import Data.List (nub, partition)
import Data.Accessor

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Plot
import Graphics.Rendering.Chart.Renderable

----------------------------------------------------------------------
-- Legend

data LegendStyle = LegendStyle {
   legend_label_style_ :: CairoFontStyle,
   legend_margin_ :: Double,
   legend_plot_size_ :: Double
}

-- | Accessor for field legend_label_style_
legend_label_style = accessor (\v->legend_label_style_ v) (\a v -> v{legend_label_style_=a})

-- | Accessor for field legend_margin_
legend_margin = accessor (\v->legend_margin_ v) (\a v -> v{legend_margin_=a})

-- | Accessor for field legend_plot_size_
legend_plot_size = accessor (\v->legend_plot_size_ v) (\a v -> v{legend_plot_size_=a})


data Legend x y = Legend Bool LegendStyle [(String,Plot x y)]

instance ToRenderable (Legend x y) where
  toRenderable l = Renderable {
    minsize=minsizeLegend l,
    render=renderLegend l
  }

minsizeLegend :: Legend x y -> CRender RectSize
minsizeLegend (Legend _ ls plots) = do
    let labels = nub $ map fst plots
    setFontStyle $ legend_label_style_ ls
    lsizes <- mapM textSize labels
    lgap <- legendSpacer
    let lm = legend_margin_ ls
    let pw = legend_plot_size_ ls
    let h = maximum  [h | (w,h) <- lsizes]
    let n = fromIntegral (length lsizes)
    let w = sum [w + lgap | (w,h) <- lsizes] + pw * (n+1) + lm * (n-1)
    return (w,h)

renderLegend :: Legend x y -> RectSize -> CRender (PickFn ())
renderLegend (Legend _ ls plots) (w,h) = do
    foldM_ rf rp1 $ join_nub plots
    return (const ())
  where
    rp1 = (Point 0 0)
    rp2 = (Point w h)
    lm = legend_margin_ ls
    lps = legend_plot_size_ ls

    rf p1 (label,theseplots) = do
        setFontStyle $ legend_label_style_ ls
        (w,h) <- textSize label
	lgap <- legendSpacer
	let p2 = (p1 `pvadd` Vector lps 0)
        mapM_ (\p -> plot_render_legend_ p (mkrect p1 rp1 p2 rp2)) theseplots
	let p3 = Point (p_x p2 + lgap) (p_y rp1)
	drawText HTA_Left VTA_Top p3 label
        return (p3 `pvadd` Vector (w+lm) 0)
    join_nub :: [(String, a)] -> [(String, [a])]
    join_nub ((x,a1):ys) = case partition ((==x) . fst) ys of
                           (xs, rest) -> (x, a1:map snd xs) : join_nub rest
    join_nub [] = []

legendSpacer = do
    (lgap,_) <- textSize "X"
    return lgap

defaultLegendStyle = LegendStyle {
    legend_label_style_=defaultFontStyle,
    legend_margin_=20,
    legend_plot_size_=20
}
