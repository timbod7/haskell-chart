-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Legend
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

{-# OPTIONS_GHC -XTemplateHaskell #-}

module Graphics.Rendering.Chart.Legend(
    Legend(..),
    LegendStyle(..),
    defaultLegendStyle,
    legendToRenderable,
    legend_label_style,
    legend_margin,
    legend_plot_size,
) where

import qualified Graphics.Rendering.Cairo as C
import Control.Monad
import Data.List (nub, partition,intersperse)
import Data.Accessor.Template

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Plot
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Grid

----------------------------------------------------------------------
-- Legend

data LegendStyle = LegendStyle {
   legend_label_style_ :: CairoFontStyle,
   legend_margin_      :: Double,
   legend_plot_size_   :: Double
}

data Legend x y = Legend Bool LegendStyle [(String, Rect -> CRender ())]

instance ToRenderable (Legend x y) where
  toRenderable = setPickFn nullPickFn.legendToRenderable

legendToRenderable :: Legend x y -> Renderable String
legendToRenderable (Legend _ ls lvs) = gridToRenderable grid
  where
    grid = besideN $ intersperse ggap1 (map (tval.rf) ps)

    ps  :: [(String, [Rect -> CRender ()])]
    ps   = join_nub lvs

    rf (title,rfs) = gridToRenderable grid1
      where
        grid1  = besideN $ intersperse ggap2 (map rp rfs) ++ [ggap2,gtitle]
        gtitle = tval $ lbl title
        rp rfn = tval $ Renderable {
                     minsize = return (legend_plot_size_ ls, 0),
                     render  = \(w,h) -> do 
                         rfn (Rect (Point 0 0) (Point w h))
                         return nullPickFn
                 }

    ggap1 = tval $ spacer (legend_margin_ ls,0)
    ggap2 = tval $ spacer1 (lbl "X")
    lbl s = label (legend_label_style_ ls) HTA_Centre VTA_Centre s

join_nub :: [(String, a)] -> [(String, [a])]
join_nub ((x,a1):ys) = case partition ((==x) . fst) ys of
                         (xs, rest) -> (x, a1:map snd xs) : join_nub rest
join_nub []          = []

defaultLegendStyle = LegendStyle {
    legend_label_style_ = defaultFontStyle,
    legend_margin_      = 20,
    legend_plot_size_   = 20
}

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.
$( deriveAccessors ''LegendStyle )

