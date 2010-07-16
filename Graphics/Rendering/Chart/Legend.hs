-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Legend
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

{-# OPTIONS_GHC -XTemplateHaskell #-}

module Graphics.Rendering.Chart.Legend(
    Legend(..),
    LegendStyle(..),
    LegendOrientation(..),
    defaultLegendStyle,
    legendToRenderable,
    legend_label_style,
    legend_margin,
    legend_plot_size,
    legend_orientation
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
   legend_plot_size_   :: Double,
   legend_orientation_ :: LegendOrientation
}

-- | Legends can be constructed in two orientations: in rows
-- (where we specify the maximum number of columns), and in
-- columns (where we specify the maximum number of rows)
data LegendOrientation = LORows Int
                       | LOCols Int
                       

data Legend x y = Legend LegendStyle [(String, Rect -> CRender ())]

instance ToRenderable (Legend x y) where
  toRenderable = setPickFn nullPickFn.legendToRenderable

legendToRenderable :: Legend x y -> Renderable String
legendToRenderable (Legend ls lvs) = gridToRenderable grid
  where
    grid = case legend_orientation_ ls of
        LORows n -> mkGrid n aboveG besideG
        LOCols n -> mkGrid n besideG aboveG 

    aboveG = aboveN.(intersperse ggap1)
    besideG = besideN.(intersperse ggap1)

    mkGrid n join1 join2 = join1 [ join2 (map rf ps1) | ps1 <- groups n ps ]

    ps  :: [(String, [Rect -> CRender ()])]
    ps   = join_nub lvs

    rf (title,rfs) = besideN [gpic,ggap2,gtitle]
      where
        gpic = besideN $ intersperse ggap2 (map rp rfs)
        gtitle = tval $ lbl title
        rp rfn = tval $ Renderable {
                     minsize = return (legend_plot_size_ ls, 0),
                     render  = \(w,h) -> do 
                         rfn (Rect (Point 0 0) (Point w h))
                         return (\_-> Just title)
                 }

    ggap1 = tval $ spacer (legend_margin_ ls,legend_margin_ ls / 2)
    ggap2 = tval $ spacer1 (lbl "X")
    lbl s = label (legend_label_style_ ls) HTA_Left VTA_Centre s

groups :: Int -> [a] -> [[a]]
groups  n [] = []
groups  n vs = let (vs1,vs2) = splitAt n vs in vs1:groups n vs2

join_nub :: [(String, a)] -> [(String, [a])]
join_nub ((x,a1):ys) = case partition ((==x) . fst) ys of
                         (xs, rest) -> (x, a1:map snd xs) : join_nub rest
join_nub []          = []

defaultLegendStyle :: LegendStyle
defaultLegendStyle = LegendStyle {
    legend_label_style_ = defaultFontStyle,
    legend_margin_      = 20,
    legend_plot_size_   = 20,
    legend_orientation_ = LORows 4
}

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.
$( deriveAccessors ''LegendStyle )

