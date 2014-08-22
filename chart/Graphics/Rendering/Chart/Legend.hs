-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Legend
-- Copyright   :  (c) Tim Docker 2006, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
-- 
-- Types and functions for handling the legend(s) on a chart. A legend
-- is an area on the chart used to label the plotted values.
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Legend(
    Legend(..),
    LegendStyle(..),
    LegendOrientation(..),
    legendToRenderable,
    legend_label_style,
    legend_margin,
    legend_plot_size,
    legend_orientation
) where

import Data.List (partition,intersperse)
import Control.Lens
import Data.Default.Class

import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Grid

----------------------------------------------------------------------
-- Legend

data LegendStyle = LegendStyle {
   _legend_label_style :: FontStyle,
   _legend_margin      :: Double,
   _legend_plot_size   :: Double,
   _legend_orientation :: LegendOrientation
}

-- | Legends can be constructed in two orientations: in rows
-- (where we specify the maximum number of columns), and in
-- columns (where we specify the maximum number of rows)
data LegendOrientation = LORows Int
                       | LOCols Int
                       

data Legend x y = Legend LegendStyle [(String, Rect -> ChartBackend ())]

instance ToRenderable (Legend x y) where
  toRenderable = setPickFn nullPickFn . legendToRenderable

legendToRenderable :: Legend x y -> Renderable String
legendToRenderable (Legend ls lvs) = gridToRenderable grid
  where
    grid = case _legend_orientation ls of
        LORows n -> mkGrid n aboveG besideG
        LOCols n -> mkGrid n besideG aboveG 

    aboveG = aboveN.intersperse ggap1
    besideG = besideN.intersperse ggap1

    mkGrid n join1 join2 = join1 [ join2 (map rf ps1) | ps1 <- groups n ps ]

    ps  :: [(String, [Rect -> ChartBackend ()])]
    ps   = join_nub lvs

    rf (title,rfs) = besideN [gpic,ggap2,gtitle]
      where
        gpic = besideN $ intersperse ggap2 (map rp rfs)
        gtitle = tval $ lbl title
        rp rfn = tval Renderable {
                     minsize = return (_legend_plot_size ls, 0),
                     render  = \(w,h) -> do 
                         _ <- rfn (Rect (Point 0 0) (Point w h))
                         return (\_-> Just title)
                 }

    ggap1 = tval $ spacer (_legend_margin ls,_legend_margin ls / 2)
    ggap2 = tval $ spacer1 (lbl "X")
    lbl = label (_legend_label_style ls) HTA_Left VTA_Centre 

groups :: Int -> [a] -> [[a]]
groups _ [] = []
groups n vs = let (vs1,vs2) = splitAt n vs in vs1:groups n vs2

join_nub :: [(String, a)] -> [(String, [a])]
join_nub ((x,a1):ys) = case partition ((==x) . fst) ys of
                         (xs, rest) -> (x, a1:map snd xs) : join_nub rest
join_nub []          = []

instance Default LegendStyle where
  def = LegendStyle 
    { _legend_label_style = def
    , _legend_margin      = 20
    , _legend_plot_size   = 20
    , _legend_orientation = LORows 4
    }

$( makeLenses ''LegendStyle )

