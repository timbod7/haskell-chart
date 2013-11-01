module TestParametric where

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class

import Utils

chart :: Double -> Renderable (LayoutPick Double Double Double)
chart lwidth = layoutToRenderable layout
  where
    circle = [ (r a * sin (a*dr),r a * cos (a*dr)) | a <- [0,0.5..360::Double] ]
      where
        dr = 2 * pi / 360
        r a = 0.8 * cos (a * 20 * pi /360)

    circleP = plot_lines_values .~ [circle]
            $ plot_lines_style .~ solidLine lwidth (opaque blue) 
            $ def

    layout = layout_title .~ "Parametric Plot"
           $ layout_plots .~ [toPlot circleP]
           $ def

-- main = main' "test1" (chart 0.25)
