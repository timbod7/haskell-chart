-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Annotation
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Show textual annotations on a chart.

{-# OPTIONS_GHC -XTemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Annotation(
    PlotAnnotation(..),
    defaultPlotAnnotation,

    plot_annotation_hanchor,
    plot_annotation_vanchor,
    plot_annotation_angle,
    plot_annotation_style,
    plot_annotation_values
) where

import Control.Lens
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Plot.Types
import Data.Colour (opaque)
import Data.Colour.Names (black, blue)
import Data.Colour.SRGB (sRGB)
-- | Value for describing a series of text annotations
--   to be placed at arbitrary points on the graph. Annotations
--   can be rotated and styled. Rotation angle is given in degrees,
--   rotation is performend around the anchor point.

data PlotAnnotation  x y = PlotAnnotation {
      _plot_annotation_hanchor :: HTextAnchor,
      _plot_annotation_vanchor :: VTextAnchor,
      _plot_annotation_angle   :: Double,
      _plot_annotation_style   :: CairoFontStyle,
      _plot_annotation_values  :: [(x,y,String)]
}


instance ToPlot PlotAnnotation where
    toPlot p = Plot {
        _plot_render = renderAnnotation p,
        _plot_legend = [],
        _plot_all_points = (map (\(x,_,_)->x)  vs , map (\(_,y,_)->y) vs)
    }
      where
        vs = _plot_annotation_values p


renderAnnotation :: PlotAnnotation x y -> PointMapFn x y -> CRender ()

renderAnnotation p pMap = preserveCState $ do
                            setFontStyle style
                            mapM_ drawOne values
    where hta = _plot_annotation_hanchor p
          vta = _plot_annotation_vanchor p
          values = _plot_annotation_values p
          angle =  _plot_annotation_angle p
          style =  _plot_annotation_style p
          drawOne (x,y,s) = drawTextsR hta vta angle point s
              where point = pMap (LValue x, LValue y)

defaultPlotAnnotation = PlotAnnotation {
                          _plot_annotation_hanchor = HTA_Centre,
                          _plot_annotation_vanchor = VTA_Centre,
                          _plot_annotation_angle   = 0,
                          _plot_annotation_style   = defaultFontStyle,
                          _plot_annotation_values  = []
}

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.

$( makeLenses ''PlotAnnotation )
