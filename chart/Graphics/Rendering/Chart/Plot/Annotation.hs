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

import Data.Accessor.Template
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
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
      plot_annotation_hanchor_ :: HTextAnchor,
      plot_annotation_vanchor_ :: VTextAnchor,
      plot_annotation_angle_   :: Double,
      plot_annotation_style_   :: FontStyle,
      plot_annotation_values_  :: [(x,y,String)]
}


instance ToPlot PlotAnnotation where
    toPlot p = Plot {
        plot_render_ = renderAnnotation p,
	plot_legend_ = [],
	plot_all_points_ = (map (\(x,_,_)->x)  vs , map (\(_,y,_)->y) vs)
    }
      where
        vs = plot_annotation_values_ p


renderAnnotation :: (ChartBackend m) => PlotAnnotation x y -> PointMapFn x y -> m ()
renderAnnotation p pMap = bLocal $ do
                            bSetFontStyle style                            
                            mapM_ drawOne values
    where hta = plot_annotation_hanchor_ p
          vta = plot_annotation_vanchor_ p
          values = plot_annotation_values_ p
          angle =  plot_annotation_angle_ p
          style =  plot_annotation_style_ p
          drawOne (x,y,s) = bDrawTextsR hta vta angle point s
              where point = pMap (LValue x, LValue y)

defaultPlotAnnotation = PlotAnnotation {
                          plot_annotation_hanchor_ = HTA_Centre,
                          plot_annotation_vanchor_ = VTA_Centre,
                          plot_annotation_angle_   = 0,
                          plot_annotation_style_   = defaultFontStyle,
                          plot_annotation_values_  = []
}

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.

$( deriveAccessors ''PlotAnnotation )

