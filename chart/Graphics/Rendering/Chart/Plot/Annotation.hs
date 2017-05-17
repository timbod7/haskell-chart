-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Annotation
-- Copyright   :  (c) Tim Docker 2006, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Show textual annotations on a chart.

{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Annotation(
    PlotAnnotation(..),

    plot_annotation_hanchor,
    plot_annotation_vanchor,
    plot_annotation_angle,
    plot_annotation_style,
    plot_annotation_values,
    plot_annotation_offset
) where

import Control.Lens
import Data.Default.Class
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Plot.Types

-- | Value for describing a series of text annotations
--   to be placed at arbitrary points on the graph. Annotations
--   can be rotated and styled.

data PlotAnnotation  x y = PlotAnnotation {
      _plot_annotation_hanchor :: HTextAnchor,
      _plot_annotation_vanchor :: VTextAnchor,
      _plot_annotation_angle   :: Double,
      -- ^ Angle, in degrees, to rotate the annotation about the anchor point.
      _plot_annotation_style   :: FontStyle,
      _plot_annotation_values  :: [(x,y,String)],
      _plot_annotation_offset  :: Vector
}


instance ToPlot PlotAnnotation where
    toPlot p = Plot {
      _plot_render = renderAnnotation p,
      _plot_legend = [],
      _plot_all_points = (map (^._1) vs , map (^._2) vs)
    }
      where
        vs = _plot_annotation_values p


renderAnnotation :: PlotAnnotation x y -> PointMapFn x y -> BackendProgram ()
renderAnnotation p pMap = withFontStyle style $
                            mapM_ drawOne values
    where hta = _plot_annotation_hanchor p
          vta = _plot_annotation_vanchor p
          values = _plot_annotation_values p
          angle =  _plot_annotation_angle p
          style =  _plot_annotation_style p
          offset = _plot_annotation_offset p
          drawOne (x,y,s) = drawTextsR hta vta angle point s
              where point = pMap (LValue x, LValue y) `pvadd` offset

instance Default (PlotAnnotation x y) where
  def = PlotAnnotation 
    { _plot_annotation_hanchor = HTA_Centre
    , _plot_annotation_vanchor = VTA_Centre
    , _plot_annotation_angle   = 0
    , _plot_annotation_style   = def
    , _plot_annotation_values  = []
    , _plot_annotation_offset  = Vector 0 0
    }

$( makeLenses ''PlotAnnotation )
