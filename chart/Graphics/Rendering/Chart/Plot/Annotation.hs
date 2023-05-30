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
    plot_annotation_background,
    plot_annotation_offset,
    plot_annotation_values
) where

import Control.Lens
import Data.Default.Class
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Plot.Types
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
      _plot_annotation_background :: Rectangle,
      -- ^ Rectangle which style determines the background of the annotation
      -- text and which '_rect_minsize' determines the additional width and
      -- height of the background area
      _plot_annotation_offset  :: Vector,
      _plot_annotation_values  :: [(x,y,String)]
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
renderAnnotation p pMap = withFontStyle style $ do
                            mapM_ drawRect values
                            mapM_ drawOne values
    where hta = _plot_annotation_hanchor p
          vta = _plot_annotation_vanchor p
          values = _plot_annotation_values p
          angle =  _plot_annotation_angle p
          style =  _plot_annotation_style p
          offset = _plot_annotation_offset p
          rectangle = _plot_annotation_background p
          (x1,y1) = _rect_minsize rectangle
          drawRect (x,y,s) = do
              ts <- textSize s
              let (x2,y2) = (textSizeWidth ts, textSizeHeight ts)
                  Point x3 y3 = point x y
                  -- position of top-left vertex of the rectangle
                  xvp HTA_Left = x3 - x1 / 2
                  xvp HTA_Centre = x3 - (x1 + x2) / 2
                  xvp HTA_Right = x3 - x2 - x1 / 2
                  yvp VTA_Top = y3 - y1 / 2
                  yvp VTA_Centre = y3 - (y1 + y2) / 2
                  yvp VTA_Bottom = y3 - y2 - y1 / 2
                  yvp VTA_BaseLine = y3 - y1 / 2 - textSizeAscent ts
              drawRectangle (Point (xvp hta) (yvp vta) `pvadd` offset) rectangle{ _rect_minsize = (x1+x2,y1+y2) }
          drawOne (x,y,s) = drawTextsR hta vta angle (point x y) s
          point x y = pMap (LValue x, LValue y) `pvadd` offset

instance Default (PlotAnnotation x y) where
  def = PlotAnnotation
    { _plot_annotation_hanchor = HTA_Centre
    , _plot_annotation_vanchor = VTA_Centre
    , _plot_annotation_angle   = 0
    , _plot_annotation_style   = def
    , _plot_annotation_background = def
    , _plot_annotation_values  = []
    , _plot_annotation_offset  = Vector 0 0
    }

$( makeLenses ''PlotAnnotation )
