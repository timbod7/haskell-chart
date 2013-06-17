-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Candle
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Candlestick charts for financial plotting
--
{-# OPTIONS_GHC -XTemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Candle(
    PlotCandle(..),
    Candle(..),
    defaultPlotCandle,

    plot_candle_title,
    plot_candle_line_style,
    plot_candle_tick_length,
    plot_candle_width,
    plot_candle_centre,
    plot_candle_fill,
    plot_candle_rise_fill_style,
    plot_candle_fall_fill_style,
    plot_candle_values,
) where

import Data.Accessor.Template
import Data.Monoid

import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Plot.Types
import Control.Monad
import Data.Colour (opaque)
import Data.Colour.Names (black, white, blue)
import Data.Colour.SRGB (sRGB)

-- | Value defining a financial interval: opening and closing prices, with
--   maxima and minima; and a style in which to render them.
--   By convention, there are different fill styles depending on whether
--   the price rises (open < close) or falls (close < open).
--   (This plot type can also be re-purposed for statistical intervals, e.g.
--    minimum, first quartile, median, third quartile, maximum.)
data PlotCandle x y = PlotCandle {
    plot_candle_title_           :: String,
    plot_candle_line_style_      :: LineStyle,
    plot_candle_fill_            :: Bool,
    plot_candle_rise_fill_style_ :: FillStyle,
    plot_candle_fall_fill_style_ :: FillStyle,
    plot_candle_tick_length_     :: Double,
    plot_candle_width_           :: Double,
    plot_candle_centre_          :: Double,
    plot_candle_values_          :: [Candle x y]
}

-- | A Value holding price intervals for a given x-coord.
--   An alternative view is that these are statistical intervals: the
--   0th, 25th, 50th, 75th, and 100th percentiles.
data Candle x y = Candle { candle_x     :: x
                         , candle_low   :: y
                         , candle_open  :: y
                         , candle_mid   :: y
                         , candle_close :: y
                         , candle_high  :: y
                         } deriving (Show)

instance ToPlot PlotCandle where
    toPlot p = Plot {
        plot_render_     = renderPlotCandle p,
        plot_legend_     = [(plot_candle_title_ p, renderPlotLegendCandle p)],
        plot_all_points_ = ( map candle_x pts
                           , concat [ [candle_low c, candle_high c]
                                    | c <- pts ] )
    }
      where
        pts = plot_candle_values_ p

renderPlotCandle :: (ChartBackend m) => PlotCandle x y -> PointMapFn x y -> m ()
renderPlotCandle p pmap = do
    mapM_ (drawCandle p . candlemap) (plot_candle_values_ p)
  where
    candlemap (Candle x lo op mid cl hi) =
        Candle x' lo' op' mid' cl' hi'
        where (Point x' mid')  = pmap' (x,mid)
              (Point _  lo')   = pmap' (x,lo)
              (Point _  op')   = pmap' (x,op)
              (Point _  cl')   = pmap' (x,cl)
              (Point _  hi')   = pmap' (x,hi)
    pmap' = mapXY pmap

drawCandle ps (Candle x lo open mid close hi) = do
        let tl = plot_candle_tick_length_ ps
        let wd = plot_candle_width_ ps
        let ct = plot_candle_centre_ ps
        let f  = plot_candle_fill_ ps
        -- the pixel coordinate system is inverted wrt the value coords.
        when f $ withFillStyle (if open >= close
                                   then plot_candle_rise_fill_style_ ps
                                   else plot_candle_fall_fill_style_ ps) $ do
                    fillPath $ moveTo' (x-wd) open
                            <> lineTo' (x-wd) close
                            <> lineTo' (x+wd) close
                            <> lineTo' (x+wd) open
                            <> lineTo' (x-wd) open

        withLineStyle (plot_candle_line_style_ ps) $ do
          strokePath $ moveTo' (x-wd) open
                    <> lineTo' (x-wd) close
                    <> lineTo' (x+wd) close
                    <> lineTo' (x+wd) open
                    <> lineTo' (x-wd) open

          strokePath $ moveTo' x (min lo hi)
                    <> lineTo' x (min open close)
                    <> moveTo' x (max open close)
                    <> lineTo' x (max hi lo)

          when (tl > 0) $ strokePath $ moveTo' (x-tl) lo
                                    <> lineTo' (x+tl) lo
                                    <> moveTo' (x-tl) hi
                                    <> lineTo' (x+tl) hi
          
          when (ct > 0) $ do strokePath $ moveTo' (x-ct) mid
                                       <> lineTo' (x+ct) mid

renderPlotLegendCandle :: (ChartBackend m) => PlotCandle x y -> Rect -> m ()
renderPlotLegendCandle p r@(Rect p1 p2) = do
    drawCandle p{ plot_candle_width_ = 2}
                      (Candle ((p_x p1 + p_x p2)*1/4) lo open mid close hi)
    drawCandle p{ plot_candle_width_ = 2}
                      (Candle ((p_x p1 + p_x p2)*2/3) lo close mid open hi)
  where
    lo    = max (p_y p1) (p_y p2)
    mid   = (p_y p1 + p_y p2)/2
    hi    = min (p_y p1) (p_y p2)
    open  = (lo + mid) / 2
    close = (mid + hi) / 2

defaultPlotCandle :: PlotCandle x y
defaultPlotCandle = PlotCandle {
    plot_candle_title_       = "",
    plot_candle_line_style_  = solidLine 1 $ opaque blue,
    plot_candle_fill_        = False,
    plot_candle_rise_fill_style_  = solidFillStyle $ opaque white,
    plot_candle_fall_fill_style_  = solidFillStyle $ opaque blue,
    plot_candle_tick_length_ = 2,
    plot_candle_width_       = 5,
    plot_candle_centre_      = 0,
    plot_candle_values_      = []
}

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.

$( deriveAccessors ''PlotCandle )
