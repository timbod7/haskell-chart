-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Candle
-- Copyright   :  (c) Tim Docker 2006, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Candlestick charts for financial plotting
--
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Candle(
    PlotCandle(..),
    Candle(..),

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

import Control.Lens hiding (op)

import Graphics.Rendering.Chart.Geometry hiding (close)
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Plot.Types
import Control.Monad
import Data.Colour (opaque)
import Data.Colour.Names (white, blue)
import Data.Default.Class

-- | Value defining a financial interval: opening and closing prices, with
--   maxima and minima; and a style in which to render them.
--   By convention, there are different fill styles depending on whether
--   the price rises (open < close) or falls (close < open).
--   (This plot type can also be re-purposed for statistical intervals, e.g.
--    minimum, first quartile, median, third quartile, maximum.)
data PlotCandle x y = PlotCandle {
    _plot_candle_title           :: String,
    _plot_candle_line_style      :: LineStyle,
    _plot_candle_fill            :: Bool,
    _plot_candle_rise_fill_style :: FillStyle,
    _plot_candle_fall_fill_style :: FillStyle,
    _plot_candle_tick_length     :: Double,
    _plot_candle_width           :: Double,
    _plot_candle_centre          :: Double,
    _plot_candle_values          :: [Candle x y]
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
        _plot_render     = renderPlotCandle p,
        _plot_legend     = [(_plot_candle_title p, renderPlotLegendCandle p)],
        _plot_all_points = ( map candle_x pts
                           , concat [ [candle_low c, candle_high c]
                                    | c <- pts ] )
    }
      where
        pts = _plot_candle_values p

renderPlotCandle :: PlotCandle x y -> PointMapFn x y -> BackendProgram ()
renderPlotCandle p pmap = 
    mapM_ (drawCandle p . candlemap) (_plot_candle_values p)
  where
    candlemap (Candle x lo op mid cl hi) =
        Candle x' lo' op' mid' cl' hi'
        where (Point x' mid')  = pmap' (x,mid)
              (Point _  lo')   = pmap' (x,lo)
              (Point _  op')   = pmap' (x,op)
              (Point _  cl')   = pmap' (x,cl)
              (Point _  hi')   = pmap' (x,hi)
    pmap' = mapXY pmap

drawCandle :: PlotCandle x y -> Candle Double Double -> BackendProgram ()
drawCandle ps (Candle x lo open mid close hi) = do
        let tl = _plot_candle_tick_length ps
        let wd = _plot_candle_width ps
        let ct = _plot_candle_centre ps
        let f  = _plot_candle_fill ps
        -- the pixel coordinate system is inverted wrt the value coords.
        when f $ withFillStyle (if open >= close
                                   then _plot_candle_rise_fill_style ps
                                   else _plot_candle_fall_fill_style ps) $ 
                    fillPath $ moveTo' (x-wd) open
                            <> lineTo' (x-wd) close
                            <> lineTo' (x+wd) close
                            <> lineTo' (x+wd) open
                            <> lineTo' (x-wd) open

        withLineStyle (_plot_candle_line_style ps) $ do
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
          
          when (ct > 0) $ strokePath $ moveTo' (x-ct) mid
                                    <> lineTo' (x+ct) mid

renderPlotLegendCandle :: PlotCandle x y -> Rect -> BackendProgram ()
renderPlotLegendCandle pc (Rect p1 p2) = do
    drawCandle pc2 (Candle (xwid*1/4) lo open mid close hi)
    drawCandle pc2 (Candle (xwid*2/3) lo close mid open hi)
  where
    pc2   = pc { _plot_candle_width = 2 }
    xwid  = p_x p1 + p_x p2
    lo    = max (p_y p1) (p_y p2)
    mid   = (p_y p1 + p_y p2)/2
    hi    = min (p_y p1) (p_y p2)
    open  = (lo + mid) / 2
    close = (mid + hi) / 2

instance Default (PlotCandle x y) where
  def = PlotCandle 
    { _plot_candle_title       = ""
    , _plot_candle_line_style  = solidLine 1 $ opaque blue
    , _plot_candle_fill        = False
    , _plot_candle_rise_fill_style  = solidFillStyle $ opaque white
    , _plot_candle_fall_fill_style  = solidFillStyle $ opaque blue
    , _plot_candle_tick_length = 2
    , _plot_candle_width       = 5
    , _plot_candle_centre      = 0
    , _plot_candle_values      = []
    }

$( makeLenses ''PlotCandle )
