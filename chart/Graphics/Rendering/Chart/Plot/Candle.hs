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

import Control.Lens
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart.Types
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
    _plot_candle_title           :: String,
    _plot_candle_line_style      :: CairoLineStyle,
    _plot_candle_fill            :: Bool,
    _plot_candle_rise_fill_style :: CairoFillStyle,
    _plot_candle_fall_fill_style :: CairoFillStyle,
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

renderPlotCandle :: PlotCandle x y -> PointMapFn x y -> CRender ()
renderPlotCandle p pmap = preserveCState $ do
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

drawCandle ps (Candle x lo open mid close hi) = do
        let tl = _plot_candle_tick_length ps
        let wd = _plot_candle_width ps
        let ct = _plot_candle_centre ps
        let f  = _plot_candle_fill ps
        -- the pixel coordinate system is inverted wrt the value coords.
        when f $ do setFillStyle (if open >= close
                                  then _plot_candle_rise_fill_style ps
                                  else _plot_candle_fall_fill_style ps)

                    c $ C.newPath
                    c $ C.moveTo (x-wd) open
                    c $ C.lineTo (x-wd) close
                    c $ C.lineTo (x+wd) close
                    c $ C.lineTo (x+wd) open
                    c $ C.lineTo (x-wd) open
                    c $ C.fill

        setLineStyle (_plot_candle_line_style ps)
        c $ C.newPath
        c $ C.moveTo (x-wd) open
        c $ C.lineTo (x-wd) close
        c $ C.lineTo (x+wd) close
        c $ C.lineTo (x+wd) open
        c $ C.lineTo (x-wd) open
        c $ C.stroke

        c $ C.newPath
        c $ C.moveTo x (min lo hi)
        c $ C.lineTo x (min open close)
        c $ C.moveTo x (max open close)
        c $ C.lineTo x (max hi lo)
        c $ C.stroke

        when (tl > 0) $ do c $ C.newPath
                           c $ C.moveTo (x-tl) lo
                           c $ C.lineTo (x+tl) lo
                           c $ C.moveTo (x-tl) hi
                           c $ C.lineTo (x+tl) hi
                           c $ C.stroke

        when (ct > 0) $ do c $ C.moveTo (x-ct) mid
                           c $ C.lineTo (x+ct) mid
                           c $ C.stroke

renderPlotLegendCandle :: PlotCandle x y -> Rect -> CRender ()
renderPlotLegendCandle p r@(Rect p1 p2) = preserveCState $ do
    drawCandle p{ _plot_candle_width = 2}
                      (Candle ((p_x p1 + p_x p2)*1/4) lo open mid close hi)
    drawCandle p{ _plot_candle_width = 2}
                      (Candle ((p_x p1 + p_x p2)*2/3) lo close mid open hi)
  where
    lo    = max (p_y p1) (p_y p2)
    mid   = (p_y p1 + p_y p2)/2
    hi    = min (p_y p1) (p_y p2)
    open  = (lo + mid) / 2
    close = (mid + hi) / 2

defaultPlotCandle :: PlotCandle x y
defaultPlotCandle = PlotCandle {
    _plot_candle_title       = "",
    _plot_candle_line_style  = solidLine 1 $ opaque blue,
    _plot_candle_fill        = False,
    _plot_candle_rise_fill_style  = solidFillStyle $ opaque white,
    _plot_candle_fall_fill_style  = solidFillStyle $ opaque blue,
    _plot_candle_tick_length = 2,
    _plot_candle_width       = 5,
    _plot_candle_centre      = 0,
    _plot_candle_values      = []
}

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.

$( makeLenses ''PlotCandle )
