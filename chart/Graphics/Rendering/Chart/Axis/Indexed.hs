-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.Unit
-- Copyright   :  (c) Tim Docker 2010, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Calculate and render indexed axes

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Graphics.Rendering.Chart.Axis.Indexed(
    PlotIndex(..),
    autoIndexAxis',
    autoIndexAxis,
    addIndexes,
) where

import Data.Default.Class

import Graphics.Rendering.Chart.Axis.Types

-- | Type for capturing values plotted by index number
--   (ie position in a list) rather than a numerical value.
newtype PlotIndex = PlotIndex { plotindex_i :: Int }
  deriving (Eq,Ord,Enum,Num,Real,Integral,Show)

instance PlotValue PlotIndex where
    toValue (PlotIndex i) = fromIntegral i
    fromValue             = PlotIndex . round
    autoAxis              = autoIndexAxis []

-- | Augment a list of values with index numbers for plotting.
addIndexes :: [a] -> [(PlotIndex,a)]
addIndexes = zipWith (\n x -> (PlotIndex n, x)) [0..]

-- | Create an axis for values indexed by position. The
--   list of strings are the labels to be used.
autoIndexAxis' :: Integral i => Bool -> [String] -> AxisFn i
autoIndexAxis' tks labels vs = AxisData {
    _axis_visibility = def { _axis_show_ticks = False },
    _axis_viewport = vport,
    _axis_tropweiv = invport,
    _axis_ticks    = if tks then map (, 5) $ take (length labels) [0..] else [],
    _axis_labels   = [filter (\(i,_) -> i >= imin && i <= imax)
                             (zip [0..] labels)],
    _axis_grid     = []
    }
  where
    vport r i = linMap id ( fromIntegral imin - 0.5
                          , fromIntegral imax + 0.5) r (fromIntegral i)
    invport = invLinMap round fromIntegral (imin, imax)
    imin = minimum vs
    imax = maximum vs

autoIndexAxis :: Integral i => [String] -> AxisFn i
autoIndexAxis = autoIndexAxis' False