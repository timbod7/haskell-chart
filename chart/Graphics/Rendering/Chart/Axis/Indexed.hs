-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.Unit
-- Copyright   :  (c) Tim Docker 2010
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Calculate and render indexed axes

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Rendering.Chart.Axis.Indexed(
    PlotIndex(..),
    autoIndexAxis,
    addIndexes,
) where

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
addIndexes as = map (\(i,a) -> (PlotIndex i,a))  (zip [0..] as)

-- | Create an axis for values indexed by position. The
--   list of strings are the labels to be used.
autoIndexAxis :: Integral i => [String] -> [i] -> AxisData i
autoIndexAxis labels vs = AxisData {
    axis_viewport_ = vport,
    axis_tropweiv_ = invport,
    axis_ticks_    = [],
    axis_labels_   = [filter (\(i,l) -> i >= imin && i <= imax)
                            (zip [0..] labels)],
    axis_grid_     = []
    }
  where
    vport r i = linMap id ( fromIntegral imin - 0.5
                          , fromIntegral imax + 0.5) r (fromIntegral i)
    invport r z = invLinMap round fromIntegral (imin, imax) r z
    imin = minimum vs
    imax = maximum vs
