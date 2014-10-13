-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.Unit
-- Copyright   :  (c) Tim Docker 2010, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Calculate and render indexed axes

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Rendering.Chart.Axis.Indexed(
    PlotIndex(..),
    indexAxis,
    autoIndexAxis,
    addIndexes,
) where

import Control.Arrow (first)
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
addIndexes as = map (first PlotIndex) (zip [0..] as)

-- | Create an axis for values indexed by position. The
--   list of strings are the labels to be used.
autoIndexAxis :: Integral i => [String] -> [i] -> AxisData i
autoIndexAxis labels vs = indexAxis labels r
  where
    r = (minimum vs,maximum vs)

indexAxis :: Integral i => [String] -> (i,i) -> AxisData i
indexAxis labels (imin,imax) = AxisData {
    _axis_visibility = def { _axis_show_ticks = False },
    _axis_viewport = vport,
    _axis_tropweiv = invport,
    _axis_ticks    = [],
    _axis_labels   = [filter (\(i,_) -> i >= imin && i <= imax)
                            (zip [0..] labels)],
    _axis_grid     = [],
    _axis_ranged   = indexAxis labels
    }
  where
    vport r i = linMap id ( fromIntegral imin - 0.5
                          , fromIntegral imax + 0.5) r (fromIntegral i)
    invport = invLinMap round fromIntegral (imin, imax)

