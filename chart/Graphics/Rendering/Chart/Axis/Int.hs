-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.Int
-- Copyright   :  (c) Tim Docker 2010, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Calculate and render integer indexed axes

module Graphics.Rendering.Chart.Axis.Int(
    defaultIntAxis,
    scaledIntAxis,
    autoScaledIntAxis
) where

import Data.List(genericLength)
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Axis.Types
import Graphics.Rendering.Chart.Axis.Floating

instance PlotValue Int where
    toValue    = fromIntegral
    fromValue  = round
    autoAxis   = autoScaledIntAxis defaultIntAxis

instance PlotValue Integer where
    toValue    = fromIntegral
    fromValue  = round
    autoAxis   = autoScaledIntAxis defaultIntAxis

defaultIntAxis :: (Show a) => LinearAxisParams a
defaultIntAxis  = LinearAxisParams {
    _la_labelf  = map show,
    _la_nLabels = 5,
    _la_nTicks  = 10
}

autoScaledIntAxis :: (Integral i, PlotValue i) =>
                     LinearAxisParams i -> AxisFn i
autoScaledIntAxis lap ps = scaledIntAxis lap rs ps
  where
    rs = (minimum ps,maximum ps)

scaledIntAxis :: (Integral i, PlotValue i) =>
                 LinearAxisParams i -> (i,i) -> AxisFn i
scaledIntAxis lap (minI,maxI) ps =
    makeAxis (_la_labelf lap) (labelvs,tickvs,gridvs)
  where
    range []  = (0,1)
    range _   | minI == maxI = (fromIntegral $ minI-1, fromIntegral $ minI+1)
              | otherwise    = (fromIntegral   minI,   fromIntegral   maxI)
--  labelvs  :: [i]
    labelvs   = stepsInt (fromIntegral $ _la_nLabels lap) r
    tickvs    = stepsInt (fromIntegral $ _la_nTicks lap)
                                  ( fromIntegral $ minimum labelvs
                                  , fromIntegral $ maximum labelvs )
    gridvs    = labelvs
    r         = range ps

stepsInt :: Integral a => a -> Range -> [a]
stepsInt nSteps range = bestSize (goodness alt0) alt0 alts
  where
    bestSize n a (a':as) = let n' = goodness a' in
                           if n' < n then bestSize n' a' as else a
    bestSize _ _ []      = []

    goodness vs          = abs (genericLength vs - nSteps)

    (alt0:alts)          = map (\n -> steps n range) sampleSteps'

    -- throw away sampleSteps that are definitely too small as
    -- they takes a long time to process                           
    sampleSteps'         = let rangeMag = ceiling (snd range - fst range)
                               
                               (s1,s2) = span (< (rangeMag `div` nSteps)) sampleSteps
                           in ((reverse . take 5 . reverse) s1) ++ s2

    -- generate all possible step sizes
    sampleSteps          = [1,2,5] ++ sampleSteps1
    sampleSteps1         = [10,20,25,50] ++ map (*10) sampleSteps1

    steps size (minV,maxV) = takeWhile (<b) [a,a+size..] ++ [b]
      where
        a = (floor   (minV / fromIntegral size)) * size
        b = (ceiling (maxV / fromIntegral size)) * size

