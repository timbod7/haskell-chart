{-# LANGUAGE PatternGuards #-}

module Numeric.Histogram ( Range
                         , binBounds
                         , histValues
                         , histWeightedValues
                         , histWithBins
                         ) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST

type Range a = (a,a)

-- | 'binBounds a b n' generates bounds for 'n' bins spaced linearly between
-- 'a' and 'b'
--
-- Examples:
--
-- >>> binBounds 0 3 4
-- [(0.0,0.75),(0.75,1.5),(1.5,2.25),(2.25,3.0)]
binBounds :: RealFrac a => a -> a -> Int -> [Range a]
binBounds a b n = map (\i->(lbound i, lbound (i+1))) [0..n-1]
        where lbound i = a + (b-a) * realToFrac i / realToFrac n

-- | 'histValues a b n vs' returns the bins for the histogram of
-- 'vs' on the range from 'a' to 'b' with 'n' bins
histValues :: RealFrac a => a -> a -> Int -> [a] -> V.Vector (Range a, Int)
histValues a b n = histWithBins (V.fromList $ binBounds a b n) . zip (repeat 1)

-- | 'histValues a b n vs' returns the bins for the weighted histogram of
-- 'vs' on the range from 'a' to 'b' with 'n' bins
histWeightedValues :: RealFrac a => a -> a -> Int -> [(Double,a)] -> V.Vector (Range a, Double)
histWeightedValues a b n = histWithBins (V.fromList $ binBounds a b n)

-- | 'histWithBins bins xs' is the histogram of weighted values 'xs' with 'bins'
--
-- Examples:
--
-- >>> :{
-- histWithBins
--     (V.fromList [(0.0, 0.75), (0.75, 1.5), (1.5, 2.25), (2.25, 3.0)])
--     [(1, 0), (1, 0), (1, 1), (1, 2), (1, 2), (1, 2), (1, 3)]
-- :}
-- [((0.0,0.75),2),((0.75,1.5),1),((1.5,2.25),3),((2.25,3.0),1)]
histWithBins :: (Num w, RealFrac a) => V.Vector (Range a) -> [(w, a)] -> V.Vector (Range a, w)
histWithBins bins xs =
    let n = V.length bins
        testBin :: RealFrac a => a -> (Int, Range a) -> Bool
        testBin x (i, (a,b)) =
            if i == n - 1
                then x >= a && x <= b
                else x >= a && x < b

        f :: (RealFrac a, Num w)
          => V.Vector (Range a) -> MV.STVector s w -> (w, a)
          -> ST s ()
        f bins1 bs (w,x) =
            case V.dropWhile (not . testBin x) $ V.indexed bins1 of
                v | V.null v  -> return ()
                v | (idx,_) <- V.head v  -> do
                    m <- MV.read bs idx
                    MV.write bs idx $! m+w

        counts = runST $ do b <- MV.replicate n 0
                            mapM_ (f bins b) xs
                            V.freeze b
    in V.zip bins counts
