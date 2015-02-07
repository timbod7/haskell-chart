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
binBounds :: RealFrac a => a -> a -> Int -> [Range a]
binBounds a b n = map (\i->(lbound i, lbound (i+1))) [0..n]
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
histWithBins :: (Num w, RealFrac a) => V.Vector (Range a) -> [(w, a)] -> V.Vector (Range a, w)
histWithBins bins xs =
        let testBin :: RealFrac a => a -> Range a -> Bool
            testBin x (a,b) = x >= a && x < b

            f :: (RealFrac a, Num w)
              => V.Vector (Range a) -> MV.STVector s w -> (w, a)
              -> ST s ()
            f bins bs (w,x) =
                case V.dropWhile (not . testBin x . snd) $ V.indexed bins of
                    v | V.null v  -> return ()
                    v | (idx,bounds) <- V.head v  -> do
                        n <- MV.read bs idx
                        MV.write bs idx $! n+w

            counts = runST $ do b <- MV.replicate (V.length bins) 0
                                mapM_ (f bins b) xs
                                V.freeze b
        in V.zip bins counts
