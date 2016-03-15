{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

-- | The backend to render charts with the diagrams library.
module Graphics.Rendering.Chart.Backend.Serialize where

import Data.Serialize
import qualified Data.Serialize.Put as Put
import qualified Data.Serialize.Get as Get
import qualified Graphics.SVGFonts.ReadFont as ReadFont
import Diagrams.Path (Path(..))
import Diagrams.Located (Located(..))
import Diagrams.Trail (Trail(..), Trail'(..), SegTree(..))
import Diagrams.Segment (Segment(..), Offset(..), Open, Closed, SegMeasure)
import Diagrams.Prelude (V2)

import Data.Vector.Serialize

import Control.Monad (replicateM)
import qualified Data.FingerTree as FingerTree
import Data.FingerTree (FingerTree)
import qualified Data.Foldable as Foldable

import GHC.Generics (Generic)

deriving instance Generic (Path v n)
deriving instance Generic (Located a)
deriving instance Generic (ReadFont.Kern n)
deriving instance Generic (ReadFont.FontData n)

instance Serialize (Path V2 Double)
instance Serialize (Located (Trail V2 Double))
instance Serialize (ReadFont.Kern Double)
instance Serialize (ReadFont.FontData Double)

instance Serialize (FingerTree (SegMeasure V2 Double) (Segment Closed V2 Double)) where
  {-# INLINE put #-}
  put xs = Put.putListOf putClosedSegment (Foldable.toList xs)

  {-# INLINE get #-}
  get = FingerTree.fromList <$> Get.getListOf getClosedSegment

instance Serialize (Trail V2 Double) where
  {-# INLINE get #-}
  get = do
    isLine <- get
    case isLine of
      True  -> do
        fingerTree <- get
        return (Trail (Line (SegTree fingerTree)))
      False -> do
        fingerTree <- get
        segment <- getOpenSegment
        return (Trail (Loop (SegTree fingerTree) segment))

  {-# INLINE put #-}
  put (Trail (Line (SegTree fingerTree))) = do
    put True
    put fingerTree

  put (Trail (Loop (SegTree fingerTree) segment)) = do
    put False
    put fingerTree
    putOpenSegment segment

{-# INLINE putOpenSegment #-}
putOpenSegment :: Segment Open V2 Double -> Put
putOpenSegment segment = do
  put True
  case segment of
    Linear OffsetOpen    -> put True
    Cubic v w OffsetOpen -> put False >> put v >> put w

{-# INLINE getOpenSegment #-}
getOpenSegment :: Get (Segment Open V2 Double)
getOpenSegment = do
  isOpen <- get
  if isOpen
    then do
      isLinear <- get
      case isLinear of
        True  -> return (Linear OffsetOpen)
        False -> do
          v <- get
          w <- get
          return (Cubic v w OffsetOpen)
    else error "isOpen should be true"

{-# INLINE putClosedSegment #-}
putClosedSegment :: Segment Closed V2 Double -> Put
putClosedSegment segment = do
  put False
  case segment of
    Linear (OffsetClosed z)    -> put z >> put True
    Cubic v w (OffsetClosed z) -> put z >> put False >> put v >> put w

{-# INLINE getClosedSegment #-}
getClosedSegment :: Get (Segment Closed V2 Double)
getClosedSegment = do
  isOpen <- get
  if isOpen
    then error "isOpen should be false"
    else do
      z <- get
      isLinear <- get
      case isLinear of
        True  -> return (Linear (OffsetClosed z))
        False -> do
          v <- get
          w <- get
          return (Cubic v w (OffsetClosed z))
