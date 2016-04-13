-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Grid
-- Copyright   :  (c) Tim Docker 2010, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- A container type for values that can be composed by horizonal
-- and vertical layout.

module Graphics.Rendering.Chart.Grid (
    Grid, Span, SpaceWeight,
    tval, tspan,
    empty, nullt,
    (.|.), (./.),
    above, aboveN,
    beside, besideN,
    overlay,
    width, height,
    gridToRenderable,
    weights,
    aboveWide,
    wideAbove,
    tallBeside,
    besideTall,
    fullOverlayUnder,
    fullOverlayOver
) where

import Data.Array
import Control.Monad
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Geometry hiding (x0, y0)
import Graphics.Rendering.Chart.Drawing

type Span        = (Int,Int)
type Size        = (Int,Int)

-- | When more space is available for an item than the total width of items,
--   extra added space is proportional to 'space weight'.
type SpaceWeight = (Double,Double)

type Cell a      = (a,Span,SpaceWeight)

-- | Abstract datatype representing a grid.
data Grid a
    = Value (a,Span,SpaceWeight)       -- ^ A singleton grid item "a" spanning
                                       --   a given rectangle (measured in grid
                                       --   cells), with given space weight.
    | Above (Grid a) (Grid a) Size     -- ^ One grid above the other. "Size" is
                                       --   their cached total size (so it is
                                       --   NOT specified manually).
    | Beside (Grid a) (Grid a) Size    -- ^ One grid horizontally beside
                                       --   the other.
    | Overlay (Grid a) (Grid a) Size   -- ^ Two grids positioned one over
                                       --   the other.
    | Empty                            -- ^ An empty 1x1 grid cell.
    | Null                             -- ^ An empty 0x0 grid cell.
   deriving (Show)

width :: Grid a -> Int
width Null                = 0
width Empty               = 1
width (Value _)           = 1
width (Beside _ _ (w,_))  = w
width (Above _ _ (w,_))   = w
width (Overlay _ _ (w,_)) = w

height :: Grid a -> Int
height Null                = 0
height Empty               = 1
height (Value _)           = 1
height (Beside _ _ (_,h))  = h
height (Above _ _ (_,h))   = h
height (Overlay _ _ (_,h)) = h

-- | A 1x1 grid from a given value, with no extra space.
tval :: a -> Grid a
tval a = Value (a,(1,1),(0,0))

-- | A WxH (measured in cells) grid from a given value, with space weight (1,1).
tspan :: a -> Span -> Grid a
tspan a spn = Value (a,spn,(1,1))

-- | A 1x1 empty grid.
empty :: Grid a
empty = Empty

-- | A 0x0 empty grid.
nullt :: Grid a
nullt = Null

above, beside :: Grid a -> Grid a -> Grid a
above Null t = t
above t Null = t
above t1 t2  = Above t1 t2 size
  where size = (max (width t1) (width t2), height t1 + height t2)

-- | A value occupying 1 row with the same  horizontal span as the grid.
wideAbove :: a -> Grid a -> Grid a
wideAbove a g = weights (0,0) (tspan a (width g,1)) `above` g

-- | A value placed below the grid, occupying 1 row with the same
--   horizontal span as the grid.
aboveWide :: Grid a -> a -> Grid a
aboveWide g a = g `above` weights (0,0) (tspan a (width g,1))

-- | A value placed to the left of the grid, occupying 1 column with
--   the same vertical span as the grid.
tallBeside  :: a -> Grid a -> Grid a
tallBeside  a g = weights (0,0) (tspan a (1,height g)) `beside` g

-- | A value placed to the right of the grid, occupying 1 column with
--   the same vertical span as the grid.
besideTall :: Grid a -> a -> Grid a
besideTall g a = g `beside` weights (0,0) (tspan a (1,height g))

-- | A value placed under a grid, with the same span as the grid.
fullOverlayUnder :: a -> Grid a -> Grid a
fullOverlayUnder a g = g `overlay` tspan a (width g,height g)

-- | A value placed over a grid, with the same span as the grid.
fullOverlayOver :: a -> Grid a -> Grid a
fullOverlayOver  a g = tspan a (width g,height g) `overlay` g

beside Null t = t
beside t Null = t
beside t1 t2  = Beside t1 t2 size
  where size  = (width t1 + width t2, max (height t1) (height t2))

aboveN, besideN :: [Grid a] -> Grid a
aboveN  = foldl above nullt
besideN = foldl beside nullt

-- | One grid over the other. The first argument is shallow, the second is deep.
overlay :: Grid a -> Grid a -> Grid a
overlay Null t = t
overlay t Null = t
overlay t1 t2  = Overlay t1 t2 size
  where size   = (max (width t1) (width t2), max (height t1) (height t2))

-- | A synonym for 'beside'.
(.|.) :: Grid a -> Grid a -> Grid a
(.|.) = beside

-- | A synonym for 'above'.
(./.) :: Grid a -> Grid a -> Grid a
(./.) = above

-- | Sets the space weight of *every* cell of the grid to given value.
weights :: SpaceWeight -> Grid a -> Grid a
weights _  Null               = Null
weights _  Empty              = Empty
weights sw (Value (v,sp,_))   = Value   (v,sp,sw)
weights sw (Above t1 t2 sz)   = Above   (weights sw t1) (weights sw t2) sz
weights sw (Beside t1 t2 sz)  = Beside  (weights sw t1) (weights sw t2) sz
weights sw (Overlay t1 t2 sz) = Overlay (weights sw t1) (weights sw t2) sz

-- fix me, need to make .|. and .||. higher precedence
-- than ./. and .//.

instance Functor Grid where
    fmap f (Value (a,spn,ew))  = Value   (f a,spn,ew)
    fmap f (Above t1 t2 s)     = Above   (fmap f t1) (fmap f t2) s
    fmap f (Beside t1 t2 s)    = Beside  (fmap f t1) (fmap f t2) s
    fmap f (Overlay t1 t2 s)   = Overlay (fmap f t1) (fmap f t2) s
    fmap _ Empty               = Empty
    fmap _ Null                = Null

mapGridM :: Monad m => (a -> m b) -> Grid a -> m (Grid b)
mapGridM f (Value (a,spn,ew)) = do b <- f a
                                   return (Value (b,spn,ew))
mapGridM f (Above t1 t2 s)    = do t1' <- mapGridM f t1
                                   t2' <- mapGridM f t2
                                   return (Above t1' t2' s)
mapGridM f (Beside t1 t2 s)   = do t1' <- mapGridM f t1
                                   t2' <- mapGridM f t2
                                   return (Beside t1' t2' s)
mapGridM f (Overlay t1 t2 s)  = do t1' <- mapGridM f t1
                                   t2' <- mapGridM f t2
                                   return (Overlay t1' t2' s)
mapGridM _ Empty              = return Empty
mapGridM _ Null               = return Null

----------------------------------------------------------------------
type FlatGrid a = Array (Int,Int) [(a,Span,SpaceWeight)]

flatten :: Grid a -> FlatGrid a
flatten t = accumArray (flip (:)) [] ((0,0), (width t - 1, height t - 1))
                       (flatten2 (0,0) t [])

type FlatEl a = ((Int,Int),Cell a)

flatten2 :: (Int,Int) -> Grid a -> [FlatEl a] -> [FlatEl a]
flatten2 _ Empty        els = els
flatten2 _ Null         els = els
flatten2 i (Value cell) els = (i,cell):els

flatten2 i@(x,y) (Above t1 t2 _) els   = (f1.f2) els
  where
    f1 = flatten2 i t1
    f2 = flatten2 (x,y + height t1) t2

flatten2 i@(x,y) (Beside t1 t2 _) els  = (f1.f2) els
  where
    f1 = flatten2 i t1
    f2 = flatten2 (x + width t1, y) t2

flatten2 i (Overlay t1 t2 _) els = (f1.f2) els
  where
    f1 = flatten2 i t1
    f2 = flatten2 i t2

foldT :: ((Int,Int) -> Cell a -> r -> r) -> r -> FlatGrid a -> r
foldT f iv ft = foldr f' iv (assocs ft)
  where
    f' (i,vs) r = foldr (f i) r vs

----------------------------------------------------------------------
type DArray = Array Int Double

getSizes :: Grid (Renderable a) -> CBProgram (DArray, DArray, DArray, DArray)
getSizes t = do
    szs <- mapGridM minsize t :: CBProgram (Grid RectSize)
    let szs'     = flatten szs
    let widths   = accumArray max 0 (0, width  t - 1)
                                                   (foldT (ef wf  fst) [] szs')
    let heights  = accumArray max 0 (0, height t - 1)
                                                   (foldT (ef hf  snd) [] szs')
    let xweights = accumArray max 0 (0, width  t - 1)
                                                   (foldT (ef xwf fst) [] szs')
    let yweights = accumArray max 0 (0, height t - 1)
                                                   (foldT (ef ywf snd) [] szs')
    return (widths,heights,xweights,yweights)
  where
      wf  (x,_) (w,_) _      = (x,w)
      hf  (_,y) (_,h) _      = (y,h)
      xwf (x,_) _     (xw,_) = (x,xw)
      ywf (_,y) _     (_,yw) = (y,yw)

      ef f ds loc (size,spn,ew) r | ds spn == 1 = f loc size ew:r
                                  | otherwise    = r

instance (ToRenderable a) => ToRenderable (Grid a) where
  toRenderable = gridToRenderable . fmap toRenderable

gridToRenderable :: Grid (Renderable a) -> Renderable a
gridToRenderable gt = Renderable minsizef renderf
  where
    minsizef :: CBProgram RectSize
    minsizef = do
        (widths, heights, _, _) <- getSizes gt
        return (sum (elems widths), sum (elems heights))

    renderf (w,h)  = do
        (widths, heights, xweights, yweights) <- getSizes gt
        let widths'  = addExtraSpace w widths xweights
        let heights' = addExtraSpace h heights yweights
        let borders  = (ctotal widths',ctotal heights')
        rf1 borders (0,0) gt

    -- (x borders, y borders) -> (x,y) -> grid -> drawing
    rf1 borders loc@(i,j) t = case t of
        Null  -> return nullPickFn
        Empty -> return nullPickFn
        (Value (r,spn,_)) -> do
            let (Rect p0 p1) = mkRect borders loc spn
            (Point x0 y0) <- alignFillPoint p0
            (Point x1 y1) <- alignFillPoint p1
            withTranslation (Point x0 y0) $ do
              pf <- render r (x1-x0,y1-y0)
              return (newpf pf x0 y0)
        (Above t1 t2 _) -> do
             pf1 <- rf1 borders (i,j) t1
             pf2 <- rf1 borders (i,j+height t1) t2
             let pf p@(Point _ y) = if y < (snd borders ! (j + height t1))
                                    then pf1 p else pf2 p
             return pf
        (Beside t1 t2 _) -> do
             pf1 <- rf1 borders (i,j) t1
             pf2 <- rf1 borders (i+width t1,j) t2
             let pf p@(Point x _) = if x < (fst borders ! (i + width t1))
                                    then pf1 p else pf2 p
             return pf
        (Overlay t1 t2 _) ->  do
             pf2 <- rf1 borders (i,j) t2
             pf1 <- rf1 borders (i,j) t1
             let pf p = pf1 p `mplus` pf2 p
             return pf

    newpf pf x0 y0 (Point x1 y1) = pf (Point (x1-x0) (y1-y0))

    -- (x borders, y borders) -> (x,y) -> (w,h)
    --     -> rectangle of grid[x..x+w, y..y+h]
    mkRect :: (DArray, DArray) -> (Int,Int) -> (Int,Int) -> Rect
    mkRect (cwidths,cheights) (x,y) (w,h) = Rect (Point x1 y1) (Point x2 y2)
      where
        x1 = cwidths  ! x
        y1 = cheights ! y
        x2 = cwidths  ! min (x+w) (snd $ bounds cwidths)
        y2 = cheights ! min (y+h) (snd $ bounds cheights)
        -- mx = fst (bounds cwidths)
        -- my = fst (bounds cheights)

    -- total size -> item sizes -> item weights -> new item sizes such that
    -- their sum == total size, and added size is proportional to weight
    addExtraSpace :: Double -> DArray -> DArray -> DArray
    addExtraSpace size sizes weights' =
        if totalws == 0 then sizes
                        else listArray (bounds sizes) sizes'
      where
        ws      = elems weights'
        totalws = sum ws
        extra   = size - sum (elems sizes)
        extras  = map (*(extra/totalws)) ws
        sizes'  = zipWith (+) extras (elems sizes)

    -- [1,2,3] -> [0,1,3,6].
    ctotal :: DArray -> DArray
    ctotal a = listArray (let (i,j) = bounds a in (i,j+1))
                         (scanl (+) 0 (elems a))
