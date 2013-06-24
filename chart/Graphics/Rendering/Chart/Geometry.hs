module Graphics.Rendering.Chart.Geometry
  ( -- * Points and Vectors
    Rect(..)
  , Point(..)
  , Vector(..)

  , RectSize
  , Range
  
  , pointToVec

  , mkrect
  , rectPath
  , pvadd
  , pvsub
  , psub
  , vscale
  , within
  , intersectRect

  , RectEdge(..)
  , Limit(..)
  , PointMapFn
  
  -- * Paths
  , Path
  , lineTo, moveTo
  , lineTo', moveTo'
  , arc, arc'
  , arcNeg, arcNeg'
  , close
  
  , foldPath
  
  -- * Matrices
  , Matrix(..)
  , identity
  , rotate
  , scale
  , translate
  ) where

import Data.Monoid

-- | A point in two dimensions.
data Point = Point {
    p_x :: Double,
    p_y :: Double
} deriving Show

data Vector = Vector {
    v_x :: Double,
    v_y :: Double
} deriving Show

pointToVec :: Point -> Vector
pointToVec (Point x y) = Vector x y

-- | Scale a vector by a constant.
vscale :: Double -> Vector -> Vector
vscale c (Vector x y) = (Vector (x*c) (y*c))

-- | Add a point and a vector.
pvadd :: Point -> Vector -> Point
pvadd (Point x1 y1) (Vector x2 y2) = (Point (x1+x2) (y1+y2))

-- | Subtract a vector from a point.
pvsub :: Point -> Vector -> Point
pvsub (Point x1 y1) (Vector x2 y2) = (Point (x1-x2) (y1-y2))

-- | Subtract two points.
psub :: Point -> Point -> Vector
psub (Point x1 y1) (Point x2 y2) = (Vector (x1-x2) (y1-y2))

data Limit a = LMin | LValue a | LMax
   deriving Show

-- | A function mapping between points.
type PointMapFn x y = (Limit x, Limit y) -> Point

-- | A rectangle is defined by two points.
data Rect = Rect Point Point
   deriving Show

data RectEdge = E_Top | E_Bottom | E_Left | E_Right

-- | Create a rectangle based upon the coordinates of 4 points.
mkrect :: Point -> Point -> Point -> Point -> Rect
mkrect (Point x1 _) (Point _ y2) (Point x3 _) (Point _ y4) =
    Rect (Point x1 y2) (Point x3 y4)

-- | Test if a point is within a rectangle.
within :: Point -> Rect -> Bool
within (Point x y) (Rect (Point x1 y1) (Point x2 y2)) =
    x >= x1 && x <= x2 && y >= y1 && y <= y2

-- | Intersects the both rect. If they intersect the
--   intersection rectangle is returned, otherwise nothing is.
intersectRect :: Rect -> Rect -> Maybe Rect
intersectRect (Rect (Point x11 y11) (Point x12 y12)) 
              (Rect (Point x21 y21) (Point x22 y22)) =
  let p1@(Point x1 y1) = Point (max x11 x21) (max y11 y21)
      p2@(Point x2 y2) = Point (min x12 x22) (min y12 y22)
  in if x2 < x1 || y2 < y1 
        then Nothing
        else Just $ Rect p1 p2

type Range    = (Double,Double)
type RectSize = (Double,Double)

-- | Make a path from a rectangle.
rectPath :: Rect -> [Point]
rectPath (Rect p1@(Point x1 y1) p3@(Point x2 y2)) = [p1,p2,p3,p4,p1]
  where    
    p2 = (Point x1 y2)
    p4 = (Point x2 y1)

-- -----------------------------------------------------------------------
-- Path Types
-- -----------------------------------------------------------------------

data Path = MoveTo Point Path 
          | LineTo Point Path
          | Arc Point Double Double Double Path
          | ArcNeg Point Double Double Double Path
          | End 
          | Close

instance Monoid Path where
  mappend p1 p2 = case p1 of
    MoveTo p path -> MoveTo p $ mappend path p2
    LineTo p path -> LineTo p $ mappend path p2
    Arc    p r a1 a2 path -> Arc p r a1 a2 $ mappend path p2
    ArcNeg p r a1 a2 path -> ArcNeg p r a1 a2 $ mappend path p2
    End   -> p2
    Close -> Close
  mempty = End

moveTo :: Point -> Path
moveTo p = MoveTo p mempty

moveTo' :: Double -> Double -> Path
moveTo' x y = moveTo $ Point x y

lineTo :: Point -> Path
lineTo p = LineTo p mempty

lineTo' :: Double -> Double -> Path
lineTo' x y = lineTo $ Point x y

arc :: Point -> Double -> Double -> Double -> Path
arc p r a1 a2 = Arc p r a1 a2 mempty

arc' :: Double -> Double -> Double -> Double -> Double -> Path
arc' x y r a1 a2 = Arc (Point x y) r a1 a2 mempty

arcNeg :: Point -> Double -> Double -> Double -> Path
arcNeg p r a1 a2 = ArcNeg p r a1 a2 mempty

arcNeg' :: Double -> Double -> Double -> Double -> Double -> Path
arcNeg' x y r a1 a2 = ArcNeg (Point x y) r a1 a2 mempty

close :: Path
close = Close

-- | Fold the given path to a monoid structure.
foldPath :: (Monoid m)
         => (Point -> m) -- ^ MoveTo
         -> (Point -> m) -- ^ LineTo
         -> (Point -> Double -> Double -> Double -> m) -- ^ Arc
         -> (Point -> Double -> Double -> Double -> m) -- ^ ArcNeg
         -> m    -- ^ Close
         -> Path -- ^ Path to fold
         -> m
foldPath moveTo lineTo arc arcNeg close path = 
  let restF = foldPath moveTo lineTo arc arcNeg close
  in case path of 
    MoveTo p rest -> moveTo p <> restF rest
    LineTo p rest -> lineTo p <> restF rest
    Arc    p r a1 a2 rest -> arc    p r a1 a2 <> restF rest
    ArcNeg p r a1 a2 rest -> arcNeg p r a1 a2 <> restF rest
    End   -> mempty
    Close -> close

-- -----------------------------------------------------------------------
-- Matrix Type
-- -----------------------------------------------------------------------

-- | Copied from Graphics.Rendering.Cairo.Matrix
data Matrix = Matrix { xx :: !Double, yx :: !Double,
                       xy :: !Double, yy :: !Double,
                       x0 :: !Double, y0 :: !Double }

-- | Copied from Graphics.Rendering.Cairo.Matrix
instance Num Matrix where
  (*) (Matrix xx yx xy yy x0 y0) (Matrix xx' yx' xy' yy' x0' y0') =
    Matrix (xx * xx' + yx * xy')
           (xx * yx' + yx * yy')
           (xy * xx' + yy * xy')
           (xy * yx' + yy * yy')
           (x0 * xx' + y0 * xy' + x0')
           (x0 * yx' + y0 * yy' + y0')

  (+) = pointwise2 (+)
  (-) = pointwise2 (-)

  negate = pointwise negate
  abs    = pointwise abs
  signum = pointwise signum
  
  fromInteger n = Matrix (fromInteger n) 0 0 (fromInteger n) 0 0

-- | Copied from Graphics.Rendering.Cairo.Matrix
{-# INLINE pointwise #-}
pointwise f (Matrix xx yx xy yy x0 y0) =
  Matrix (f xx) (f yx) (f xy) (f yy) (f x0) (f y0)

-- | Copied from Graphics.Rendering.Cairo.Matrix
{-# INLINE pointwise2 #-}
pointwise2 f (Matrix xx yx xy yy x0 y0) (Matrix xx' yx' xy' yy' x0' y0') =
  Matrix (f xx xx') (f yx yx') (f xy xy') (f yy yy') (f x0 x0') (f y0 y0')

-- | Copied from Graphics.Rendering.Cairo.Matrix
identity :: Matrix
identity = Matrix 1 0 0 1 0 0

-- | Copied and adopted from Graphics.Rendering.Cairo.Matrix
translate :: Vector -> Matrix -> Matrix
translate tv m = m * (Matrix 1 0 0 1 (v_x tv) (v_y tv))

-- | Copied and adopted from Graphics.Rendering.Cairo.Matrix
scale :: Vector -> Matrix -> Matrix
scale sv m = m * (Matrix (v_x sv) 0 0 (v_y sv) 0 0)

-- | Copied from Graphics.Rendering.Cairo.Matrix
--   Rotations angle is given in radians.
rotate :: Double -> Matrix -> Matrix
rotate r m = m * (Matrix c s (-s) c 0 0)
  where s = sin r
        c = cos r
















