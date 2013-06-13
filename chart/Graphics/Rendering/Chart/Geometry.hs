module Graphics.Rendering.Chart.Geometry
  ( Rect(..)
  , Point(..)
  , Vector(..)

  , RectSize
  , Range

  , mkrect
  , rectPath
  , pvadd
  , pvsub
  , psub
  , vscale
  , within

  , RectEdge(..)
  , Limit(..)
  , PointMapFn
    
  , Matrix
  , identity
  , rotate
  , scale
  , translate
  ) where

-- | A point in two dimensions.
data Point = Point {
    p_x :: Double,
    p_y :: Double
} deriving Show

data Vector = Vector {
    v_x :: Double,
    v_y :: Double
} deriving Show

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

type Range    = (Double,Double)
type RectSize = (Double,Double)

-- | Make a path from a rectangle.
rectPath :: Rect -> [Point]
rectPath (Rect p1@(Point x1 y1) p3@(Point x2 y2)) = [p1,p2,p3,p4,p1]
  where    
    p2 = (Point x1 y2)
    p4 = (Point x2 y1)

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
rotate :: Double -> Matrix -> Matrix
rotate r m = m * (Matrix c s (-s) c 0 0)
  where s = sin r
        c = cos r
















