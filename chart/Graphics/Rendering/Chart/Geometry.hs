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
  , Path(..)
  , lineTo, moveTo
  , lineTo', moveTo'
  , arc, arc'
  , arcNeg, arcNeg'
  , close
  
  , foldPath
  , makeLinesExplicit
  
  -- * Matrices
  , transformP, scaleP, rotateP, translateP
  , Matrix(..)
  , identity
  , rotate, scale, translate
  , scalarMultiply
  , adjoint
  , invert
  ) where

import Data.Monoid

-- | A point in two dimensions.
data Point = Point {
    p_x :: Double,
    p_y :: Double
} deriving Show

-- | A vector in two dimensions.
data Vector = Vector {
    v_x :: Double,
    v_y :: Double
} deriving Show

-- | Convert a 'Point' to a 'Vector'.
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

-- | Edge of a rectangle.
data RectEdge = E_Top | E_Bottom | E_Left | E_Right

-- | Create a rectangle based upon the coordinates of 4 points.
mkrect :: Point -> Point -> Point -> Point -> Rect
mkrect (Point x1 _) (Point _ y2) (Point x3 _) (Point _ y4) =
    Rect (Point x1 y2) (Point x3 y4)

-- | Test if a point is within a rectangle.
within :: Point -> Rect -> Bool
within (Point x y) (Rect (Point x1 y1) (Point x2 y2)) =
    x >= x1 && x <= x2 && y >= y1 && y <= y2

-- | Intersects the rectangles. If they intersect the
--   intersection rectangle is returned.
--   'LMin' is the empty rectangle / intersection and
--   'LMax' is the infinite plane.
intersectRect :: Limit Rect -> Limit Rect -> Limit Rect
intersectRect LMax r = r
intersectRect r LMax = r
intersectRect LMin _ = LMin
intersectRect _ LMin = LMin
intersectRect (LValue (Rect (Point x11 y11) (Point x12 y12))) 
              (LValue (Rect (Point x21 y21) (Point x22 y22))) =
  let p1@(Point x1 y1) = Point (max x11 x21) (max y11 y21)
      p2@(Point x2 y2) = Point (min x12 x22) (min y12 y22)
  in if x2 < x1 || y2 < y1 
        then LMin
        else LValue $ Rect p1 p2

type Range    = (Double,Double)
type RectSize = (Double,Double)

{-
-- | Make a path from a rectangle.
rectPointPath :: Rect -> [Point]
rectPointPath (Rect p1@(Point x1 y1) p3@(Point x2 y2)) = [p1,p2,p3,p4,p1]
  where    
    p2 = (Point x1 y2)
    p4 = (Point x2 y1)
-}

-- | Make a path from a rectangle.
rectPath :: Rect -> Path
rectPath (Rect p1@(Point x1 y1) p3@(Point x2 y2)) = 
  let p2 = (Point x1 y2)
      p4 = (Point x2 y1)
  in moveTo p1 <> lineTo p2 <> lineTo p3 <> lineTo p4 <> close

-- -----------------------------------------------------------------------
-- Path Types
-- -----------------------------------------------------------------------

-- | The path type used by Charts.
--   
--   A path can consist of several subpaths. Each
--   is started by a 'MoveTo' operation. All subpaths
--   are open, except the last one, which may be closed
--   using the 'Close' operation. When filling a path
--   all subpaths are closed implicitly.
--   
--   Closing a subpath means that a line is drawn from
--   the end point to the start point of the subpath.
--   
--   If a 'Arc' (or 'ArcNeg') is drawn a implicit line
--   from the last end point of the subpath is drawn
--   to the beginning of the arc. Another implicit line
--   is drawn from the end of an arc to the beginning of
--   the next path segment.
--   
--   The beginning of a subpath is either (0,0) or set
--   by a 'MoveTo' instruction. If the first subpath is started
--   with an arc the beginning of that subpath is the beginning
--   of the arc.
data Path = MoveTo Point Path 
          | LineTo Point Path
          | Arc Point Double Double Double Path
          | ArcNeg Point Double Double Double Path
          | End 
          | Close

-- | Paths are monoids. After a path is closed you can not append
--   anything to it anymore. The empty path is open. 
--   Use 'close' to close a path.
instance Monoid Path where
  mappend p1 p2 = case p1 of
    MoveTo p path -> MoveTo p $ mappend path p2
    LineTo p path -> LineTo p $ mappend path p2
    Arc    p r a1 a2 path -> Arc p r a1 a2 $ mappend path p2
    ArcNeg p r a1 a2 path -> ArcNeg p r a1 a2 $ mappend path p2
    End   -> p2
    Close -> Close
  mempty = End

-- | Move the paths pointer to the given location.
moveTo :: Point -> Path
moveTo p = MoveTo p mempty

-- | Short-cut for 'moveTo', if you don't want to create a 'Point'.
moveTo' :: Double -> Double -> Path
moveTo' x y = moveTo $ Point x y

-- | Move the paths pointer to the given location and draw a straight 
--   line while doing so.
lineTo :: Point -> Path
lineTo p = LineTo p mempty

-- | Short-cut for 'lineTo', if you don't want to create a 'Point'.
lineTo' :: Double -> Double -> Path
lineTo' x y = lineTo $ Point x y

-- | Draw the arc of a circle. A straight line connects
--   the end of the previous path with the beginning of the arc.
--   The zero angle points in direction of the positive x-axis.
--   Angles increase in clock-wise direction. If the stop angle
--   is smaller then the start angle it is increased by multiples of
--   @2 * pi@ until is is greater or equal.
arc :: Point  -- ^ Center point of the circle arc.
    -> Double -- ^ Redius of the circle.
    -> Double -- ^ Angle to start drawing at, in radians.
    -> Double -- ^ Angle to stop drawing at, in radians.
    -> Path
arc p r a1 a2 = Arc p r a1 a2 mempty

-- | Short-cut for 'arc', if you don't want to create a 'Point'.
arc' :: Double -> Double -> Double -> Double -> Double -> Path
arc' x y r a1 a2 = Arc (Point x y) r a1 a2 mempty

-- | Like 'arc', but draws from the stop angle to the start angle
--   instead of between them.
arcNeg :: Point -> Double -> Double -> Double -> Path
arcNeg p r a1 a2 = ArcNeg p r a1 a2 mempty

-- | Short-cut for 'arcNeg', if you don't want to create a 'Point'.
arcNeg' :: Double -> Double -> Double -> Double -> Double -> Path
arcNeg' x y r a1 a2 = ArcNeg (Point x y) r a1 a2 mempty

-- | A closed empty path. Closes a path when appended.
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

-- | Enriches the path with explicit instructions to draw lines,
--   that otherwise would be implicit. See 'Path' for details
--   about what lines in paths are implicit.
makeLinesExplicit :: Path -> Path
makeLinesExplicit (Arc c r s e rest) = 
  Arc c r s e $ makeLinesExplicit' rest
makeLinesExplicit (ArcNeg c r s e rest) = 
  ArcNeg c r s e $ makeLinesExplicit' rest
makeLinesExplicit path = makeLinesExplicit' path

-- | Utility for 'makeLinesExplicit'.
makeLinesExplicit' :: Path -> Path
makeLinesExplicit' End   = End
makeLinesExplicit' Close = Close
makeLinesExplicit' (Arc c r s e rest) = 
  let p = translateP (pointToVec c) $ rotateP s $ Point r 0
  in lineTo p <> arc c r s e <> makeLinesExplicit' rest
makeLinesExplicit' (ArcNeg c r s e rest) = 
  let p = translateP (pointToVec c) $ rotateP s $ Point r 0
  in lineTo p <> arcNeg c r s e <> makeLinesExplicit' rest
makeLinesExplicit' (MoveTo p0 rest) = 
  MoveTo p0 $ makeLinesExplicit' rest
makeLinesExplicit' (LineTo p0 rest) = 
  LineTo p0 $ makeLinesExplicit' rest

-- -----------------------------------------------------------------------
-- Matrix Type
-- -----------------------------------------------------------------------

-- | Transform a point using the given matrix.
transformP :: Matrix -> Point -> Point
transformP t (Point x y) = Point
  (xx t * x + xy t * y + x0 t)
  (yx t * x + yy t * y + y0 t)

-- | Rotate a point around the origin.
--   The angle is given in radians.
rotateP :: Double -> Point -> Point
rotateP a = transformP (rotate a 1)

-- | Scale a point.
scaleP :: Vector -> Point -> Point
scaleP s = transformP (scale s 1)

-- | Translate a point.
translateP :: Vector -> Point -> Point
translateP = flip pvadd

-- | Copied from Graphics.Rendering.Cairo.Matrix
data Matrix = Matrix { xx :: !Double, yx :: !Double,
                       xy :: !Double, yy :: !Double,
                       x0 :: !Double, y0 :: !Double }
                     deriving Show

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

-- | Copied from Graphics.Rendering.Cairo.Matrix
scalarMultiply :: Double -> Matrix -> Matrix
scalarMultiply scalar = pointwise (* scalar)

-- | Copied from Graphics.Rendering.Cairo.Matrix
adjoint :: Matrix -> Matrix
adjoint (Matrix a b c d tx ty) =
  Matrix d (-b) (-c) a (c*ty - d*tx) (b*tx - a*ty)

-- | Copied from Graphics.Rendering.Cairo.Matrix
invert :: Matrix -> Matrix
invert m@(Matrix xx yx xy yy _ _) = scalarMultiply (recip det) $ adjoint m
  where det = xx*yy - yx*xy












