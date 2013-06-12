-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Renderable
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- This module contains the definition of the 'Renderable' type, which
-- is a composable drawing element, along with assorted functions to
-- them.
--

{-# LANGUAGE TypeFamilies #-}

module Graphics.Rendering.Chart.Renderable(
    Renderable(..),
    --ToRenderable(..), -- TODO: See class definition
    PickFn,
    
    rectangleToRenderable,

    fillBackground,
    addMargins,
    emptyRenderable,
    embedRenderable,
    label,
    rlabel,
    spacer,
    spacer1,
    setPickFn,
    mapMaybePickFn,
    mapPickFn,
    nullPickFn,

    rect_minsize,
    rect_fillStyle,
    rect_lineStyle,
    rect_cornerStyle,
) where

import Control.Monad
import Data.Accessor
import Data.List ( nub, transpose, sort )

import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Utils

-- | A function that maps a point in device coordinates to some value.
--
--   Perhaps it might be generalised from Maybe a to
--   (MonadPlus m ) => m a in the future.
type PickFn a = Point -> (Maybe a)

nullPickFn :: PickFn a
nullPickFn = const Nothing

-- | A Renderable is a record of functions required to layout a
--   graphic element.
data Renderable m a = Renderable {

   -- | A Cairo action to calculate a minimum size.
   minsize :: m RectSize,

   -- | A Cairo action for drawing it within a rectangle.
   --   The rectangle is from the origin to the given point.
   --
   --   The resulting "pick" function  maps a point in the image to a value.
   render  :: RectSize -> m (PickFn a)
}

{- TODO: Removed this because in all example using it, it is to ambigious.
-- | A type class abtracting the conversion of a value to a Renderable.
class ToRenderable a where
  type RenderableT m b :: *
  toRenderable :: (ChartBackend m) => RenderableT m a -> Renderable m ()
-}
emptyRenderable :: (ChartBackend m) => Renderable m a
emptyRenderable = spacer (0,0)

-- | Create a blank renderable with a specified minimum size.
spacer :: (ChartBackend m) => RectSize -> Renderable m a 
spacer sz  = Renderable {
   minsize = return sz,
   render  = \_ -> return nullPickFn
}


-- | Create a blank renderable with a minimum size the same as
--   some other renderable.
spacer1 :: (ChartBackend m) => Renderable m a -> Renderable m b
spacer1 r  = r{ render  = \_ -> return nullPickFn }

-- | Replace the pick function of a renderable with another.
setPickFn :: (ChartBackend m) => PickFn b -> Renderable m a -> Renderable m b
setPickFn pickfn r = r{ render  = \sz -> do { render r sz; return pickfn; } }

-- | Map a function over the result of a renderable's pickfunction, keeping only 'Just' results.
mapMaybePickFn :: (ChartBackend m) => (a -> Maybe b) -> Renderable m a -> Renderable m b
mapMaybePickFn f r = r{ render = \sz -> do pf <- render r sz
                                           return (join . fmap f . pf) }

-- | Map a function over result of a renderable's pickfunction.
mapPickFn :: (ChartBackend m) => (a -> b) -> Renderable m a -> Renderable m b
mapPickFn f = mapMaybePickFn (Just . f)

-- | Add some spacing at the edges of a renderable.
addMargins :: (ChartBackend m) => (Double,Double,Double,Double) -- ^ The spacing to be added.
           -> Renderable m a                  -- ^ The source renderable.
           -> Renderable m a
addMargins (t,b,l,r) rd = Renderable { minsize = mf, render = rf }
  where
    mf = do
        (w,h) <- minsize rd
        return (w+l+r,h+t+b)

    rf (w,h) = do
        bLocal $ do
            bTranslate (Point l t)
            pickf <- render rd (w-l-r,h-t-b)
            return (mkpickf pickf (t,b,l,r) (w,h))

    mkpickf pickf (t,b,l,r) (w,h) (Point x y)
        | x >= l && x <= w-r && y >= t && t <= h-b = pickf (Point (x-l) (y-t))
        | otherwise                                = Nothing

-- | Overlay a renderable over a solid background fill.
fillBackground :: (ChartBackend m) => FillStyle -> Renderable m a -> Renderable m a
fillBackground fs r = r{ render = rf }
  where
    rf rsize@(w,h) = do
        bLocal $ do
            bSetClipRegion $ Rect (Point 0 0) (Point w h)
            bSetFillStyle fs
            bPaint
	render r rsize

-- | Helper function for using a renderable, when we generate it
--   in the CRender monad.
embedRenderable :: (ChartBackend m) => m (Renderable m a) -> Renderable m a
embedRenderable ca = Renderable {
   minsize = do { a <- ca; minsize a },
   render  = \ r -> do { a <- ca; render a r }
}


----------------------------------------------------------------------
-- Labels

-- | Construct a renderable from a text string, aligned with the axes.
label :: (ChartBackend m) => FontStyle -> HTextAnchor -> VTextAnchor -> String
         -> Renderable m String
label fs hta vta = rlabel fs hta vta 0

-- | Construct a renderable from a text string, rotated wrt to axes. The angle
--   of rotation is in degrees.
rlabel :: (ChartBackend m) => FontStyle -> HTextAnchor -> VTextAnchor -> Double -> String
          -> Renderable m String
rlabel fs hta vta rot s = Renderable { minsize = mf, render = rf }
  where
    mf = bLocal $ do
       bSetFontStyle fs
       (w,h) <- bTextSize s
       return (w*acr+h*asr,w*asr+h*acr)
    rf (w0,h0) = bLocal $ do
       bSetFontStyle fs
       sz@(w,h) <- bTextSize s
       extents <- bFontExtents
       let descent = fontExtentsDescent extents
       bTranslate $ Point 0 (-descent)
       bTranslate $ Point (xadj sz hta 0 w0) (yadj sz vta 0 h0)
       bRotate rot'
       bMoveTo $ Point (-w/2) (h/2)
       bShowText s
       return (\_-> Just s)  -- PickFn String
    xadj (w,h) HTA_Left   x1 x2 =  x1 +(w*acr+h*asr)/2
    xadj (w,h) HTA_Centre x1 x2 = (x1 + x2)/2
    xadj (w,h) HTA_Right  x1 x2 =  x2 -(w*acr+h*asr)/2
    yadj (w,h) VTA_Top    y1 y2 =  y1 +(w*asr+h*acr)/2
    yadj (w,h) VTA_Centre y1 y2 = (y1+y2)/2
    yadj (w,h) VTA_Bottom y1 y2 =  y2 - (w*asr+h*acr)/2

    rot'      = rot / 180 * pi
    (cr,sr)   = (cos rot', sin rot')
    (acr,asr) = (abs cr, abs sr)

----------------------------------------------------------------------
-- Rectangles

data RectCornerStyle = RCornerSquare
                     | RCornerBevel Double
                     | RCornerRounded Double

data Rectangle = Rectangle {
  rect_minsize_     :: RectSize,
  rect_fillStyle_   :: Maybe FillStyle,
  rect_lineStyle_   :: Maybe LineStyle,
  rect_cornerStyle_ :: RectCornerStyle
}

-- | Accessor for field rect_minsize_.
rect_minsize :: Accessor Rectangle RectSize
rect_minsize     = accessor (\v->rect_minsize_ v)
                            (\a v -> v{rect_minsize_=a})

-- | Accessor for field rect_fillStyle_.
rect_fillStyle :: Accessor Rectangle (Maybe FillStyle)
rect_fillStyle   = accessor (\v->rect_fillStyle_ v)
                            (\a v -> v{rect_fillStyle_=a})

-- | Accessor for field rect_lineStyle_.
rect_lineStyle :: Accessor Rectangle (Maybe LineStyle)
rect_lineStyle   = accessor (\v->rect_lineStyle_ v)
                            (\a v -> v{rect_lineStyle_=a})

-- | Accessor for field rect_cornerStyle_.
rect_cornerStyle :: Accessor Rectangle RectCornerStyle
rect_cornerStyle = accessor (\v->rect_cornerStyle_ v)
                            (\a v -> v{rect_cornerStyle_=a})


defaultRectangle :: Rectangle
defaultRectangle = Rectangle {
  rect_minsize_     = (0,0),
  rect_fillStyle_   = Nothing,
  rect_lineStyle_   = Nothing,
  rect_cornerStyle_ = RCornerSquare
}

rectangleToRenderable :: (ChartBackend m) => Rectangle -> Renderable m ()
rectangleToRenderable rectangle = Renderable mf rf
  where
    mf    = return (rect_minsize_ rectangle)
    rf sz = bLocal $ do
      maybeM () (fill sz) (rect_fillStyle_ rectangle)
      maybeM () (stroke sz) (rect_lineStyle_ rectangle)
      return nullPickFn

    fill sz fs = do
        bSetFillStyle fs
        strokeRectangle sz (rect_cornerStyle_ rectangle)
        bFill

    stroke sz ls = do
        bSetLineStyle ls
        strokeRectangle sz (rect_cornerStyle_ rectangle)
        bStroke

    strokeRectangle (x2,y2) RCornerSquare = do
        let (x1,y1) = (0,0)
        bMoveTo $ Point x1 y1
        bLineTo $ Point x1 y2
        bLineTo $ Point x2 y2
        bLineTo $ Point x2 y1
        bLineTo $ Point x1 y1
        bLineTo $ Point x1 y2
                                
    strokeRectangle (x2,y2) (RCornerBevel s) = do
        let (x1,y1) = (0,0)
        bMoveTo $ Point x1 (y1+s)
        bLineTo $ Point x1 (y2-s)
        bLineTo $ Point (x1+s) y2
        bLineTo $ Point (x2-s) y2
        bLineTo $ Point x2 (y2-s)
        bLineTo $ Point x2 (y1+s)
        bLineTo $ Point (x2-s) y1
        bLineTo $ Point (x1+s) y1
        bLineTo $ Point x1 (y1+s)
        bLineTo $ Point x1 (y2-s)

    strokeRectangle (x2,y2) (RCornerRounded s) = do
        let (x1,y1) = (0,0)
        bArcNegative (Point (x1+s) (y2-s)) s (pi2*2) pi2 
        bArcNegative (Point (x2-s) (y2-s)) s pi2 0
        bArcNegative (Point (x2-s) (y1+s)) s 0 (pi2*3)
        bArcNegative (Point (x1+s) (y1+s)) s (pi2*3) (pi2*2)
        bLineTo $ Point x1 (y2-s)
    
    pi2 = pi / 2

{- TODO: See class definition
instance ToRenderable Rectangle where
  type RenderableT m Rectangle = Rectangle
  toRenderable rectangle = rectangleToRenderable
-}










