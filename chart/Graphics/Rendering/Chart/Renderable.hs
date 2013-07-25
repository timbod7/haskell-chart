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

module Graphics.Rendering.Chart.Renderable(
    Renderable(..),
    ToRenderable(..),
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
import Data.Monoid
import Data.Default.Class

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
data Renderable a = Renderable {

   -- | A Cairo action to calculate a minimum size.
   minsize :: ChartBackend RectSize,

   -- | A Cairo action for drawing it within a rectangle.
   --   The rectangle is from the origin to the given point.
   --
   --   The resulting "pick" function  maps a point in the image to a value.
   render  :: RectSize -> ChartBackend (PickFn a)
}

-- | A type class abtracting the conversion of a value to a Renderable.
class ToRenderable a where
  toRenderable :: a -> Renderable ()

emptyRenderable :: Renderable a
emptyRenderable = spacer (0,0)

-- | Create a blank renderable with a specified minimum size.
spacer :: RectSize -> Renderable a 
spacer sz  = Renderable {
   minsize = return sz,
   render  = \_ -> return nullPickFn
}


-- | Create a blank renderable with a minimum size the same as
--   some other renderable.
spacer1 :: Renderable a -> Renderable b
spacer1 r  = r{ render  = \_ -> return nullPickFn }

-- | Replace the pick function of a renderable with another.
setPickFn :: PickFn b -> Renderable a -> Renderable b
setPickFn pickfn r = r{ render  = \sz -> do { render r sz; return pickfn; } }

-- | Map a function over the result of a renderable's pickfunction, keeping only 'Just' results.
mapMaybePickFn :: (a -> Maybe b) -> Renderable a -> Renderable b
mapMaybePickFn f r = r{ render = \sz -> do pf <- render r sz
                                           return (join . fmap f . pf) }

-- | Map a function over result of a renderable's pickfunction.
mapPickFn :: (a -> b) -> Renderable a -> Renderable b
mapPickFn f = mapMaybePickFn (Just . f)

-- | Add some spacing at the edges of a renderable.
addMargins :: (Double,Double,Double,Double) -- ^ The spacing to be added.
           -> Renderable a                  -- ^ The source renderable.
           -> Renderable a
addMargins (t,b,l,r) rd = Renderable { minsize = mf, render = rf }
  where
    mf = do
        (w,h) <- minsize rd
        return (w+l+r,h+t+b)

    rf (w,h) = do
        withTranslation (Point l t) $ do
          pickf <- render rd (w-l-r,h-t-b)
          return (mkpickf pickf (t,b,l,r) (w,h))

    mkpickf pickf (t,b,l,r) (w,h) (Point x y)
        | x >= l && x <= w-r && y >= t && t <= h-b = pickf (Point (x-l) (y-t))
        | otherwise                                = Nothing

-- | Overlay a renderable over a solid background fill.
fillBackground :: FillStyle -> Renderable a -> Renderable a
fillBackground fs r = r{ render = rf }
  where
    rf rsize@(w,h) = do
      withFillStyle fs $ do
        p <- alignFillPath $ rectPath (Rect (Point 0 0) (Point w h))
        fillPath p
      render r rsize

-- | Helper function for using a renderable, when we generate it
--   in the CRender monad.
embedRenderable :: ChartBackend (Renderable a) -> Renderable a
embedRenderable ca = Renderable {
   minsize = do { a <- ca; minsize a },
   render  = \ r -> do { a <- ca; render a r }
}


----------------------------------------------------------------------
-- Labels

-- | Construct a renderable from a text string, aligned with the axes.
label :: FontStyle -> HTextAnchor -> VTextAnchor -> String -> Renderable String
label fs hta vta = rlabel fs hta vta 0

-- | Construct a renderable from a text string, rotated wrt to axes. The angle
--   of rotation is in degrees.
rlabel :: FontStyle -> HTextAnchor -> VTextAnchor -> Double -> String -> Renderable String
rlabel fs hta vta rot s = Renderable { minsize = mf, render = rf }
  where
    mf = withFontStyle fs $ do
       ts <- textSize s
       let (w,h) = (textSizeWidth ts, textSizeHeight ts)
       return (w*acr+h*asr,w*asr+h*acr)
    rf (w0,h0) = withFontStyle fs $ do
      ts <- textSize s
      let sz@(w,h) = (textSizeWidth ts, textSizeHeight ts)
      let descent = textSizeDescent ts
      withTranslation (Point 0 (-descent)) $ do
        withTranslation (Point (xadj sz hta 0 w0) (yadj sz vta 0 h0)) $ do
          withRotation rot' $ do
            drawText (Point (-w/2) (h/2)) s
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

{-# DEPRECATED defaultRectangle "Use the according Data.Default instance!" #-}
defaultRectangle :: Rectangle
defaultRectangle = def

instance Default Rectangle where
  def = Rectangle
    { rect_minsize_     = (0,0)
    , rect_fillStyle_   = Nothing
    , rect_lineStyle_   = Nothing
    , rect_cornerStyle_ = RCornerSquare
    }

instance ToRenderable Rectangle where
  toRenderable = rectangleToRenderable

rectangleToRenderable :: Rectangle -> Renderable a
rectangleToRenderable rectangle = Renderable mf rf
  where
    mf    = return (rect_minsize_ rectangle)
    rf sz = do
      maybeM () (fill sz) (rect_fillStyle_ rectangle)
      maybeM () (stroke sz) (rect_lineStyle_ rectangle)
      return nullPickFn

    fill sz fs = do
        withFillStyle fs $ do
          fillPath $ strokeRectangleP sz (rect_cornerStyle_ rectangle)

    stroke sz ls = do
        withLineStyle ls $ do
          strokePath $ strokeRectangleP sz (rect_cornerStyle_ rectangle)

    strokeRectangleP (x2,y2) RCornerSquare =
      let (x1,y1) = (0,0) in moveTo' x1 y1
                          <> lineTo' x1 y2
                          <> lineTo' x2 y2
                          <> lineTo' x2 y1
                          <> lineTo' x1 y1
                          <> lineTo' x1 y2
                                
    strokeRectangleP (x2,y2) (RCornerBevel s) =
      let (x1,y1) = (0,0) in moveTo' x1 (y1+s)
                          <> lineTo' x1 (y2-s)
                          <> lineTo' (x1+s) y2
                          <> lineTo' (x2-s) y2
                          <> lineTo' x2 (y2-s)
                          <> lineTo' x2 (y1+s)
                          <> lineTo' (x2-s) y1
                          <> lineTo' (x1+s) y1
                          <> lineTo' x1 (y1+s)
                          <> lineTo' x1 (y2-s)

    strokeRectangleP (x2,y2) (RCornerRounded s) =
      let (x1,y1) = (0,0) in arcNeg (Point (x1+s) (y2-s)) s (pi2*2) pi2 
                          <> arcNeg (Point (x2-s) (y2-s)) s pi2 0
                          <> arcNeg (Point (x2-s) (y1+s)) s 0 (pi2*3)
                          <> arcNeg (Point (x1+s) (y1+s)) s (pi2*3) (pi2*2)
                          <> lineTo' x1 (y2-s)
    
    pi2 = pi / 2











