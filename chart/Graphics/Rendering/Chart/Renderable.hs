-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Renderable
-- Copyright   :  (c) Tim Docker 2006, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- This module contains the definition of the 'Renderable' type, which
-- is a composable drawing element, along with assorted functions to
-- them.
--
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Renderable(
    Renderable(..),
    ToRenderable(..),
    PickFn,
    Rectangle(..),
    RectCornerStyle(..),
    
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
import Control.Lens
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
--   of rotation is in degrees, measured clockwise from the horizontal.
rlabel :: FontStyle -> HTextAnchor -> VTextAnchor -> Double -> String -> Renderable String
rlabel fs hta vta rot s = Renderable { minsize = mf, render = rf }
  where
    mf = withFontStyle fs $ do
       ts <- textSize s
       let sz = (textSizeWidth ts, textSizeHeight ts)
       return (xwid sz, ywid sz)
       
    rf (w0,h0) = withFontStyle fs $ do
      ts <- textSize s
      let sz@(w,h) = (textSizeWidth ts, textSizeHeight ts)
          descent = textSizeDescent ts
          
          xadj HTA_Left   = xwid sz/2
          xadj HTA_Centre = w0/2
          xadj HTA_Right  = w0 - xwid sz/2
    
          yadj VTA_Top      = ywid sz/2
          yadj VTA_Centre   = h0/2
          yadj VTA_Bottom   = h0 - ywid sz/2
          yadj VTA_BaseLine = h0 - ywid sz/2 + descent*acr

      withTranslation (Point 0 (-descent)) $ 
        withTranslation (Point (xadj hta) (yadj vta)) $ 
          withRotation rot' $ do
            drawText (Point (-w/2) (h/2)) s
            return (\_-> Just s)  -- PickFn String
            
    rot'      = rot / 180 * pi
    (cr,sr)   = (cos rot', sin rot')
    (acr,asr) = (abs cr, abs sr)

    xwid (w,h) = w*acr + h*asr
    ywid (w,h) = w*asr + h*acr

----------------------------------------------------------------------
-- Rectangles

data RectCornerStyle = RCornerSquare
                     | RCornerBevel Double
                     | RCornerRounded Double

data Rectangle = Rectangle {
  _rect_minsize     :: RectSize,
  _rect_fillStyle   :: Maybe FillStyle,
  _rect_lineStyle   :: Maybe LineStyle,
  _rect_cornerStyle :: RectCornerStyle
}

instance Default Rectangle where
  def = Rectangle
    { _rect_minsize     = (0,0)
    , _rect_fillStyle   = Nothing
    , _rect_lineStyle   = Nothing
    , _rect_cornerStyle = RCornerSquare
    }

instance ToRenderable Rectangle where
  toRenderable = rectangleToRenderable

rectangleToRenderable :: Rectangle -> Renderable a
rectangleToRenderable rectangle = Renderable mf rf
  where
    mf    = return (_rect_minsize rectangle)
    rf sz = do
      maybeM () (fill sz) (_rect_fillStyle rectangle)
      maybeM () (stroke sz) (_rect_lineStyle rectangle)
      return nullPickFn

    fill sz fs = do
        withFillStyle fs $ do
          fillPath $ strokeRectangleP sz (_rect_cornerStyle rectangle)

    stroke sz ls = do
        withLineStyle ls $ do
          strokePath $ strokeRectangleP sz (_rect_cornerStyle rectangle)

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

$( makeLenses ''Rectangle )
