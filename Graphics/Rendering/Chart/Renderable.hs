-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Renderable
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Renderable where

import qualified Graphics.Rendering.Cairo as C
import Control.Monad
import Data.List ( nub, transpose, sort )

import Graphics.Rendering.Chart.Types

type PickFn a = Point -> a

-- | A Renderable is a record of functions required to layout a
-- graphic element.
data Renderable a = Renderable {

   -- | a Cairo action to calculate a minimum size,
   minsize :: CRender RectSize,

   -- | a Cairo action for drawing it within a rectangle.
   -- The rectangle is from the origin to the given point.
   --
   -- The resulting "pick" function  maps a point in the image to
   -- a value.
   render ::  RectSize -> CRender (PickFn a)
}

-- | A type class abtracting the conversion of a value to a
-- Renderable.

class ToRenderable a where
   toRenderable :: a -> Renderable ()

emptyRenderable = spacer (0,0)

spacer sz = Renderable {
   minsize = return sz,
   render  = \_ -> return (const ())
}

-- | Replace the pick function of a renderable with another
setPickFn :: PickFn b -> Renderable a -> Renderable b
setPickFn pickfn r = Renderable {
    minsize=minsize r,
    render = \sz -> do { render r sz; return pickfn; }
    }

addMargins :: (Double,Double,Double,Double) -> a -> Renderable a -> Renderable a
addMargins (t,b,l,r) a rd = Renderable { minsize = mf, render = rf }
  where
    mf = do
        (w,h) <- minsize rd
        return (w+l+r,h+t+b)

    rf (w,h) = do
        preserveCState $ do
            c $ C.translate l t
            pickf <- render rd (w-l-r,h-t-b)
            mtx <- c $ C.getMatrix
            return (mkpickf pickf (w,h) mtx)

    mkpickf pickf (w,h) mtx pt | within pt' rect = pickf pt'
                               | otherwise = a
      where
        pt' = transform mtx pt
        rect = (Rect (Point 0 0) (Point w h))

transform :: C.Matrix -> Point -> Point
transform = undefined

fillBackground :: CairoFillStyle -> Renderable a -> Renderable a
fillBackground fs r = Renderable { minsize = minsize r, render = rf }
  where
    rf rsize@(w,h) = do
        preserveCState $ do
            setClipRegion (Point 0 0) (Point w h)
            setFillStyle fs
            c $ C.paint
	render r rsize

-- | Output the given renderable to a PNG file of the specifed size
-- (in pixels), to the specified file.
renderableToPNGFile :: Renderable a -> Int -> Int -> FilePath -> IO ()
renderableToPNGFile chart width height path = 
    C.withImageSurface C.FormatARGB32 width height $ \result -> do
    C.renderWith result $ runCRender rfn bitmapEnv
    C.surfaceWriteToPNG result path
  where
    rfn = do
	render chart (fromIntegral width, fromIntegral height)

renderableToFile withSurface chart width height path = 
    withSurface path (fromIntegral width) (fromIntegral height) $ \result -> do
    C.renderWith result $ runCRender rfn vectorEnv
    C.surfaceFinish result
  where
    rfn = do
        render chart (fromIntegral width, fromIntegral height)
        c $ C.showPage

-- | Output the given renderable to a PDF file of the specifed size
-- (in points), to the specified file.
renderableToPDFFile :: Renderable a -> Int -> Int -> FilePath -> IO ()
renderableToPDFFile = renderableToFile C.withPDFSurface

-- | Output the given renderable to a postscript file of the specifed size
-- (in points), to the specified file.
renderableToPSFile :: Renderable a -> Int -> Int -> FilePath -> IO ()
renderableToPSFile = renderableToFile C.withPSSurface

-- | Output the given renderable to an SVG file of the specifed size
-- (in points), to the specified file.
renderableToSVGFile :: Renderable a -> Int -> Int -> FilePath -> IO ()
renderableToSVGFile = renderableToFile C.withSVGSurface

bitmapEnv = CEnv adjfn
  where
    adjfn (Point x y)= Point (adj x) (adj y)
    adj x = (fromIntegral (round (x-0.5)))+0.5

vectorEnv = CEnv id

embedRenderable :: CRender (Renderable a) -> Renderable a
embedRenderable ca = Renderable {
   minsize = do { a <- ca; minsize a },
   render = \ r -> do { a <- ca; render a r }
}


----------------------------------------------------------------------
-- Labels

label :: CairoFontStyle -> HTextAnchor -> VTextAnchor -> String -> Renderable ()
label fs hta vta = rlabel fs hta vta 0

rlabel :: CairoFontStyle -> HTextAnchor -> VTextAnchor -> Double -> String -> Renderable ()
rlabel fs hta vta rot s = Renderable { minsize = mf, render = rf }
  where
    mf = preserveCState $ do
       setFontStyle fs
       (w,h) <- textSize s
       return (w*acr+h*asr,w*asr+h*acr)
    rf (w0,h0) = preserveCState $ do
       setFontStyle fs
       sz@(w,h) <- textSize s
       fe <- c $ C.fontExtents
       c $ C.translate 0 (-C.fontExtentsDescent fe)
       c $ C.translate (xadj sz hta 0 w0) (yadj sz vta 0 h0)
       c $ C.rotate rot'
       c $ C.moveTo (-w/2) (h/2)
       c $ C.showText s
       return (const ())
    xadj (w,h) HTA_Left x1 x2 =  x1 +(w*acr+h*asr)/2
    xadj (w,h) HTA_Centre x1 x2 = (x1 + x2)/2
    xadj (w,h) HTA_Right x1 x2 =  x2 -(w*acr+h*asr)/2
    yadj (w,h) VTA_Top y1 y2 =  y1 +(w*asr+h*acr)/2
    yadj (w,h) VTA_Centre y1 y2 = (y1+y2)/2
    yadj (w,h) VTA_Bottom y1 y2 =  y2 - (w*asr+h*acr)/2

    rot' = rot / 180 * pi
    (cr,sr) = (cos rot', sin rot')
    (acr,asr) = (abs cr, abs sr)

----------------------------------------------------------------------
-- Rectangles

data RectCornerStyle = RCornerSquare
                     | RCornerBevel Double
                     | RCornerRounded Double

data Rectangle = Rectangle {
  rect_minsize :: RectSize,
  rect_fillStyle :: Maybe CairoFillStyle,
  rect_lineStyle :: Maybe CairoLineStyle,
  rect_cornerStyle :: RectCornerStyle
}

defaultRectangle = Rectangle {
  rect_minsize = (0,0),
  rect_fillStyle = Nothing,
  rect_lineStyle = Nothing,
  rect_cornerStyle = RCornerSquare
}

instance ToRenderable Rectangle where
   toRenderable rectangle = Renderable mf rf
     where
      mf = return (rect_minsize rectangle)
      rf sz = preserveCState $ do
        maybeM () (fill sz) (rect_fillStyle rectangle)
        maybeM () (stroke sz) (rect_lineStyle rectangle)
        return (const ())

      fill sz fs = do
          setFillStyle fs
          strokeRectangle sz (rect_cornerStyle rectangle)
          c $ C.fill

      stroke sz ls = do
          setLineStyle ls
          strokeRectangle sz (rect_cornerStyle rectangle)
          c $ C.stroke

      strokeRectangle (x2,y2) RCornerSquare = c $ do
          let (x1,y1) = (0,0)
          C.moveTo x1 y1
          C.lineTo x1 y2
          C.lineTo x2 y2
          C.lineTo x2 y1
          C.lineTo x1 y1
          C.lineTo x1 y2
                                  
      strokeRectangle (x2,y2) (RCornerBevel s) = c $ do
          let (x1,y1) = (0,0)
          C.moveTo x1 (y1+s)
          C.lineTo x1 (y2-s)
          C.lineTo (x1+s) y2
          C.lineTo (x2-s) y2
          C.lineTo x2 (y2-s)
          C.lineTo x2 (y1+s)
          C.lineTo (x2-s) y1
          C.lineTo (x1+s) y1
          C.lineTo x1 (y1+s)
          C.lineTo x1 (y2-s)

      strokeRectangle (x2,y2) (RCornerRounded s) = c $ do
          let (x1,y1) = (0,0)
          C.arcNegative (x1+s) (y2-s) s (pi2*2) pi2 
          C.arcNegative (x2-s) (y2-s) s pi2 0
          C.arcNegative (x2-s) (y1+s) s 0 (pi2*3)
          C.arcNegative (x1+s) (y1+s) s (pi2*3) (pi2*2)
          C.lineTo x1 (y2-s)

      pi2 = pi / 2

maybeM v = maybe (return v)
