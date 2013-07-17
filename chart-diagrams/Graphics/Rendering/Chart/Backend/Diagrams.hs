
{-# LANGUAGE FlexibleContexts #-}

-- | The backend to render charts with the diagrams library.
module Graphics.Rendering.Chart.Backend.Diagrams
  ( runBackend
  ) where

import Data.Default
import Data.Colour
import Data.Colour.SRGB
import Data.List (unfoldr)
import Data.Monoid

import Control.Monad.Reader

import Diagrams.Core.Transform ( Transformation(..) )
import Diagrams.Prelude 
  ( Diagram
  , R2, P2, T2
  , r2, p2, unr2, unp2
  , Trail
  , (.+^), (<->)
  )
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD as D2
import qualified Diagrams.TwoD.Arc as D2

import Graphics.Rendering.Chart.Backend as G
import Graphics.Rendering.Chart.Backend.Impl
import Graphics.Rendering.Chart.Geometry as G
import Graphics.Rendering.Chart.Renderable

-- -----------------------------------------------------------------------
-- Backend
-- -----------------------------------------------------------------------

-- | Run this backends renderer.
runBackend :: (D.Renderable (D.Path R2) b)
           => ChartBackendEnv   -- ^ Environment to start rendering with.
           -> ChartBackend a    -- ^ Chart render code.
           -> (Diagram b R2, a) -- ^ The diagram.
runBackend env m = compileBackend
      strokePathD fillPathD fillClipD textSizeD drawTextD 
      withTransformD withLineStyleD withFillStyleD withFontStyleD withClipRegionD
      env m


{-
data Path = MoveTo Point Path 
          | LineTo Point Path
          | Arc Point Double Double Double Path
          | ArcNeg Point Double Double Double Path
          | End 
          | Close
 -}

{-

 foldPath :: (Monoid m)
         => (Point -> m) -- ^ MoveTo
         -> (Point -> m) -- ^ LineTo
         -> (Point -> Double -> Double -> Double -> m) -- ^ Arc
         -> (Point -> Double -> Double -> Double -> m) -- ^ ArcNeg
         -> m    -- ^ Close
         -> Path -- ^ Path to fold
         -> m
 
 -}

strokePathD :: (D.Renderable (D.Path R2) b)
            => ChartBackendEnv -> Path -> Diagram b R2
strokePathD env p = applyLineStyle (cbeLineStyle env) $ D.stroke $ convertPath p

fillPathD :: (D.Renderable (D.Path R2) b)
          => ChartBackendEnv -> Path -> Diagram b R2
fillPathD env p = applyFillStyle (cbeFillStyle env) $ D.stroke $ convertPath p

fillClipD :: (D.Renderable (D.Path R2) b)
          => ChartBackendEnv -> Diagram b R2
fillClipD env = mempty -- TODO

textSizeD :: (D.Renderable (D.Path R2) b)
          => ChartBackendEnv -> String -> (Diagram b R2, TextSize)
textSizeD env text = (mempty, TextSize 10 10 10 10 10) -- TODO

drawTextD :: (D.Renderable (D.Path R2) b)
          => ChartBackendEnv -> Point -> String -> Diagram b R2
drawTextD env p text = mempty -- TODO

withTransformD :: (D.Renderable (D.Path R2) b)
               => ChartBackendEnv -> Change Matrix -> Diagram b R2 -> Diagram b R2
withTransformD env c = D.transform (toTransformation $ diffValue c)

withLineStyleD :: (D.Renderable (D.Path R2) b)
               => ChartBackendEnv -> Change LineStyle -> Diagram b R2 -> Diagram b R2
withLineStyleD env c = applyLineStyle (cbeLineStyle env)

withFillStyleD :: (D.Renderable (D.Path R2) b)
               => ChartBackendEnv -> Change FillStyle -> Diagram b R2 -> Diagram b R2
withFillStyleD env c = applyFillStyle (cbeFillStyle env)

withFontStyleD :: (D.Renderable (D.Path R2) b)
               => ChartBackendEnv -> Change FontStyle -> Diagram b R2 -> Diagram b R2
withFontStyleD env c = id -- TODO

withClipRegionD :: (D.Renderable (D.Path R2) b)
                => ChartBackendEnv -> Change (Limit Rect) -> Diagram b R2 -> Diagram b R2
withClipRegionD env c = case diffValue c of
  LValue clip -> D2.clipBy (convertPath $ rectPath clip)
  LMax -> error "Infinite plane clipping should never happen!"
  LMin -> D2.clipBy mempty

-- -----------------------------------------------------------------------
-- Converions Helpers
-- -----------------------------------------------------------------------

toTransformation :: Matrix -> T2
toTransformation m = Transformation 
  (applyWithoutTrans m <-> applyWithoutTrans (invert m))
  (applyWithoutTrans (transpose m) <-> applyWithoutTrans (transpose (invert m)))
  (r2 (x0 m, y0 m))

transpose :: Matrix -> Matrix
transpose (Matrix xx yx xy yy _ _) = Matrix xx xy yx yy 0 0

-- | Apply a given affine transformation to a vector.
applyTransformation :: Matrix -> P2 -> P2
applyTransformation m p =
  let (x,y) = D2.unp2 p
  in p2 ( xx m * x + xy m * y + x0 m
        , yx m * x + yy m * y + y0 m
        )

-- | Apply a given affine transformation to a vector.
applyWithoutTrans :: Matrix -> R2 -> R2
applyWithoutTrans m v =
  let (x,y) = D2.unr2 v
  in r2 ( xx m * x + xy m * y
        , yx m * x + yy m * y
        )

-- | Apply the Chart line style to a diagram.
applyLineStyle :: (D.HasStyle a) => LineStyle -> a -> a
applyLineStyle ls = D.lineWidth (line_width_ ls) 
                  . D.lineColor (line_color_ ls) 
                  . D.lineCap (convertLineCap $ line_cap_ ls) 
                  . D.lineJoin (convertLineJoin $ line_join_ ls) 
                  . D.dashing (line_dashes_ ls) 0

-- | Apply the Chart fill style to a diagram.
applyFillStyle :: (D.HasStyle a) => FillStyle -> a -> a
applyFillStyle fs = case fs of
  FillStyleSolid cl -> D.fillColor cl

-- | Convert line caps.
convertLineCap :: LineCap -> D.LineCap
convertLineCap cap = case cap of
  LineCapButt   -> D.LineCapButt
  LineCapRound  -> D.LineCapRound
  LineCapSquare -> D.LineCapSquare

-- | Convert line joins.
convertLineJoin :: LineJoin -> D.LineJoin
convertLineJoin join = case join of
  LineJoinMiter -> D.LineJoinMiter
  LineJoinRound -> D.LineJoinRound
  LineJoinBevel -> D.LineJoinBevel

-- | Convert paths.
convertPath :: Path -> D.Path R2
convertPath p = convertPath' (p2 (0,0)) p
  where
    convertPath' :: D.Point R2 -> Path -> D.Path R2
    convertPath' offset p = 
      let (start, t, restP) = pathToTrail offset p
      in D.pathFromTrailAt t start <> case restP of
        Nothing -> mempty
        Just rest -> convertPath' (start .+^ D.trailOffset t) rest

pathToTrail :: D.Point R2 -> Path 
            -> (D.Point R2, Trail R2, Maybe Path)
pathToTrail start (MoveTo (Point x y) p) = 
  let (t, rest) = pathToTrail' p (p2 (x,y))
  in (p2 (x,y), t, rest)
pathToTrail start p = 
  let (t, rest) = pathToTrail' p start
  in (start, t, rest)

pathToTrail' :: Path -> P2 -> (Trail R2, Maybe Path)
pathToTrail' p@(MoveTo _ _) _ = (mempty, Just p)
pathToTrail' (LineTo (Point x y) p) offset = 
  let (t, rest) = pathToTrail' p $ p2 (x,y)
  in (D.fromSegments [D.straight $ (x,y) `adjustBy` offset] <> t, rest)
pathToTrail' (Arc (Point x y) r as ae p) offset = 
  let (t, rest) = pathToTrail' p $ p2 (x,y)
  in ( ((D2.translate ((x,y) `adjustBy` offset)) $ D2.scale r $ D2.arc (D2.Rad as) (D2.Rad ae)) <> t
     , rest )
pathToTrail' (ArcNeg (Point x y) r as ae p) offset = 
  let (t, rest) = pathToTrail' p $ p2 (x,y)
  in ( ((D2.translate ((x,y) `adjustBy` offset)) $ D2.scale r $ D2.arcCW (D2.Rad as) (D2.Rad ae)) <> t
     , rest )
pathToTrail' End _ = (mempty, Nothing)
pathToTrail' Close _ = (D.close mempty, Nothing)

adjustBy :: (Double, Double) -> P2 -> R2
adjustBy (x,y) p = 
  let (x0, y0) = unp2 p
  in r2 (x - x0, y - y0)

