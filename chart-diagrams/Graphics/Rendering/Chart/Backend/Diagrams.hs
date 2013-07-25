
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- | The backend to render charts with the diagrams library.
module Graphics.Rendering.Chart.Backend.Diagrams
  ( runBackend
  , defaultEnv
  , DEnv(..)
  ) where

import Data.Default.Class
import Data.Colour
import Data.Colour.SRGB
import Data.List (unfoldr)
import Data.Monoid

import Control.Monad.Operational

import Diagrams.Core.Transform ( Transformation(..) )
import Diagrams.Prelude 
  ( Diagram
  , R2, P2, T2
  , r2, p2, unr2, unp2
  , Trail(..), Segment
  , Rad(..), CircleFrac(..)
  , (.+^), (<->), (~~)
  )
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD as D2
import qualified Diagrams.TwoD.Arc as D2

import qualified Graphics.SVGFonts.ReadFont as F

import Graphics.Rendering.Chart.Backend as G
import Graphics.Rendering.Chart.Backend.Impl
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Geometry as G
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Renderable

-- -----------------------------------------------------------------------
-- Backend
-- -----------------------------------------------------------------------

data DEnv = DEnv
  { envAlignmentFns :: AlignmentFns
  , envFontStyle :: FontStyle
  }

-- | Produce a environment with no transformation and clipping. 
--   It will use the default styles.
defaultEnv :: AlignmentFns
           -> DEnv
defaultEnv alignFns = DEnv 
  { envAlignmentFns = alignFns
  , envFontStyle = def
  }

-- | Run this backends renderer.
runBackend :: (D.Renderable (D.Path R2) b)
           => DEnv   -- ^ Environment to start rendering with.
           -> ChartBackend a    -- ^ Chart render code.
           -> (Diagram b R2, a) -- ^ The diagram.
runBackend env m =  eval env (view m)
  where
    eval :: (D.Renderable (D.Path R2) b)
         => DEnv -> ProgramView ChartBackendInstr a -> (Diagram b R2, a)
    eval env (Return v) = (mempty, v)
    eval env (StrokePath p :>>= f) = dStrokePath env p   <># step env f
    eval env (FillPath p   :>>= f) = dFillPath   env p   <># step env f
    eval env (DrawText p s :>>= f) = dDrawText   env p s <># step env f
    eval env (GetTextSize s :>>= f) = dTextSize env s   <>= step env f
    eval env (GetAlignments :>>= f) = dAlignmentFns env <>= step env f
    eval env (WithTransform m p :>>= f)  = dWithTransform env m  p <>= step env f
    eval env (WithFontStyle fs p :>>= f) = dWithFontStyle env fs p <>= step env f
    eval env (WithFillStyle fs p :>>= f) = dWithFillStyle env fs p <>= step env f
    eval env (WithLineStyle ls p :>>= f) = dWithLineStyle env ls p <>= step env f
    eval env (WithClipRegion r p :>>= f) = dWithClipRegion env r p <>= step env f

    step :: (D.Renderable (D.Path R2) b)
         => DEnv -> (v -> ChartBackend a) -> v -> (Diagram b R2, a)
    step env f =  \v -> runBackend env (f v)
    
    (<>#) :: (Monoid m) => m -> (() -> (m, a)) -> (m, a)
    (<>#) m f = (m, ()) <>= f
    
    (<>=) :: (Monoid m) => (m, a) -> (a -> (m, b)) -> (m, b)
    (<>=) (ma, a) f = let (mb, b) = f a
                      in (mb <> ma, b)

dStrokePath :: (D.Renderable (D.Path R2) b)
            => DEnv -> Path -> Diagram b R2
dStrokePath env p = applyFillStyle noFillStyle $ D.stroke $ convertPath p

dFillPath :: (D.Renderable (D.Path R2) b)
          => DEnv -> Path -> Diagram b R2
dFillPath env p = applyLineStyle noLineStyle $ D.stroke $ convertPath p

dTextSize :: (D.Renderable (D.Path R2) b)
          => DEnv -> String -> (Diagram b R2, TextSize)
dTextSize env text = 
  let fs = envFontStyle env
      font@(fontData,_) = fontFromName $ font_name_ $ fs
      (_,_,_,_,_,(_,_,weight,_,_,panose,ascent,descent,xHeight,capHeight,stemh,stemv,_)) = fontData
  in (mempty, TextSize { textSizeWidth = D2.width $ F.textSVG' (fontStyleToTextOpts fs text)
                       , textSizeAscent = 10 -- ascent
                       , textSizeDescent = 10 -- descent
                       , textSizeYBearing = F.bbox_dy fontData / 2 -- TODO: Is this really what we want?
                       , textSizeHeight = font_size_ $ fs -- TODO: Should we get this from the font itself?
                       })

dAlignmentFns :: (D.Renderable (D.Path R2) b)
              => DEnv -> (Diagram b R2, AlignmentFns)
dAlignmentFns env = (mempty, envAlignmentFns env) -- TODO

dDrawText :: (D.Renderable (D.Path R2) b)
          => DEnv -> Point -> String -> Diagram b R2
dDrawText env (Point x y) text 
  = D.transform (toTransformation $ translate (Vector x y) 1)
  $ applyFontStyle (envFontStyle env)
  $ D2.scaleY (-1)
  $ F.textSVG_ (fontStyleToTextOpts (envFontStyle env) text)

dWith :: (D.Renderable (D.Path R2) b)
      => DEnv -> (DEnv -> DEnv) -> (Diagram b R2 -> Diagram b R2) 
      -> ChartBackend a -> (Diagram b R2, a)
dWith env envF dF m = let (ma, a) = runBackend (envF env) m
                      in (dF ma, a)

dWithTransform :: (D.Renderable (D.Path R2) b)
               => DEnv -> Matrix -> ChartBackend a -> (Diagram b R2, a)
dWithTransform env t = dWith env id $ D.transform (toTransformation t)

dWithLineStyle :: (D.Renderable (D.Path R2) b)
               => DEnv -> LineStyle -> ChartBackend a -> (Diagram b R2, a)
dWithLineStyle env ls = dWith env id $ applyLineStyle ls

dWithFillStyle :: (D.Renderable (D.Path R2) b)
               => DEnv -> FillStyle -> ChartBackend a -> (Diagram b R2, a)
dWithFillStyle env fs = dWith env id $ applyFillStyle fs

dWithFontStyle :: (D.Renderable (D.Path R2) b)
               => DEnv -> FontStyle -> ChartBackend a -> (Diagram b R2, a)
dWithFontStyle env fs = dWith env (\e -> e { envFontStyle = fs }) $ id -- TODO

dWithClipRegion :: (D.Renderable (D.Path R2) b)
                => DEnv -> Rect -> ChartBackend a -> (Diagram b R2, a)
dWithClipRegion env clip = dWith env id $ D2.clipBy (convertPath $ rectPath clip)

-- -----------------------------------------------------------------------
-- Converions Helpers
-- -----------------------------------------------------------------------

pointToP2 :: Point -> P2
pointToP2 (Point x y) = p2 (x,y)

noLineStyle :: LineStyle
noLineStyle = def 
  { line_width_ = 0
  , line_color_ = transparent
  }

noFillStyle :: FillStyle
noFillStyle = solidFillStyle transparent

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

-- | Apply all pure diagrams properties from the font style.
applyFontStyle :: (D.HasStyle a) => FontStyle -> a -> a
applyFontStyle fs = applyLineStyle noLineStyle 
                  . applyFillStyle (solidFillStyle $ font_color_ fs)

-- TODO: FontSlant and FontWeight can not be expressed properly.
fontStyleToTextOpts :: FontStyle -> String -> F.TextOpts
fontStyleToTextOpts fs text = F.TextOpts
  { F.txt = text
  , F.fdo = fontFromName $ font_name_ fs
  , F.mode = F.INSIDE_H
  , F.spacing = F.KERN
  , F.underline = False
  , F.textWidth = 1
  , F.textHeight = font_size_ fs
  }

fontFromName :: String -> (F.FontData, F.OutlineMap)
fontFromName name = case name of
  "serif" -> F.lin
  "monospace" -> F.bit
  _ -> F.lin

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
convertPath path = 
  let (start, t, restM) = pathToTrail (Point 0 0) $ makeLinesExplicit path
  in D.pathFromTrailAt t start <> case restM of
    Nothing -> mempty
    Just rest -> convertPath rest

pathToTrail :: Point -> Path 
            -> (D.Point R2, Trail R2, Maybe Path)
pathToTrail _ (MoveTo p0 path) = 
  let (t, close, rest) = pathToTrail' path p0
  in (pointToP2 p0, makeTrail close t, rest)
pathToTrail _ path@(Arc c r s _ _) = 
  let p0 = translateP (pointToVec c) $ rotateP s $ Point r 0
      (t, close, rest) = pathToTrail' path p0
  in (pointToP2 p0, makeTrail close t, rest)
pathToTrail _ path@(ArcNeg c r s _ _) = 
  let p0 = translateP (pointToVec c) $ rotateP s $ Point r 0
      (t, close, rest) = pathToTrail' path p0
  in (pointToP2 p0, makeTrail close t, rest)
pathToTrail start path = 
  let (t, close, rest) = pathToTrail' path start
  in (pointToP2 start, makeTrail close t, rest)

makeTrail :: Bool -> D.Trail' D.Line R2 -> Trail R2
makeTrail True  t = D.wrapTrail $ D.closeLine t
makeTrail False t = D.wrapTrail $ t

pathToTrail' :: Path -> Point -> (D.Trail' D.Line R2, Bool, Maybe Path)
pathToTrail' p@(MoveTo _ _) _ = (mempty, False, Just p)
pathToTrail' (LineTo p1 path) p0 = 
  let (t, c, rest) = pathToTrail' path p1
  in ( (pointToP2 p0 ~~ pointToP2 p1) <> t, c, rest )
pathToTrail' (Arc p0 r s e path) _ = 
  let endP = translateP (pointToVec p0) $ rotateP e $ Point r 0
      (t, c, rest) = pathToTrail' path endP
      arcTrail = D2.scale r $ D2.arc (Rad s) (Rad e)
  in ( arcTrail <> t, c, rest )
pathToTrail' (ArcNeg p0 r s e path) _ = 
  let endP = translateP (pointToVec p0) $ rotateP e $ Point r 0
      (t, c, rest) = pathToTrail' path endP
      arcTrail = D2.scale r $ D2.arcCW (Rad s) (Rad e)
  in ( arcTrail <> t, c, rest )
pathToTrail' End _ = (mempty, False, Nothing)
pathToTrail' Close _ = (mempty, True, Nothing)














