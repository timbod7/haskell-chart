
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- | The backend to render charts with the diagrams library.
module Graphics.Rendering.Chart.Backend.Diagrams
  ( runBackend
  , runBackendR
  , defaultEnv
  , customFontEnv
  , DEnv(..), DFont
  , renderableToEPSFile
  , renderableToEPSFile'
  , renderableToSVG
  , renderableToSVG'
  , renderableToSVGFile
  , renderableToSVGFile'
  , renderableToSVGString
  , renderableToSVGString'
  ) where

import Data.Default.Class
import Data.Colour
import Data.Colour.SRGB
import Data.List (unfoldr)
import Data.Monoid
import Data.Traversable
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T

import Control.Monad.Operational
import Control.Monad.State.Lazy

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
import qualified Diagrams.Backend.Postscript as DEPS
import qualified Diagrams.Backend.SVG as DSVG

import Text.Blaze.Svg.Renderer.Utf8 ( renderSvg )
import qualified Text.Blaze.Svg11 as Svg

import qualified Graphics.SVGFonts.CharReference as F
import qualified Graphics.SVGFonts.ReadFont as F

import Graphics.Rendering.Chart.Backend as G
import Graphics.Rendering.Chart.Backend.Impl
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Geometry as G
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Renderable

import Paths_Chart_diagrams ( getDataFileName )

-- -----------------------------------------------------------------------
-- General Utility Functions
-- -----------------------------------------------------------------------

-- | Output the given renderable to a SVG file of the specifed size
--   (in points), to the specified file using the default environment.
renderableToSVGFile :: Renderable a -> Double -> Double -> FilePath -> IO (PickFn a)
renderableToSVGFile r w h file = do
  (svg, x) <- renderableToSVGString r w h
  BS.writeFile file svg
  return x

-- | Output the given renderable to a SVG file using the given environment.
renderableToSVGFile' :: Renderable a -> DEnv -> FilePath -> IO (PickFn a)
renderableToSVGFile' r env file = do
  let (svg, x) = renderableToSVGString' r env
  BS.writeFile file svg
  return x

-- | Output the given renderable to a string containing a SVG of the specifed size
--   (in points) using the default environment.
renderableToSVGString :: Renderable a -> Double -> Double -> IO (BS.ByteString, PickFn a)
renderableToSVGString  r w h = do
  (svg, x) <- renderableToSVG r w h
  return (renderSvg svg, x)

-- | Output the given renderable to a string containing a SVG using the given environment.
renderableToSVGString' :: Renderable a -> DEnv -> (BS.ByteString, PickFn a)
renderableToSVGString'  r env =
  let (svg, x) = renderableToSVG' r env
  in (renderSvg svg, x)

-- | Output the given renderable as a SVG of the specifed size
--   (in points) using the default environment.
renderableToSVG :: Renderable a -> Double -> Double -> IO (Svg.Svg, PickFn a)
renderableToSVG r w h = do
  env <- defaultEnv vectorAlignmentFns w h
  return $ renderableToSVG' r env

-- | Output the given renderable as a SVG using the given environment.
renderableToSVG' :: Renderable a -> DEnv -> (Svg.Svg, PickFn a)
renderableToSVG' r env = 
  let (w, h) = envOutputSize env
      (d, x) = runBackendR env r
      svg = D.renderDia DSVG.SVG (DSVG.SVGOptions $ D2.Dims w h) d
  in (svg, x)

-- | Output the given renderable to a EPS file using the default environment.
renderableToEPSFile :: Renderable a -> Double -> Double -> FilePath -> IO (PickFn a)
renderableToEPSFile r w h file = do
  env <- defaultEnv vectorAlignmentFns w h
  renderableToEPSFile' r env file

-- | Output the given renderable to a EPS file using the given environment.
renderableToEPSFile' :: Renderable a -> DEnv -> FilePath -> IO (PickFn a)
renderableToEPSFile' r env file = do
  let (w, h) = envOutputSize env
  let (d, x) = runBackendR env r
  let psOpts = DEPS.PostscriptOptions 
                  file 
                  (D2.Dims w h) 
                  DEPS.EPS
  D.renderDia DEPS.Postscript psOpts d
  return x
  

-- -----------------------------------------------------------------------
-- Backend
-- -----------------------------------------------------------------------

-- | The diagrams backend environement.
data DEnv = DEnv
  { envAlignmentFns :: AlignmentFns     -- ^ The used alignment functions.
  , envFontStyle :: FontStyle           -- ^ The current/initial font style.
  , envSelectFont :: FontStyle -> DFont -- ^ The font selection function.
  , envOutputSize :: (Double, Double)   -- ^ The size of the rendered output.
  , envUsedGlyphs :: M.Map (String, FontSlant, FontWeight) (S.Set String)
    -- ^ The map of all glyphs that are used from a specific font.
  , envRenderWithSVGFonts :: Bool
    -- ^ If the backend should use SVGFonts or the normal diarams 
    --   text primitive when rendering fonts.
  }

-- | A font a delivered by SVGFonts.
type DFont = (F.FontData, F.OutlineMap)

type DState a = State DEnv a

defaultFonts :: IO (FontStyle -> DFont)
defaultFonts = do
  serifR   <- loadDefaultFont "fonts/LinLibertine_R.svg"
  serifRB  <- loadDefaultFont "fonts/LinLibertine_RB.svg"
  serifRBI <- loadDefaultFont "fonts/LinLibertine_RBI.svg"
  serifRI  <- loadDefaultFont "fonts/LinLibertine_RI.svg"
  sansR   <- loadDefaultFont "fonts/SourceSansPro_R.svg"
  sansRB  <- loadDefaultFont "fonts/SourceSansPro_RB.svg"
  sansRBI <- loadDefaultFont "fonts/SourceSansPro_RBI.svg"
  sansRI  <- loadDefaultFont "fonts/SourceSansPro_RI.svg"
  monoR  <- loadDefaultFont "fonts/SourceCodePro_R.svg"
  monoRB <- loadDefaultFont "fonts/SourceCodePro_RB.svg"
  
  let selectFont :: FontStyle -> DFont
      selectFont fs = case (_font_name fs, _font_slant fs, _font_weight fs) of
        ("serif", FontSlantNormal , FontWeightNormal) -> serifR
        ("serif", FontSlantNormal , FontWeightBold  ) -> serifRB
        ("serif", FontSlantItalic , FontWeightNormal) -> serifRI
        ("serif", FontSlantOblique, FontWeightNormal) -> serifRI
        ("serif", FontSlantItalic , FontWeightBold  ) -> serifRBI
        ("serif", FontSlantOblique, FontWeightBold  ) -> serifRBI
        
        ("sans-serif", FontSlantNormal , FontWeightNormal) -> sansR
        ("sans-serif", FontSlantNormal , FontWeightBold  ) -> sansRB
        ("sans-serif", FontSlantItalic , FontWeightNormal) -> sansRI
        ("sans-serif", FontSlantOblique, FontWeightNormal) -> sansRI
        ("sans-serif", FontSlantItalic , FontWeightBold  ) -> sansRBI
        ("sans-serif", FontSlantOblique, FontWeightBold  ) -> sansRBI
        
        ("monospace", _, FontWeightNormal) -> monoR
        ("monospace", _, FontWeightBold  ) -> monoRB
        
        (_, slant, weight) -> selectFont (fs { _font_name = "sans-serif" })
  
  return selectFont
  
loadDefaultFont :: FilePath -> IO DFont
loadDefaultFont file = getDataFileName file >>= return . F.outlMap

loadFont :: FilePath -> IO DFont
loadFont = return . F.outlMap

-- | Produce an environment with a custom set of fonts.
--   The defult fonts are still loaded as fall back.
customFontEnv :: AlignmentFns     -- ^ Alignment functions to use.
              -> Double -- ^ The output image width in backend coordinates.
              -> Double -- ^ The output image height in backend coordinates.
              -> M.Map (String, FontSlant, FontWeight) FilePath -> IO DEnv
customFontEnv alignFns w h fontFiles = do
  fonts <- traverse loadFont fontFiles
  selectFont <- defaultFonts
  return $ DEnv 
    { envAlignmentFns = alignFns
    , envFontStyle = def
    , envSelectFont = \fs -> 
        case M.lookup (_font_name fs, _font_slant fs, _font_weight fs) fonts of
          Just font -> font
          Nothing -> selectFont fs
    , envOutputSize = (w,h)
    , envUsedGlyphs = M.empty
    , envRenderWithSVGFonts = True
    }

-- | Produce a default environment with the default fonts.
defaultEnv :: AlignmentFns     -- ^ Alignment functions to use.
           -> Double -- ^ The output image width in backend coordinates.
           -> Double -- ^ The output image height in backend coordinates.
           -> IO DEnv
defaultEnv alignFns w h = customFontEnv alignFns w h M.empty

-- | Run this backends renderer.
runBackendR :: (D.Backend b R2, D.Renderable (D.Path R2) b)
           => DEnv         -- ^ Environment to start rendering with.
           -> Renderable a -- ^ Chart render code.
           -> (Diagram b R2, PickFn a) -- ^ The diagram.
runBackendR env r = 
  let cr = render r (envOutputSize env)
  in runBackend env cr

-- | Run this backends renderer.
runBackend :: (D.Backend b R2, D.Renderable (D.Path R2) b)
           => DEnv   -- ^ Environment to start rendering with.
           -> ChartBackend a    -- ^ Chart render code.
           -> (Diagram b R2, a) -- ^ The diagram.
runBackend env m = 
  let (d, x, _) = runBackendWithGlyphs env m
  in (d, x)

-- | Run this backends renderer.
runBackendWithGlyphs :: (D.Backend b R2, D.Renderable (D.Path R2) b)
                     => DEnv   -- ^ Environment to start rendering with.
                     -> ChartBackend a    -- ^ Chart render code.
                     -> (Diagram b R2, a, M.Map (String, FontSlant, FontWeight) (S.Set String))
runBackendWithGlyphs env m = 
  let ((d, x), env') = runState (runBackend' (withDefaultStyle m)) env
  in (D2.reflectY $ D2.view (p2 (0,0)) (r2 (envOutputSize env)) d, x, envUsedGlyphs env')

runBackend' :: (D.Renderable (D.Path R2) b) 
            => ChartBackend a -> DState (Diagram b R2, a)
runBackend' m = eval (view m)
  where
    eval :: (D.Renderable (D.Path R2) b)
         => ProgramView ChartBackendInstr a -> DState (Diagram b R2, a)
    eval (Return v) = return (mempty, v)
    eval (StrokePath p   :>>= f) = dStrokePath p   <># step f
    eval (FillPath   p   :>>= f) = dFillPath   p   <># step f
    eval (DrawText   p s :>>= f) = dDrawText   p s <># step f
    eval (GetTextSize  s :>>= f) = dTextSize     s <>= step f
    eval (GetAlignments  :>>= f) = dAlignmentFns   <>= step f
    eval (WithTransform m p :>>= f)  = dWithTransform  m  p <>= step f
    eval (WithFontStyle fs p :>>= f) = dWithFontStyle  fs p <>= step f
    eval (WithFillStyle fs p :>>= f) = dWithFillStyle  fs p <>= step f
    eval (WithLineStyle ls p :>>= f) = dWithLineStyle  ls p <>= step f
    eval (WithClipRegion r p :>>= f) = dWithClipRegion r  p <>= step f

    step :: (D.Renderable (D.Path R2) b)
         => (v -> ChartBackend a) -> v -> DState (Diagram b R2, a)
    step f v = runBackend' (f v)
    
    (<>#) :: (Monad s, Monoid m) => s m -> (() -> s (m, a)) -> s (m, a)
    (<>#) m f = do
      ma <- m
      return (ma, ()) <>= f
    
    (<>=) :: (Monad s, Monoid m) => s (m, a) -> (a -> s (m, b)) -> s (m, b)
    (<>=) m f = do
      (ma, a) <- m
      (mb, b) <- f a
      return (mb <> ma, b)

-- | Executes the given state locally, but preserves the changes to the 'envUsedGlyphs'
--   map. Assumes that values are never removed from the map inbetween.
dLocal :: DState a -> DState a
dLocal m = do
  env <- get
  x <- m
  env' <- get
  put $ env { envUsedGlyphs = envUsedGlyphs env' }
  return x

dStrokePath :: (D.Renderable (D.Path R2) b)
            => Path -> DState (Diagram b R2)
dStrokePath p = return $ applyFillStyle noFillStyle $ D.stroke $ convertPath False p

dFillPath :: (D.Renderable (D.Path R2) b)
          => Path -> DState (Diagram b R2)
dFillPath p = return $ applyLineStyle noLineStyle $ D.stroke $ convertPath True p

dTextSize :: (D.Renderable (D.Path R2) b)
          => String -> DState (Diagram b R2, TextSize)
dTextSize text = do
  env <- get
  let fs = envFontStyle env
  let (scaledH, scaledA, scaledD, scaledYB) = calcFontMetrics env
  return (mempty, TextSize 
                { textSizeWidth = D2.width $ F.textSVG' 
                                           $ fontStyleToTextOpts env text
                , textSizeAscent = scaledA -- scaledH * (a' / h') -- ascent
                , textSizeDescent = scaledD -- scaledH * (d' / h') -- descent
                , textSizeYBearing = scaledYB -- -scaledH * (capHeight / h)
                , textSizeHeight = _font_size $ fs
                })

dAlignmentFns :: (D.Renderable (D.Path R2) b)
              => DState (Diagram b R2, AlignmentFns)
dAlignmentFns = do
  env <- get
  return (mempty, envAlignmentFns env)

dDrawText :: (D.Renderable (D.Path R2) b)
          => Point -> String -> DState (Diagram b R2)
dDrawText (Point x y) text = do
  env <- get
  addGlyphsOfString text
  return $ D.transform (toTransformation $ translate (Vector x y) 1)
         $ applyFontStyle (envFontStyle env)
         $ D2.scaleY (-1)
         $ F.textSVG_ (fontStyleToTextOpts env text)

dWith :: (D.Renderable (D.Path R2) b)
      => (DEnv -> DEnv) -> (Diagram b R2 -> Diagram b R2) 
      -> ChartBackend a -> DState (Diagram b R2, a)
dWith envF dF m = dLocal $ do
  env <- get
  put $ envF env
  (ma, a) <- runBackend' m
  return (dF ma, a)

dWithTransform :: (D.Renderable (D.Path R2) b)
               => Matrix -> ChartBackend a -> DState (Diagram b R2, a)
dWithTransform t = dWith id $ D.transform (toTransformation t)

dWithLineStyle :: (D.Renderable (D.Path R2) b)
               => LineStyle -> ChartBackend a -> DState (Diagram b R2, a)
dWithLineStyle ls = dWith id $ applyLineStyle ls

dWithFillStyle :: (D.Renderable (D.Path R2) b)
               => FillStyle -> ChartBackend a -> DState (Diagram b R2, a)
dWithFillStyle fs = dWith id $ applyFillStyle fs

dWithFontStyle :: (D.Renderable (D.Path R2) b)
               => FontStyle -> ChartBackend a -> DState (Diagram b R2, a)
dWithFontStyle fs = dWith (\e -> e { envFontStyle = fs }) $ id

dWithClipRegion :: (D.Renderable (D.Path R2) b)
                => Rect -> ChartBackend a -> DState (Diagram b R2, a)
dWithClipRegion clip = dWith id $ D2.clipBy (convertPath True $ rectPath clip)

-- -----------------------------------------------------------------------
-- Converions Helpers
-- -----------------------------------------------------------------------

addGlyphsOfString :: String -> DState ()
addGlyphsOfString s = do
  env <- get
  let fs = envFontStyle env
  let fontData = fst $ envSelectFont env fs
  let ligatures = ((filter ((>1) . length)) . (M.keys) . F.fontDataGlyphs) fontData
  let glyphs = fmap T.unpack $ F.characterStrings s ligatures
  modify $ \env -> 
    let gKey = (_font_name fs, _font_slant fs, _font_weight fs)
        gMap = envUsedGlyphs env
        entry = case M.lookup gKey gMap of
          Nothing -> S.fromList glyphs
          Just gs -> gs `S.union` S.fromList glyphs
    in env { envUsedGlyphs = M.insert gKey entry gMap }
  return ()

pointToP2 :: Point -> P2
pointToP2 (Point x y) = p2 (x,y)

noLineStyle :: LineStyle
noLineStyle = def 
  { _line_width = 0
  , _line_color = transparent
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
applyLineStyle ls = D.lineWidth (_line_width ls) 
                  . D.lineColor (_line_color ls) 
                  . D.lineCap (convertLineCap $ _line_cap ls) 
                  . D.lineJoin (convertLineJoin $ _line_join ls) 
                  . D.dashing (_line_dashes ls) 0

-- | Apply the Chart fill style to a diagram.
applyFillStyle :: (D.HasStyle a) => FillStyle -> a -> a
applyFillStyle fs = case fs of
  FillStyleSolid cl -> D.fillColor cl

-- | Apply all pure diagrams properties from the font style.
applyFontStyle :: (D.HasStyle a) => FontStyle -> a -> a
applyFontStyle fs = applyLineStyle noLineStyle 
                  . applyFillStyle (solidFillStyle $ _font_color fs)

-- | Calculate the font metrics for the currently set font style.
--   The returned value will be @(height, ascent, descent, ybearing)@.
calcFontMetrics :: DEnv -> (Double, Double, Double, Double)
calcFontMetrics env = 
  let fs = envFontStyle env
      font@(fontData,_) = envSelectFont env fs
      bbox = F.fontDataBoundingBox fontData
      capHeight = F.fontDataCapHeight fontData
      a = bbox !! 3
      d = -bbox !! 1
      h = unscaledH
      a' = unscaledH
      d' = (d / h) * h'
      h' = (a + d) / (1 - d / h)
      unscaledH = F.bbox_dy $ fontData
      scaledHeight  = _font_size fs * (h' / h)
      scaledAscent  = scaledHeight * (a' / h')
      scaledDescent = scaledHeight * (d' / h')
      scaledMaxHAdv = -scaledHeight * (capHeight / h)
  in (scaledHeight, scaledAscent, scaledDescent, scaledMaxHAdv)

fontStyleToTextOpts :: DEnv -> String -> F.TextOpts
fontStyleToTextOpts env text = 
  let fs = envFontStyle env
      font = envSelectFont env fs
      (scaledH, _, _, _) = calcFontMetrics env
  in F.TextOpts
      { F.txt = text
      , F.fdo = font
      , F.mode = F.INSIDE_H
      , F.spacing = F.KERN
      , F.underline = False
      , F.textWidth = 1
      , F.textHeight = scaledH -- _font_size fs
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

-- | Convert paths. The boolean says wether all trails 
--   of the path shall be closed or remain open.
convertPath :: Bool -> Path -> D.Path R2
convertPath closeAll path = 
  let (start, t, restM) = pathToTrail closeAll (Point 0 0) $ makeLinesExplicit path
  in D.pathFromTrailAt t start <> case restM of
    Nothing -> mempty
    Just rest -> convertPath closeAll rest

pathToTrail :: Bool -> Point -> Path 
            -> (D.Point R2, Trail R2, Maybe Path)
pathToTrail closeAll _ (MoveTo p0 path) = 
  let (t, close, rest) = pathToTrail' closeAll path p0
  in (pointToP2 p0, makeTrail close t, rest)
pathToTrail closeAll _ path@(Arc c r s _ _) = 
  let p0 = translateP (pointToVec c) $ rotateP s $ Point r 0
      (t, close, rest) = pathToTrail' closeAll path p0
  in (pointToP2 p0, makeTrail close t, rest)
pathToTrail closeAll _ path@(ArcNeg c r s _ _) = 
  let p0 = translateP (pointToVec c) $ rotateP s $ Point r 0
      (t, close, rest) = pathToTrail' closeAll path p0
  in (pointToP2 p0, makeTrail close t, rest)
pathToTrail closeAll start path = 
  let (t, close, rest) = pathToTrail' closeAll path start
  in (pointToP2 start, makeTrail close t, rest)

makeTrail :: Bool -> D.Trail' D.Line R2 -> Trail R2
makeTrail True  t = D.wrapTrail $ D.closeLine t
makeTrail False t = D.wrapTrail $ t

pathToTrail' :: Bool -> Path -> Point -> (D.Trail' D.Line R2, Bool, Maybe Path)
pathToTrail' closeAll p@(MoveTo _ _) _ = (mempty, False || closeAll, Just p)
pathToTrail' closeAll (LineTo p1 path) p0 = 
  let (t, c, rest) = pathToTrail' closeAll path p1
  in ( (pointToP2 p0 ~~ pointToP2 p1) <> t, c || closeAll, rest )
pathToTrail' closeAll (Arc p0 r s e path) _ = 
  let endP = translateP (pointToVec p0) $ rotateP e $ Point r 0
      (t, c, rest) = pathToTrail' closeAll path endP
      arcTrail = D2.scale r $ D2.arc (Rad s) (Rad e)
  in ( arcTrail <> t, c || closeAll, rest )
pathToTrail' closeAll (ArcNeg p0 r s e path) _ = 
  let endP = translateP (pointToVec p0) $ rotateP e $ Point r 0
      (t, c, rest) = pathToTrail' closeAll path endP
      arcTrail = D2.scale r $ D2.arcCW (Rad s) (Rad e)
  in ( arcTrail <> t, c || closeAll, rest )
pathToTrail' closeAll End _ = (mempty, False || closeAll, Nothing)
pathToTrail' closeAll Close _ = (mempty, True || closeAll, Nothing)














