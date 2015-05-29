{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The backend to render charts with the diagrams library.
module Graphics.Rendering.Chart.Backend.Diagrams
  ( runBackend
  , runBackendR
  , defaultEnv
  , customFontEnv
  , DEnv(..), DFont

  -- * File Output Functons
  , FileFormat(..)
  , FileOptions(..)
  , fo_size
  , fo_format
  , fo_customFonts
  , renderableToFile
  , toFile
  , cBackendToFile

  -- * EPS Utility Functions
  , cBackendToEPSFile

  -- * SVG Utility Functions
  , cBackendToSVG
  , cBackendToEmbeddedFontSVG
  , renderableToSVG
  , renderableToSVG'
  , renderableToSVGString
  , renderableToSVGString'

  -- * SVG Embedded Font Utility Functions
  , renderableToEmbeddedFontSVG
  , renderableToEmbeddedFontSVG'
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

import Control.Lens(makeLenses)
import Control.Monad.Operational
import Control.Monad.State.Lazy

import Diagrams.Core.Transform ( Transformation(..) )
import Diagrams.Prelude
  ( Diagram
  , V2, P2, T2
  , r2, p2, unr2, unp2
  , rad, (@@)
  , Trail(..), Segment
  , (.+^), (<->), (~~)
  )
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD as D2
import Diagrams (N, V)
import Diagrams.TwoD (V2)
import qualified Diagrams.TwoD.Arc as D2
import qualified Diagrams.TwoD.Text as D2
import qualified Diagrams.Backend.Postscript as DEPS
import qualified Diagrams.Backend.SVG as DSVG

import Lucid.Svg (renderBS)
import qualified Lucid.Svg as Svg

import qualified Graphics.SVGFonts as F
import qualified Graphics.SVGFonts.CharReference as F
import qualified Graphics.SVGFonts.ReadFont as F
import Graphics.SVGFonts.WriteFont ( makeSvgFont )

import Graphics.Rendering.Chart.Backend as G
import Graphics.Rendering.Chart.Backend.Impl
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Geometry as G
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.State(EC, execEC)

import Paths_Chart_diagrams ( getDataFileName )

-- -----------------------------------------------------------------------
-- General purpose file output function
-- -----------------------------------------------------------------------


-- | The file output format:
--     EPS -> Embedded Postscript
--     SVG -> SVG with text rendered as stroked paths
--     SVG -> SVG with embedded font information and text rendered as text operations
data FileFormat = EPS
                | SVG
                | SVG_EMBEDDED

data FileOptions = FileOptions {
  _fo_size :: (Double,Double),
  _fo_format :: FileFormat,
  _fo_customFonts :: M.Map (String, FontSlant, FontWeight) FilePath
}

instance Default FileOptions where
  def =  FileOptions (800,600) SVG M.empty


-- | Generate an image file for the given renderable, at the specified path. Size, format,
-- and text rendering mode are all set through the `FileOptions` parameter.
renderableToFile :: FileOptions -> FilePath -> Renderable a -> IO (PickFn a)
renderableToFile fo path r = cBackendToFile fo cb path
  where
    cb = render r (_fo_size fo)

-- | Generate an image file from from the state content of an EC
-- computation. The state may have any type that is an instance of
-- `ToRenderable`
toFile :: (Default r,ToRenderable r) => FileOptions -> FilePath -> EC r () -> IO ()
toFile fo path ec = void $ renderableToFile fo path (toRenderable (execEC ec))

-- | Generate an image file for the given drawing instructions, at the specified path. Size and
-- format are set through the `FileOptions` parameter.
cBackendToFile :: FileOptions -> ChartBackend a -> FilePath -> IO a
cBackendToFile fo cb path = do
    env <- customFontEnv vectorAlignmentFns w h (_fo_customFonts fo)
    case _fo_format fo of
      EPS -> do
        cBackendToEPSFile cb env path
      SVG -> do
        let (svg, a) = cBackendToSVG cb env
        BS.writeFile path (renderBS svg)
        return a
      SVG_EMBEDDED -> do
        let (svg,a) = cBackendToEmbeddedFontSVG cb env
        BS.writeFile path (renderBS svg)
        return a
  where
    (w,h) = _fo_size fo

-- -----------------------------------------------------------------------
-- SVG Utility Functions
-- -----------------------------------------------------------------------

-- | Output the given renderable to a string containing a SVG of the specifed size
--   (in points) using the default environment.
renderableToSVGString :: Renderable a -> Double -> Double -> IO (BS.ByteString, PickFn a)
renderableToSVGString  r w h = do
  (svg, x) <- renderableToSVG r w h
  return (renderBS svg, x)

-- | Output the given renderable to a string containing a SVG using the given environment.
renderableToSVGString' :: Renderable a -> DEnv Double -> (BS.ByteString, PickFn a)
renderableToSVGString'  r env =
  let (svg, x) = renderableToSVG' r env
  in (renderBS svg, x)

-- | Output the given renderable as a SVG of the specifed size
--   (in points) using the default environment.

renderableToSVG :: Renderable a -> Double -> Double -> IO (Svg.Svg (), PickFn a)
renderableToSVG r w h = do
   env <- defaultEnv vectorAlignmentFns w h
   return $ renderableToSVG' r env

-- | Output the given renderable as a SVG using the given environment.
renderableToSVG' :: Renderable a -> DEnv Double -> (Svg.Svg (), PickFn a)
renderableToSVG' r env =
  let (w, h) = envOutputSize env
      (d, x) = runBackendR env r
      opts = DSVG.SVGOptions (D2.dims2D w h) Nothing T.empty
      svg = D.renderDia DSVG.SVG opts d
  in (svg, x)

-- -----------------------------------------------------------------------
-- SVG Embedded Font Utility Functions
-- -----------------------------------------------------------------------

-- | Output the given renderable as a SVG of the specifed size
--   (in points) using the default environment.
--   Font are embedded to save space.
renderableToEmbeddedFontSVG :: Renderable a -> Double -> Double -> IO (Svg.Svg (), PickFn a)
renderableToEmbeddedFontSVG r w h = do
  env <- defaultEnv vectorAlignmentFns w h
  return $ renderableToEmbeddedFontSVG' r env

-- | Output the given renderable as a SVG using the given environment.
--   Font are embedded to save space.
renderableToEmbeddedFontSVG' :: Renderable a -> DEnv Double -> (Svg.Svg (), PickFn a)
renderableToEmbeddedFontSVG' r env = cBackendToEmbeddedFontSVG (render r size) env
  where
    size = envOutputSize env

cBackendToEPSFile :: ChartBackend a -> DEnv Double -> FilePath -> IO a
cBackendToEPSFile cb env path = do
    let (w, h) = envOutputSize env
        (d, a) = runBackend env cb
        psOpts = DEPS.PostscriptOptions path (D2.dims2D w h) DEPS.EPS
    D.renderDia DEPS.Postscript psOpts d
    return a

cBackendToSVG :: ChartBackend a -> DEnv Double -> (Svg.Svg (), a)
cBackendToSVG cb env = (svg,a)
  where
    (w, h) = envOutputSize env
    (d, a) = runBackend env cb
    svg = D.renderDia DSVG.SVG (DSVG.SVGOptions (D2.dims2D w h) Nothing T.empty) d

cBackendToEmbeddedFontSVG :: ChartBackend a -> DEnv Double -> (Svg.Svg (), a)
cBackendToEmbeddedFontSVG cb env = (svg, x)
  where
    (w, h) = envOutputSize env
    (d, x, gs) = runBackendWithGlyphs env cb
    fontDefs = Just $ forM_ (M.toList gs) $ \((fFam, fSlant, fWeight), usedGs) -> do
        let fs = envFontStyle env
        let font = envSelectFont env $ fs { _font_name = fFam
                                          , _font_slant = fSlant
                                          , _font_weight = fWeight
                                          }
        makeSvgFont font usedGs
        -- M.Map (String, FontSlant, FontWeight) (S.Set String)
        -- makeSvgFont :: (FontData, OutlineMap) -> Set.Set String -> S.Svg
    svg = D.renderDia DSVG.SVG (DSVG.SVGOptions (D2.dims2D w h) Nothing T.empty) d

-- -----------------------------------------------------------------------
-- Backend
-- -----------------------------------------------------------------------

-- | The diagrams backend environement.
data DEnv n = DEnv
  { envAlignmentFns :: AlignmentFns     -- ^ The used alignment functions.
  , envFontStyle :: FontStyle           -- ^ The current/initial font style.
  , envSelectFont :: FontStyle -> F.PreparedFont n -- ^ The font selection function.
  , envOutputSize :: (n,n)              -- ^ The size of the rendered output.
  , envUsedGlyphs :: M.Map (String, FontSlant, FontWeight) (S.Set String)
    -- ^ The map of all glyphs that are used from a specific font.
  }

type DState n a = State (DEnv n) a

type DFont n = F.PreparedFont n

defaultFonts :: forall n. (RealFloat n, Read n)
             => IO (FontStyle -> F.PreparedFont n)
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

  let selectFont :: FontStyle -> F.PreparedFont n
      selectFont fs = case (_font_name fs, _font_slant fs, _font_weight fs) of
        ("serif", FontSlantNormal , FontWeightNormal) -> alterFontFamily "serif" serifR
        ("serif", FontSlantNormal , FontWeightBold  ) -> alterFontFamily "serif" serifRB
        ("serif", FontSlantItalic , FontWeightNormal) -> alterFontFamily "serif" serifRI
        ("serif", FontSlantOblique, FontWeightNormal) -> alterFontFamily "serif" serifRI
        ("serif", FontSlantItalic , FontWeightBold  ) -> alterFontFamily "serif" serifRBI
        ("serif", FontSlantOblique, FontWeightBold  ) -> alterFontFamily "serif" serifRBI

        ("sans-serif", FontSlantNormal , FontWeightNormal) -> alterFontFamily "sans-serif" sansR
        ("sans-serif", FontSlantNormal , FontWeightBold  ) -> alterFontFamily "sans-serif" sansRB
        ("sans-serif", FontSlantItalic , FontWeightNormal) -> alterFontFamily "sans-serif" sansRI
        ("sans-serif", FontSlantOblique, FontWeightNormal) -> alterFontFamily "sans-serif" sansRI
        ("sans-serif", FontSlantItalic , FontWeightBold  ) -> alterFontFamily "sans-serif" sansRBI
        ("sans-serif", FontSlantOblique, FontWeightBold  ) -> alterFontFamily "sans-serif" sansRBI

        ("monospace", _, FontWeightNormal) -> alterFontFamily "monospace" monoR
        ("monospace", _, FontWeightBold  ) -> alterFontFamily "monospace" monoRB

        (fam, FontSlantNormal , FontWeightNormal) | fam `isFontFamily` serifR   -> serifR
        (fam, FontSlantNormal , FontWeightBold  ) | fam `isFontFamily` serifRB  -> serifRB
        (fam, FontSlantItalic , FontWeightNormal) | fam `isFontFamily` serifRI  -> serifRI
        (fam, FontSlantOblique, FontWeightNormal) | fam `isFontFamily` serifRI  -> serifRI
        (fam, FontSlantItalic , FontWeightBold  ) | fam `isFontFamily` serifRBI -> serifRBI
        (fam, FontSlantOblique, FontWeightBold  ) | fam `isFontFamily` serifRBI -> serifRBI

        (fam, FontSlantNormal , FontWeightNormal) | fam `isFontFamily` sansR   -> sansR
        (fam, FontSlantNormal , FontWeightBold  ) | fam `isFontFamily` sansRB  -> sansRB
        (fam, FontSlantItalic , FontWeightNormal) | fam `isFontFamily` sansRI  -> sansRI
        (fam, FontSlantOblique, FontWeightNormal) | fam `isFontFamily` sansRI  -> sansRI
        (fam, FontSlantItalic , FontWeightBold  ) | fam `isFontFamily` sansRBI -> sansRBI
        (fam, FontSlantOblique, FontWeightBold  ) | fam `isFontFamily` sansRBI -> sansRBI

        (fam, _, FontWeightNormal) | fam `isFontFamily` monoR  -> monoR
        (fam, _, FontWeightBold  ) | fam `isFontFamily` monoRB -> monoRB

        (_, slant, weight) -> selectFont (fs { _font_name = "sans-serif" })

  return selectFont

alterFontFamily :: String -> DFont n -> DFont n
alterFontFamily n (fd, om) = (fd { F.fontDataFamily = n }, om)

isFontFamily :: String -> DFont n -> Bool
isFontFamily n (fd, _) = n == F.fontDataFamily fd

loadDefaultFont :: (RealFloat n, Read n) => FilePath -> IO (F.PreparedFont n)
loadDefaultFont file = getDataFileName file >>= F.loadFont

-- | Produce an environment with a custom set of fonts.
--   The defult fonts are still loaded as fall back.
customFontEnv :: (Read n, RealFloat n)
              => AlignmentFns     -- ^ Alignment functions to use.
              -> n -- ^ The output image width in backend coordinates.
              -> n -- ^ The output image height in backend coordinates.
              -> M.Map (String, FontSlant, FontWeight) FilePath -> IO (DEnv n)
customFontEnv alignFns w h fontFiles = do
  fonts <- traverse F.loadFont fontFiles
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
    }

-- | Produce a default environment with the default fonts.
defaultEnv :: (Read n, RealFloat n)
           => AlignmentFns -- ^ Alignment functions to use.
           -> n -- ^ The output image width in backend coordinates.
           -> n -- ^ The output image height in backend coordinates.
           -> IO (DEnv n)
defaultEnv alignFns w h = customFontEnv alignFns w h M.empty

-- | Run this backends renderer.
runBackendR :: ( D.Backend b V2 (N b), D.Renderable (D.Path V2 (N b)) b
               , D.TypeableFloat (N b), D.Metric (V b))
           => DEnv (N b)   -- ^ Environment to start rendering with.
           -> Renderable a -- ^ Chart render code.
           -> (D.QDiagram b V2 (N b) Any, PickFn a) -- ^ The diagram.
runBackendR env r =
  let cb = render r (realToFrac w, realToFrac h)
      (w,h) = envOutputSize env
  in runBackend env cb

-- | Run this backends renderer.
runBackend :: ( D.Backend b V2 (N b), D.Renderable (D.Path V2 (N b)) b
              , D.TypeableFloat (N b), D.Metric (V b))
           => DEnv (N b)        -- ^ Environment to start rendering with.
           -> ChartBackend a    -- ^ Chart render code.
           -> (D.QDiagram b V2 (N b) Any, a)    -- ^ The diagram.
runBackend env m =
  let (d, x) = evalState (runBackend' TextRenderSvg $ withDefaultStyle m) env
  in (adjustOutputDiagram env d, x)

-- | Run this backends renderer.
runBackendWithGlyphs :: ( D.Backend b V2 (N b)
                        , D.Renderable (D.Path V2 (N b)) b
                        , D.Renderable (D2.Text (N b)) b
                        , D.TypeableFloat (N b), D.Metric (V b))
                     => DEnv (N b)        -- ^ Environment to start rendering with.
                     -> ChartBackend a    -- ^ Chart render code.
                     -> ( D.QDiagram b V2 (N b) Any, a
                        , M.Map (String, FontSlant, FontWeight) (S.Set String))
runBackendWithGlyphs env m =
  let ((d, x), env') = runState (runBackend' TextRenderNative $ withDefaultStyle m) env
  in (adjustOutputDiagram env d, x, envUsedGlyphs env')

-- | Flag to decide which technique should ne used to render text.
--   The type parameter is the primitive that has to be supported by
--   a backend when rendering text using this technique.
data TextRender b a where
  TextRenderNative :: TextRender b (D2.Text (N b))
  TextRenderSvg    :: TextRender b (D.Path V2 (N b))

runBackend' :: (D.Renderable (D.Path V2 (N b)) b, D.Renderable t b, D.TypeableFloat (N b))
            => TextRender b t -> ChartBackend a
            -> DState (N b) (D.QDiagram b V2 (N b) Any, a)
runBackend' tr m = eval tr $ view $ m
  where
    eval :: (D.Renderable (D.Path V2 (N b)) b, D.Renderable t b, D.TypeableFloat (N b))
         => TextRender b t -> ProgramView ChartBackendInstr a
         -> DState (N b) (D.QDiagram b V2 (N b) Any, a)
    eval tr (Return v) = return (mempty, v)
    eval tr (StrokePath p   :>>= f) = dStrokePath  p   <># step tr f
    eval tr (FillPath   p   :>>= f) = dFillPath    p   <># step tr f
    eval tr@TextRenderSvg    (DrawText   p s :>>= f) = dDrawTextSvg    p s <># step tr f
    eval tr@TextRenderNative (DrawText   p s :>>= f) = dDrawTextNative p s <># step tr f
    eval tr (GetTextSize  s :>>= f) = dTextSize      s <>= step tr f
    eval tr (GetAlignments  :>>= f) = dAlignmentFns    <>= step tr f
    eval tr (WithTransform m p :>>= f)  = dWithTransform  tr m  p <>= step tr f
    eval tr (WithFontStyle fs p :>>= f) = dWithFontStyle  tr fs p <>= step tr f
    eval tr (WithFillStyle fs p :>>= f) = dWithFillStyle  tr fs p <>= step tr f
    eval tr (WithLineStyle ls p :>>= f) = dWithLineStyle  tr ls p <>= step tr f
    eval tr (WithClipRegion r p :>>= f) = dWithClipRegion tr r  p <>= step tr f

    step :: (D.Renderable (D.Path V2 (N b)) b, D.Renderable t b, D.TypeableFloat (N b))
         => TextRender b t -> (v -> ChartBackend a) -> v
         -> DState (N b) (D.QDiagram b V2 (N b) Any, a)
    step tr f v = runBackend' tr (f v)

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
dLocal :: DState n a -> DState n a
dLocal m = do
  env <- get
  x <- m
  env' <- get
  put $ env { envUsedGlyphs = envUsedGlyphs env' }
  return x

dStrokePath :: (D.Renderable (D.Path V2 (N b)) b, D.TypeableFloat (N b))
            => Path -> DState (N b) (D.QDiagram b V2 (N b) Any)
dStrokePath p = return $ applyFillStyle noFillStyle $ D.stroke $ convertPath False p

dFillPath :: (D.Renderable (D.Path V2 (N b)) b, D.TypeableFloat (N b))
          => Path -> DState (N b) (D.QDiagram b V2 (N b) Any)
dFillPath p = return $ applyLineStyle noLineStyle $ D.stroke $ convertPath True p

dTextSize :: (D.Renderable (D.Path V2 (N b)) b, D.TypeableFloat (N b))
          => String -> DState (N b) (D.QDiagram b V2 (N b) Any, TextSize)
dTextSize text = do
  env <- get
  let fs = envFontStyle env
  let (scaledH, scaledA, scaledD, scaledYB) = calcFontMetrics env
  return (mempty, TextSize
                { textSizeWidth = realToFrac $ D2.width $ F.textSVG' (fontStyleToTextOpts env) text
                , textSizeAscent = realToFrac scaledA -- scaledH * (a' / h') -- ascent
                , textSizeDescent = realToFrac scaledD -- scaledH * (d' / h') -- descent
                , textSizeYBearing = realToFrac scaledYB -- -scaledH * (capHeight / h)
                , textSizeHeight = realToFrac $ _font_size fs
                })

dAlignmentFns :: (D.Renderable (D.Path V2 (N b)) b, RealFloat (N b))
              => DState (N b) (D.QDiagram b V2 (N b) Any, AlignmentFns)
dAlignmentFns = do
  env <- get
  return (mempty, envAlignmentFns env)

dDrawTextSvg :: (D.Renderable (D.Path V2 (N b)) b, D.TypeableFloat (N b))
             => Point -> String -> DState (N b) (D.QDiagram b V2 (N b) Any)
dDrawTextSvg (Point x y) text = do
  env <- get
  return $ D.transform (toTransformation $ translate (Vector x y) 1)
         $ applyFontStyleSVG (envFontStyle env)
         $ D2.scaleY (-1)
         $ F.textSVG_ (fontStyleToTextOpts env) text

dDrawTextNative :: (D.Renderable (D2.Text (N b)) b, D.TypeableFloat (N b))
                => Point -> String -> DState (N b) (D.QDiagram b V2 (N b) Any)
dDrawTextNative (Point x y) text = do
  env <- get
  addGlyphsOfString text
  return $ D.transform (toTransformation $ translate (Vector x y) 1)
         $ applyFontStyleText (envFontStyle env)
         $ D2.scaleY (-1)
         $ D2.baselineText text

dWith :: ( D.TypeableFloat (N b), D.Metric V2
         , D.Renderable (D.Path V2 (N b)) b, D.Renderable t b)
      => TextRender b t -> (DEnv (N b) -> DEnv (N b))
      -> (D.QDiagram b V2 (N b) Any -> D.QDiagram b V2 (N b) Any)
      -> ChartBackend a -> DState (N b) (D.QDiagram b V2 (N b) Any, a)
dWith tr envF dF m = dLocal $ do
  modify envF
  (ma, a) <- runBackend' tr m
  return (dF ma, a)

dWithTransform :: (D.TypeableFloat (N b), D.Renderable (D.Path V2 (N b)) b, D.Renderable t b)
               => TextRender b t -> Matrix -> ChartBackend a -> DState (N b) (D.QDiagram b V2 (N b) Any, a)
dWithTransform tr t = dWith tr id $ D.transform (toTransformation t)

dWithLineStyle :: (D.TypeableFloat (N b), D.Renderable (D.Path V2 (N b)) b, D.Renderable t b)
               => TextRender b t -> LineStyle -> ChartBackend a -> DState (N b) (D.QDiagram b V2 (N b) Any, a)
dWithLineStyle tr ls = dWith tr id $ applyLineStyle ls

dWithFillStyle :: (D.TypeableFloat (N b), D.Renderable (D.Path V2 (N b)) b, D.Renderable t b)
               => TextRender b t -> FillStyle -> ChartBackend a -> DState (N b) (D.QDiagram b V2 (N b) Any, a)
dWithFillStyle tr fs = dWith tr id $ applyFillStyle fs

dWithFontStyle :: (D.TypeableFloat (N b), D.Renderable (D.Path V2 (N b)) b, D.Renderable t b)
               => TextRender b t -> FontStyle -> ChartBackend a -> DState (N b) (D.QDiagram b V2 (N b) Any, a)
dWithFontStyle tr fs = dWith tr (\e -> e { envFontStyle = fs }) $ id

dWithClipRegion :: (D.TypeableFloat (N b), D.Renderable (D.Path V2 (N b)) b, D.Renderable t b)
                => TextRender b t -> Rect -> ChartBackend a -> DState (N b) (D.QDiagram b V2 (N b) Any, a)
dWithClipRegion tr clip = dWith tr id $ D2.clipBy (convertPath True $ rectPath clip)

-- -----------------------------------------------------------------------
-- Converions Helpers
-- -----------------------------------------------------------------------

addGlyphsOfString :: String -> DState n ()
addGlyphsOfString s = do
  env <- get
  let fs = envFontStyle env
  let fontData = fst $ envSelectFont env fs
  let ligatures = (filter ((>1) . length) . M.keys . F.fontDataGlyphs) fontData
  let glyphs = fmap T.unpack $ F.characterStrings s ligatures
  modify $ \env ->
    let gKey = (_font_name fs, _font_slant fs, _font_weight fs)
        gMap = envUsedGlyphs env
        entry = case M.lookup gKey gMap of
          Nothing -> S.fromList glyphs
          Just gs -> gs `S.union` S.fromList glyphs
    in env { envUsedGlyphs = M.insert gKey entry gMap }
  return ()

pointToP2 :: RealFrac n => Point -> P2 n
pointToP2 (Point x y) = p2 (realToFrac x, realToFrac y)

adjustOutputDiagram :: (D.Backend b V2 (N b), RealFloat (N b))
                    => DEnv (N b) -> D.QDiagram b V2 (N b) Any -> D.QDiagram b V2 (N b) Any
adjustOutputDiagram env d = D2.reflectY $ D.rectEnvelope (p2 (0,0)) (r2 (envOutputSize env)) d

noLineStyle :: LineStyle
noLineStyle = def
  { _line_width = 0
  , _line_color = transparent
  }

noFillStyle :: FillStyle
noFillStyle = solidFillStyle transparent

toTransformation :: RealFloat n => Matrix -> T2 n
toTransformation m = Transformation
  (applyWithoutTrans m <-> applyWithoutTrans (invert m))
  (applyWithoutTrans (transpose m) <-> applyWithoutTrans (transpose (invert m)))
  (r2 (realToFrac $ x0 m, realToFrac $ y0 m))

transpose :: Matrix -> Matrix
transpose (Matrix xx yx xy yy _ _) = Matrix xx xy yx yy 0 0

-- | Apply a given affine transformation to a vector.
applyTransformation :: RealFloat n => Matrix -> P2 n -> P2 n
applyTransformation m p =
  let (x,y) = D2.unp2 p
      get :: RealFloat n => (Matrix -> Double) -> n
      get f = realToFrac (f m)
  in p2 ( get xx * x + get xy * y + get x0
        , get yx * x + get yy * y + get y0
        )

-- | Apply a given affine transformation to a vector.
applyWithoutTrans :: RealFloat n => Matrix -> V2 n -> V2 n
applyWithoutTrans m v =
  let (x,y) = D2.unr2 v
      get :: RealFloat n => (Matrix -> Double) -> n
      get f = realToFrac (f m)
  in r2 ( get xx * x + get xy * y
        , get yx * x + get yy * y
        )

-- | Apply the Chart line style to a diagram.
applyLineStyle :: (D.TypeableFloat (N a), D.V a ~ V2, D.HasStyle a) => LineStyle -> a -> a
applyLineStyle ls = D.lineWidth (D.global $ realToFrac $ _line_width ls)
                  . D.lineColor (_line_color ls)
                  . D.lineCap (convertLineCap $ _line_cap ls)
                  . D.lineJoin (convertLineJoin $ _line_join ls)
                  . D.dashing (map (D.global . realToFrac) $ _line_dashes ls) (D.global 0)

-- | Apply the Chart fill style to a diagram.
applyFillStyle :: (D.TypeableFloat (N a), V a ~ V2, D.HasStyle a) => FillStyle -> a -> a
applyFillStyle fs = case fs of
  FillStyleSolid cl -> D.fillColor cl

-- | Apply all pure diagrams properties from the font style.
applyFontStyleSVG :: (D.TypeableFloat (N a), D.V a ~ V2, D.HasStyle a) => FontStyle -> a -> a
applyFontStyleSVG fs = applyLineStyle noLineStyle
                     . applyFillStyle (solidFillStyle $ _font_color fs)

applyFontStyleText :: (D.TypeableFloat (N a), D.V a ~ V2, D.HasStyle a) => FontStyle -> a -> a
applyFontStyleText fs = D2.font (_font_name fs)
                      . D2.fontSize (D.global $ realToFrac $ _font_size fs)
                      . D2.fontSlant (convertFontSlant $ _font_slant fs)
                      . D2.fontWeight (convertFontWeight $ _font_weight fs)
                      . D.fillColor (_font_color fs)

-- | Calculate the font metrics for the currently set font style.
--   The returned value will be @(height, ascent, descent, ybearing)@.
calcFontMetrics :: RealFloat n => DEnv n -> (n, n, n, n)
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
      scaledHeight  = realToFrac (_font_size fs) * (h' / h)
      scaledAscent  = scaledHeight * (a' / h')
      scaledDescent = scaledHeight * (d' / h')
      scaledMaxHAdv = -scaledHeight * (capHeight / h)
  in (scaledHeight, scaledAscent, scaledDescent, scaledMaxHAdv)

fontStyleToTextOpts :: RealFloat n => DEnv n -> F.TextOpts n
fontStyleToTextOpts env =
  let fs = envFontStyle env
      font = envSelectFont env fs
      (scaledH, _, _, _) = calcFontMetrics env
  in F.TextOpts
      { F.textFont = font
      , F.mode = F.INSIDE_H
      , F.spacing = F.KERN
      , F.underline = False
      , F.textWidth = 1
      , F.textHeight = scaledH -- _font_size fs
      }

fontFromName :: (Read n, RealFloat n) => String -> F.PreparedFont n
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

convertFontSlant :: FontSlant -> D2.FontSlant
convertFontSlant fs = case fs of
  FontSlantNormal  -> D2.FontSlantNormal
  FontSlantItalic  -> D2.FontSlantItalic
  FontSlantOblique -> D2.FontSlantOblique

convertFontWeight :: FontWeight -> D2.FontWeight
convertFontWeight fw = case fw of
  FontWeightBold   -> D2.FontWeightBold
  FontWeightNormal -> D2.FontWeightNormal

-- | Convert paths. The boolean says wether all trails
--   of the path shall be closed or remain open.
convertPath :: (RealFloat n, Ord n) => Bool -> Path -> D.Path V2 n
convertPath closeAll path =
  let (start, t, restM) = pathToTrail closeAll (Point 0 0) $ makeLinesExplicit path
  in D.pathFromTrailAt t start <> case restM of
    Nothing -> mempty
    Just rest -> convertPath closeAll rest

pathToTrail :: (RealFloat n)
            => Bool -> Point -> Path
            -> (D.Point V2 n, Trail V2 n, Maybe Path)
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

makeTrail :: Bool -> D.Trail' D.Line V2 n -> Trail V2 n
makeTrail True  t = D.wrapTrail $ D.closeLine t
makeTrail False t = D.wrapTrail $ t

angleToDirection :: RealFloat n => Double -> D.Direction V2 n
angleToDirection a = D.direction $ fmap realToFrac $ D2.V2 (cos a) (sin a)

pathToTrail' :: (RealFloat n)
             => Bool -> Path -> Point -> (D.Trail' D.Line V2 n, Bool, Maybe Path)
pathToTrail' closeAll p@(MoveTo _ _) _ = (mempty, False || closeAll, Just p)
pathToTrail' closeAll (LineTo p1 path) p0 =
  let (t, c, rest) = pathToTrail' closeAll path p1
  in ( (pointToP2 p0 ~~ pointToP2 p1) <> t, c || closeAll, rest )
pathToTrail' closeAll (Arc p0 r s e path) _ =
  let endP = translateP (pointToVec p0) $ rotateP e $ Point r 0
      (t, c, rest) = pathToTrail' closeAll path endP
      arcTrail = D2.scale (realToFrac r) $ D2.arcCCW (angleToDirection s) (angleToDirection e)
  in ( arcTrail <> t, c || closeAll, rest )
pathToTrail' closeAll (ArcNeg p0 r s e path) _ =
  let endP = translateP (pointToVec p0) $ rotateP e $ Point r 0
      (t, c, rest) = pathToTrail' closeAll path endP
      arcTrail = D2.scale (realToFrac r) $ D2.arcCW (angleToDirection s) (angleToDirection e)
  in ( arcTrail <> t, c || closeAll, rest )
pathToTrail' closeAll End _ = (mempty, False || closeAll, Nothing)
pathToTrail' closeAll Close _ = (mempty, True || closeAll, Nothing)

----------------------------------------------------------------------

$( makeLenses ''FileOptions )
