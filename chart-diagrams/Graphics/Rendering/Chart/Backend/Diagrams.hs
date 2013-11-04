{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , cBackendToFile

  -- * EPS Utility Functions
  , cBackendToEPSFile
  , renderableToEPSFile            -- deprecated
  , renderableToEPSFile'           -- deprecated
  
  -- * SVG Utility Functions
  , cBackendToSVG
  , cBackendToEmbeddedFontSVG  
  , renderableToSVG
  , renderableToSVG'
  , renderableToSVGFile            -- deprecated
  , renderableToSVGFile'           -- deprecated
  , renderableToSVGString
  , renderableToSVGString'
  
  -- * SVG Embedded Font Utility Functions
  , renderableToEmbeddedFontSVG
  , renderableToEmbeddedFontSVG'
  , renderableToEmbeddedFontSVGFile  -- deprecated
  , renderableToEmbeddedFontSVGFile' -- deprecated
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
  , R2, P2, T2
  , r2, p2, unr2, unp2
  , Trail(..), Segment
  , Rad(..), CircleFrac(..)
  , (.+^), (<->), (~~)
  )
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD as D2
import qualified Diagrams.TwoD.Arc as D2
import qualified Diagrams.TwoD.Text as D2
import qualified Diagrams.Backend.Postscript as DEPS
import qualified Diagrams.Backend.SVG as DSVG

import Text.Blaze.Svg.Renderer.Utf8 ( renderSvg )
import qualified Text.Blaze.Svg11 as Svg

import qualified Graphics.SVGFonts.CharReference as F
import qualified Graphics.SVGFonts.ReadFont as F
import Graphics.SVGFonts.WriteFont ( makeSvgFont )

import Graphics.Rendering.Chart.Backend as G
import Graphics.Rendering.Chart.Backend.Impl
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Geometry as G
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Renderable

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

-- | Generate an image file for the given renderable, at the specified path. Size, format,
-- and text rendering mode are all set through the `FileOptions` parameter.
renderableToFile :: FileOptions -> Renderable a -> FilePath -> IO (PickFn a)
renderableToFile fo r path = cBackendToFile fo cb path
  where
    cb = render r (_fo_size fo)

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
        BS.writeFile path (renderSvg svg)
        return a
      SVG_EMBEDDED -> do
        let (svg,a) = cBackendToEmbeddedFontSVG cb env
        BS.writeFile path (renderSvg svg)
        return a
  where
    (w,h) = _fo_size fo

-- -----------------------------------------------------------------------
-- SVG Utility Functions
-- -----------------------------------------------------------------------

-- | Output the given renderable to a SVG file of the specifed size
--   (in points), to the specified file using the default environment.
{-# DEPRECATED renderableToSVGFile "use renderToFile" #-}
renderableToSVGFile :: Renderable a -> Double -> Double -> FilePath -> IO (PickFn a)
renderableToSVGFile r w h file = do
  (svg, x) <- renderableToSVGString r w h
  BS.writeFile file svg
  return x

-- | Output the given renderable to a SVG file using the given environment.
{-# DEPRECATED renderableToSVGFile' "use renderToFile" #-}
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
      svg = D.renderDia DSVG.SVG (DSVG.SVGOptions (D2.Dims w h) Nothing) d
  in (svg, x)

-- -----------------------------------------------------------------------
-- SVG Embedded Font Utility Functions
-- -----------------------------------------------------------------------

-- | Output the given renderable to a SVG file of the specifed size
--   (in points), to the specified file using the default environment.
--   Font are embedded to save space.
{-# DEPRECATED renderableToEmbeddedFontSVGFile "use renderToFile" #-}
renderableToEmbeddedFontSVGFile :: Renderable a -> Double -> Double -> FilePath -> IO (PickFn a)
renderableToEmbeddedFontSVGFile r w h file = do
  (svg, x) <- renderableToEmbeddedFontSVG r w h
  BS.writeFile file $ renderSvg svg
  return x

-- | Output the given renderable to a SVG file using the given environment.
--   Font are embedded to save space.
{-# DEPRECATED renderableToEmbeddedFontSVGFile' "use renderToFile" #-}
renderableToEmbeddedFontSVGFile' :: Renderable a -> DEnv -> FilePath -> IO (PickFn a)
renderableToEmbeddedFontSVGFile' r env file = do
  let (svg, x) = renderableToEmbeddedFontSVG' r env
  BS.writeFile file $ renderSvg svg
  return x

-- | Output the given renderable as a SVG of the specifed size
--   (in points) using the default environment.
--   Font are embedded to save space.
renderableToEmbeddedFontSVG :: Renderable a -> Double -> Double -> IO (Svg.Svg, PickFn a)
renderableToEmbeddedFontSVG r w h = do
  env <- defaultEnv vectorAlignmentFns w h
  return $ renderableToEmbeddedFontSVG' r env

-- | Output the given renderable as a SVG using the given environment.
--   Font are embedded to save space.
renderableToEmbeddedFontSVG' :: Renderable a -> DEnv -> (Svg.Svg,PickFn a)
renderableToEmbeddedFontSVG' r env = cBackendToEmbeddedFontSVG (render r size) env
  where
    size = envOutputSize env

cBackendToEPSFile :: ChartBackend a -> DEnv -> FilePath -> IO a
cBackendToEPSFile cb env path = do
    let (w, h) = envOutputSize env
        (d, a) = runBackend env cb
        psOpts = DEPS.PostscriptOptions path (D2.Dims w h) DEPS.EPS
    D.renderDia DEPS.Postscript psOpts d
    return a
  
cBackendToSVG :: ChartBackend a -> DEnv -> (Svg.Svg,a)
cBackendToSVG cb env = (svg,a)
  where
    (w, h) = envOutputSize env
    (d, a) = runBackend env cb
    svg = D.renderDia DSVG.SVG (DSVG.SVGOptions (D2.Dims w h) Nothing) d

cBackendToEmbeddedFontSVG :: ChartBackend a -> DEnv -> (Svg.Svg,a)
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
    svg = D.renderDia DSVG.SVG (DSVG.SVGOptions (D2.Dims w h) fontDefs) d

-- -----------------------------------------------------------------------
-- EPS Utility Functions
-- -----------------------------------------------------------------------

-- | Output the given renderable to a EPS file using the default environment.
{-# DEPRECATED renderableToEPSFile "use renderToFile" #-}
renderableToEPSFile :: Renderable a -> Double -> Double -> FilePath -> IO (PickFn a)
renderableToEPSFile r w h file = do
  env <- defaultEnv vectorAlignmentFns w h
  renderableToEPSFile' r env file

-- | Output the given renderable to a EPS file using the given environment.
{-# DEPRECATED renderableToEPSFile' "use renderToFile" #-}
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

alterFontFamily :: String -> DFont -> DFont
alterFontFamily n (fd, om) = (fd { F.fontDataFamily = n }, om)

isFontFamily :: String -> DFont -> Bool
isFontFamily n (fd, _) = n == F.fontDataFamily fd
  
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
  let cb = render r (envOutputSize env)
  in runBackend env cb

-- | Run this backends renderer.
runBackend :: (D.Backend b R2, D.Renderable (D.Path R2) b)
           => DEnv   -- ^ Environment to start rendering with.
           -> ChartBackend a    -- ^ Chart render code.
           -> (Diagram b R2, a) -- ^ The diagram.
runBackend env m = 
  let (d, x) = evalState (runBackend' TextRenderSvg $ withDefaultStyle m) env
  in (adjustOutputDiagram env d, x)

-- | Run this backends renderer.
runBackendWithGlyphs :: ( D.Backend b R2
                        , D.Renderable (D.Path R2) b
                        , D.Renderable (D2.Text) b)
                     => DEnv   -- ^ Environment to start rendering with.
                     -> ChartBackend a    -- ^ Chart render code.
                     -> ( Diagram b R2, a
                        , M.Map (String, FontSlant, FontWeight) (S.Set String))
runBackendWithGlyphs env m = 
  let ((d, x), env') = runState (runBackend' TextRenderNative $ withDefaultStyle m) env
  in (adjustOutputDiagram env d, x, envUsedGlyphs env')

-- | Flag to decide which technique should ne used to render text.
--   The type parameter is the primitive that has to be supported by 
--   a backend when rendering text using this technique.
data TextRender a where
  TextRenderNative :: TextRender (D2.Text)
  TextRenderSvg    :: TextRender (D.Path R2)

runBackend' :: (D.Renderable (D.Path R2) b, D.Renderable t b) 
            => TextRender t -> ChartBackend a -> DState (Diagram b R2, a)
runBackend' tr m = eval tr $ view $ m
  where
    eval :: (D.Renderable (D.Path R2) b, D.Renderable t b)
         => TextRender t -> ProgramView ChartBackendInstr a -> DState (Diagram b R2, a)
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

    step :: (D.Renderable (D.Path R2) b, D.Renderable t b)
         => TextRender t -> (v -> ChartBackend a) -> v -> DState (Diagram b R2, a)
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

dDrawTextSvg :: (D.Renderable (D.Path R2) b)
             => Point -> String -> DState (Diagram b R2)
dDrawTextSvg (Point x y) text = do
  env <- get
  return $ D.transform (toTransformation $ translate (Vector x y) 1)
         $ applyFontStyleSVG (envFontStyle env)
         $ D2.scaleY (-1)
         $ F.textSVG_ (fontStyleToTextOpts env text)

dDrawTextNative :: (D.Renderable D2.Text b)
                => Point -> String -> DState (Diagram b R2)
dDrawTextNative (Point x y) text = do
  env <- get
  addGlyphsOfString text
  return $ D.transform (toTransformation $ translate (Vector x y) 1)
         $ applyFontStyleText (envFontStyle env) 
         $ D2.scaleY (-1)
         $ D2.baselineText text

dWith :: (D.Renderable (D.Path R2) b, D.Renderable t b)
      => TextRender t -> (DEnv -> DEnv) -> (Diagram b R2 -> Diagram b R2) 
      -> ChartBackend a -> DState (Diagram b R2, a)
dWith tr envF dF m = dLocal $ do
  modify envF
  (ma, a) <- runBackend' tr m
  return (dF ma, a)

dWithTransform :: (D.Renderable (D.Path R2) b, D.Renderable t b)
               => TextRender t -> Matrix -> ChartBackend a -> DState (Diagram b R2, a)
dWithTransform tr t = dWith tr id $ D.transform (toTransformation t)

dWithLineStyle :: (D.Renderable (D.Path R2) b, D.Renderable t b)
               => TextRender t -> LineStyle -> ChartBackend a -> DState (Diagram b R2, a)
dWithLineStyle tr ls = dWith tr id $ applyLineStyle ls

dWithFillStyle :: (D.Renderable (D.Path R2) b, D.Renderable t b)
               => TextRender t -> FillStyle -> ChartBackend a -> DState (Diagram b R2, a)
dWithFillStyle tr fs = dWith tr id $ applyFillStyle fs

dWithFontStyle :: (D.Renderable (D.Path R2) b, D.Renderable t b)
               => TextRender t -> FontStyle -> ChartBackend a -> DState (Diagram b R2, a)
dWithFontStyle tr fs = dWith tr (\e -> e { envFontStyle = fs }) $ id

dWithClipRegion :: (D.Renderable (D.Path R2) b, D.Renderable t b)
                => TextRender t -> Rect -> ChartBackend a -> DState (Diagram b R2, a)
dWithClipRegion tr clip = dWith tr id $ D2.clipBy (convertPath True $ rectPath clip)

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

adjustOutputDiagram :: (D.Backend b R2) => DEnv -> Diagram b R2 -> Diagram b R2
adjustOutputDiagram env d = D2.reflectY $ D2.view (p2 (0,0)) (r2 (envOutputSize env)) d

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
applyFontStyleSVG :: (D.HasStyle a) => FontStyle -> a -> a
applyFontStyleSVG fs = applyLineStyle noLineStyle 
                     . applyFillStyle (solidFillStyle $ _font_color fs)

applyFontStyleText :: (D.HasStyle a) => FontStyle -> a -> a
applyFontStyleText fs = D2.font (_font_name fs)
                      . D2.fontSize (_font_size fs)
                      . D2.fontSlant (convertFontSlant $ _font_slant fs)
                      . D2.fontWeight (convertFontWeight $ _font_weight fs)
                      . D.fillColor (_font_color fs)

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

----------------------------------------------------------------------

$( makeLenses ''FileOptions )
