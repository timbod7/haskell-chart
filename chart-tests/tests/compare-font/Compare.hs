
import Data.Colour
import Data.Colour.Names
import Data.Monoid
import Data.Default.Class

import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import qualified Graphics.Rendering.Chart.Backend.Diagrams as BD
import qualified Graphics.Rendering.Chart.Backend.Cairo as BC

import Diagrams.Core.Types ( renderDia )
import Diagrams.TwoD ( SizeSpec2D(..) )
import Diagrams.Backend.Cairo hiding ( renderCairo )
import Diagrams.Backend.Cairo.Internal

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as CM

markLineStyle :: LineStyle
markLineStyle = def 
  { _line_color = opaque red
  , _line_width = 1
  }

-- Render a few lines and mark them appropriatly.
main :: IO ()
main = render ("test.png") 1000 500 $ do
  withTranslation (Point 10 10) $ do
    (flip mapM_) [0 .. 5] $ \i -> do
      let d = fromIntegral i
      withTranslation (Point 0 (d * 70)) $ testDrawText (i * 10 + 10)
    return ()
 
testDrawText :: Int -> ChartBackend ()
testDrawText fontSize = 
  withFontStyle (def { _font_size = fromIntegral fontSize, _font_name = "Source Sans Pro" }) $ do
    let text = "ÄÖÜ Testing " ++ show fontSize ++ "px"
    -- Text metrics
    ts <- textSize text
    let a = textSizeAscent ts
        d = textSizeDescent ts
    -- Baseline and descent line: Red
    withLineStyle markLineStyle $ do
      p <- alignStrokePath $ moveTo' 0 a
                          <> lineTo' 500 a
                          <> moveTo' 0 (a + d)
                          <> lineTo' 500 (a + d)
      strokePath p
    -- Bounding lines: Green
    withLineStyle (markLineStyle { _line_color = opaque green }) $ do
      p <- alignStrokePath $ moveTo' 0 0
                          <> lineTo' 500 0
                          <> moveTo' 0 (fromIntegral fontSize)
                          <> lineTo' 500 (fromIntegral fontSize)
                          <> moveTo' 0 0
                          <> lineTo' 0 (fromIntegral fontSize)
                          
      strokePath p
    drawText (Point 0 a) text
  
-- Render it side by side using cairo and diagrams cairo with SVGFonts.
render :: FilePath -> Int -> Int -> ChartBackend () -> IO ()
render f w h m = do
  rc <- renderCairo (w,h) m
  rd <- renderDiagramsCairo (w,h) m
  s <- C.createImageSurface C.FormatARGB32 w h
  C.renderWith s $ do
    C.setSourceRGB 1 1 1
    C.paint
    C.setSourceRGB 0 0 0
    C.newPath
    C.moveTo (fromIntegral w / 2) 0
    C.lineTo (fromIntegral w / 2) (fromIntegral h)
    C.stroke
  C.renderWith s $ do
    C.rectangle 0 0 500 500
    C.clip
    rc
  C.renderWith s $ do
    C.setMatrix (CM.translate (fromIntegral w/2) 0 CM.identity)
    C.rectangle 0 0 500 500
    C.clip
    rd
  C.surfaceWriteToPNG s f

renderCairo :: (Int, Int) -> ChartBackend () -> IO (C.Render ())
renderCairo (w,h) m = do
  return $ BC.runBackend (BC.defaultEnv bitmapAlignmentFns) m
  
renderDiagramsCairo :: (Int, Int) -> ChartBackend () -> IO (C.Render ())
renderDiagramsCairo (w,h) m = do
  env <- BD.defaultEnv bitmapAlignmentFns
  let (d, _) = BD.runBackend env m
  return $ snd $ renderDia Cairo (CairoOptions "" (Dims (fromIntegral w) (fromIntegral h)) PNG True) d