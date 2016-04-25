
module Drawing.Tests where

import Data.Monoid
import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Data.List

import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Backend

supportLineStyle :: LineStyle
supportLineStyle = def
  { _line_color = withOpacity red 0.15
  , _line_width = 2
  , _line_dashes = []
  , _line_cap = LineCapButt
  , _line_join = LineJoinBevel
  }

withTestEnv :: BackendProgram a -> BackendProgram a
withTestEnv m = do
  let p = rectPath $ Rect (Point 0 0) (Point 500 500)
  withFillStyle (solidFillStyle $ opaque white) $ fillPath p >> m

withCenterRot :: Double -> Int -> Int -> BackendProgram a -> BackendProgram a
withCenterRot a x y m =
  withTranslation (Point (fromIntegral x)
                         (fromIntegral y)) $ withRotation a $ m

tests :: [(String, Int, Int, BackendProgram ())]
tests = [ ("lines", 500, 500, testLines)
        , ("arcs" , 500, 500, testArcs)
        , ("text" , 500, 500, testText)
        , ("fill" , 500, 500, testFill)
        , ("clip" , 500, 500, testClip)
        , ("paths" , 500, 500, testPaths)
        , ("text-metrics" , 500, 500, testTextMetrics)
        ] ++ ( (flip map) testEnvironments
               $ \(name, env) -> ("environment-" ++ name, 500, 500, env)
             )

testPaths :: BackendProgram ()
testPaths = withTestEnv
          $ withLineStyle (def { _line_width = 10, _line_join = LineJoinMiter })
          $ withFillStyle (solidFillStyle $ opaque red)
          $ do
  let distantArc = moveTo' 10 10
                <> lineTo' 10 50
                <> arcNeg' 50 30 20 (pi / 2) (-(pi / 2))
  strokePath $ distantArc
  withTranslation (Point  90 0) $ strokePath $ distantArc <> close
  withTranslation (Point 180 0) $ fillPath   $ distantArc

  let multiArc = arcNeg' 30 150 20 pi 0
              <> arc' 90 150 40 pi (2*pi)
              <> arcNeg' 190 150 60 pi (0)
  strokePath $ multiArc
  withTranslation (Point 0 100) $ strokePath $ multiArc <> close
  withTranslation (Point 0 200) $ fillPath $ multiArc <> close

  let multiDistArc = arc' 380 150 60 pi (2*pi)
                  <> arc' 350 130 60 0 pi
  strokePath $ multiDistArc
  withTranslation (Point 0 140) $ strokePath $ multiDistArc <> close
  withTranslation (Point 0 280) $ fillPath $ multiDistArc <> close

testTextMetrics :: BackendProgram ()
testTextMetrics = withTestEnv $ do

  withFontStyle (def { _font_size = 20 }) $ do
    let text = "Text Example"
    tm <- textSize text
    drawText (Point 100 125) text
    -- Support Lines
    withLineStyle supportLineStyle $ do
      -- Baseline
      strokePath $ moveTo' 0 125 <> lineTo' 500 125
      -- Above the text
      strokePath $ moveTo' 0 (125 - textSizeAscent tm) <> lineTo' 500 (125 - textSizeAscent tm)
      -- Beneath the text
      strokePath $ moveTo' 0 (125 + textSizeDescent tm) <> lineTo' 500 (125 + textSizeDescent tm)
      -- Right side of text
      strokePath $ moveTo' (100 + textSizeWidth tm) 0 <> lineTo' (100 + textSizeWidth tm) 250
      -- Left side of text
      strokePath $ moveTo' 100 0 <> lineTo' 100 250

  withFontStyle (def { _font_size = 15 }) $ do
    (flip mapM_) [ (VTA_Top, "Top", 10)
                 , (VTA_Centre, "Centre", 50)
                 , (VTA_BaseLine, "BaseLine", 110)
                 , (VTA_Bottom, "Bottom", 190) ] $ \(vta, name, offset) -> do
      drawTextA HTA_Left vta (Point offset 375) name

  withFontStyle (def { _font_size = 15 }) $ do
    (flip mapM_) [ (HTA_Left, "Left", 10)
                 , (HTA_Centre, "Centre", 40)
                 , (HTA_Right, "Right", 70) ] $ \(hta, name, offset) -> do
      drawTextA hta VTA_Top (Point 375 $ 250 + offset) name

  withLineStyle supportLineStyle $ do
    strokePath $ moveTo' 0 375 <> lineTo' 250 375
    strokePath $ moveTo' 375 250 <> lineTo' 375 500


testClip :: BackendProgram ()
testClip = withTestEnv $ do
  let p = rectPath $ Rect (Point 0 0) (Point 500 500)
  withFillStyle (solidFillStyle $ opaque blue) $ fillPath p
  withClipRegion (Rect (Point 100 100) (Point 300 300)) $ do
    withFillStyle (solidFillStyle $ opaque green) $ fillPath p
    withClipRegion (Rect (Point 200 200) (Point 400 400)) $ do
      withFillStyle (solidFillStyle $ withOpacity red 0.5) $ fillPath p
  withClipRegion (Rect (Point 150 50) (Point 400 150)) $ do
    withFillStyle (solidFillStyle $ opaque red) $ fillPath p

testFill :: BackendProgram ()
testFill = withTestEnv $ do
  withFillStyle (solidFillStyle $ opaque green) $ do
    fillPath $ arc' 100 100 75 0 (1.5 * pi)
  withFillStyle (solidFillStyle $ opaque blue) $ do
    fillPath $ moveTo' 475 475
            <> lineTo' 325 475
            <> lineTo' 325 325
            <> close
    withFillStyle (solidFillStyle $ withOpacity teal 0.6) $ do
      fillPath $ moveTo' 125 125
              <> lineTo' 400 125
              <> lineTo' 400 400
              <> lineTo' 125 400
              <> moveTo' 450 450
              <> lineTo' 500 450
              <> lineTo' 500 500
              <> lineTo' 450 500
    fillPath $ arcNeg' 125 400 75 0 (1.5 * pi)
            <> lineTo' 125 400

testEnvironments :: [(String, BackendProgram ())]
testEnvironments =
  let envs = [ ("fill", withFillStyle $ solidFillStyle $ opaque green)
             , ("font", withFontStyle $ def { _font_color = opaque red })
             , ("line", withLineStyle $ def { _line_color = opaque blue, _line_width = 10 })
             ]
  in (flip map) (permutations envs) $ \envPerm ->
    let name = concatMap fst envPerm
    in ( name
       , withTestEnv $ foldr1 (.) (map snd envPerm) $ do
            strokePath $ moveTo' 475 10
                      <> lineTo' 475 250
            fillPath $ arc' 450 450 40 0 (2*pi)
            drawText (Point 10 30) name
            drawText (Point 10 50) "Green Fill, Red Font, Blue Line"
       )


testText :: BackendProgram ()
testText = withTestEnv $ do
  drawText (Point 10 50) "No Scale"
  withTranslation (Point 10 70) $ do
    withScale (Vector 2 1) $ drawText (Point 0 0) "Horz. Scale"
  withTranslation (Point 10 100) $ do
    withScale (Vector 1 3) $ drawText (Point 0 0) "Vert. Scale"

  drawText (Point 150 50) "Size * 1"
  withFontStyle (def { _font_size = _font_size def * 2 }) $ drawText (Point 150 70) "Size * 2"
  withFontStyle (def { _font_size = _font_size def * 3 }) $ drawText (Point 150 100) "Size * 3"

  (flip mapM_) ([ 10, 12, 14 ] `zip` [0..]) $ \(size, n) -> do
    withFontStyle (def { _font_weight = FontWeightNormal, _font_size = size }) $
      drawText (Point 10 $ 120 + n * 20) "Normal Weight"
    withFontStyle (def { _font_weight = FontWeightBold, _font_size = size   }) $
      drawText (Point 150 $ 120 + n * 20) "Bold Weight"

    withFontStyle (def { _font_slant = FontSlantNormal, _font_size = size }) $
      drawText (Point 10 $ 180 + n * 20) "Normal Slant"
    withFontStyle (def { _font_slant = FontSlantItalic, _font_size = size }) $
      drawText (Point 150 $ 180 + n * 20) "Italic Slant"
    withFontStyle (def { _font_slant = FontSlantOblique, _font_size = size }) $
      drawText (Point 290 $ 180 + n * 20) "Oblique Slant"


  (flip mapM_) ([ 10, 12, 14 ] `zip` [0..]) $ \(size, n) -> do
    withFontStyle (def { _font_name = "sans-serif", _font_size = size }) $
      drawText (Point 10 $ 240 + n * 20) "Sans-Serif"
    withFontStyle (def { _font_name = "serif", _font_size = size }) $
      drawText (Point 150 $ 240 + n * 20) "Serif"
    withFontStyle (def { _font_name = "monospace", _font_size = size }) $
      drawText (Point 290 $ 240 + n * 20) "Monospace"

  (flip mapM_) ([ (opaque red, 12)
                , (withOpacity blue 0.3, 14)
                , (opaque green, 17) ] `zip` [0..] ) $ \((cl, size), n) -> do
    withFontStyle (def { _font_color = cl, _font_size = size }) $
      drawText (Point (10 + (n * 140)) 300) "Colored"

  (flip mapM_) [0..7] $ \n -> do
    withTranslation (Point 250 400) $
      withRotation (n * 0.125 * 2 * pi) $
        drawText (Point 30 0) $ show n
  -- Support Lines
  withLineStyle supportLineStyle $ do
    strokePath $ moveTo' 0 400 <> lineTo' 500 400
    strokePath $ moveTo' 250 300 <> lineTo' 250 500

testArcs :: BackendProgram ()
testArcs = withTestEnv $ do
  (flip mapM_) ( [ def { _line_cap = LineCapButt  , _line_join = LineJoinMiter, _line_width = 10 }
                 , def { _line_cap = LineCapRound , _line_join = LineJoinRound, _line_width = 10 }
                 , def { _line_cap = LineCapSquare, _line_join = LineJoinBevel, _line_width = 10 }
                 , def { _line_width = 10 }
                 ] `zip` [0..] )
                 $ \(ls, n) -> do
    withLineStyle ls $ do
      strokePath $ arc' 250 250 (20 * (n + 1)) (n * 0.5 * pi) ((n+1) * 0.5 * pi)
      strokePath $ arc' 250 250 (125 + 30 * (n + 1))
                                (n * 0.5 * pi + 0.25 * pi)
                                ((n+1) * 0.5 * pi + 0.25 * pi)
                <> close
  -- Support Lines
  withLineStyle supportLineStyle $ do
    strokePath $ moveTo' 250 0 <> lineTo' 250 500
    strokePath $ moveTo' 0 250 <> lineTo' 500 250
    strokePath $ moveTo' 0 0 <> lineTo' 500 500
    strokePath $ moveTo' 500 0 <> lineTo' 0 500
    withCenterRot (0.0 * pi) 250 250 $ drawText (Point 100 0) $ "0"
    withCenterRot (0.5 * pi) 250 250 $ drawText (Point  50 0) $ "1/2 * pi"
    withCenterRot (1.0 * pi) 250 250 $ drawText (Point  75 0) $ "1 * pi"
    withCenterRot (1.5 * pi) 250 250 $ drawText (Point 100 0) $ "3/2 * pi"

testLines :: BackendProgram ()
testLines = withTestEnv $ do
  -- Test Line Caps
  (flip mapM_) [ (def { _line_cap = LineCapButt  , _line_width = 10 }, 0)
               , (def { _line_cap = LineCapRound , _line_width = 10 }, 20)
               , (def { _line_cap = LineCapSquare, _line_width = 10 }, 40)
               ] $ \(ls, offset) -> do
    withLineStyle ls $ do
      strokePath $ moveTo' 125 (10 + offset)
                <> lineTo' 375 (10 + offset)
  -- Test Line Joins
  (flip mapM_) [ (def { _line_join = LineJoinMiter, _line_width = 10 }, 0)
               , (def { _line_join = LineJoinRound, _line_width = 10 }, 60)
               , (def { _line_join = LineJoinBevel, _line_width = 10 }, 120)
               ] $ \(ls, offset) -> do
    withLineStyle ls $ do
      strokePath $ moveTo' 125 (70  + offset)
                <> lineTo' 375 (70  + offset)
                <> lineTo' 250 (110 + offset)
                <> close
  -- Test Line Color & Width
  (flip mapM_) [ (def { _line_color = opaque blue, _line_width = 1 }, 0)
               , (def { _line_color = opaque green, _line_width = 5 }, 10)
               , (def { _line_color = opaque magenta, _line_width = 20 }, 35)
               , (def { _line_color = withOpacity cyan 0.2, _line_width = 10 }, 60)
               , (def { _line_color = withOpacity cyan 0.4, _line_width = 10 }, 80)
               , (def { _line_color = withOpacity cyan 0.6, _line_width = 10 }, 100)
               , (def { _line_color = withOpacity cyan 0.8, _line_width = 10 }, 120)
               , (def { _line_color = withOpacity cyan 1.0, _line_width = 10 }, 140)
               ] $ \(ls, offset) -> do
    withLineStyle ls $ do
      strokePath $ moveTo' 10  (250 + offset)
                <> lineTo' 490 (250 + offset)
  -- Test Line Dashes
  (flip mapM_) [ (def { _line_dashes = []  , _line_width = 10 }, 0)
               , (def { _line_dashes = [10] , _line_width = 10 }, 20)
               , (def { _line_dashes = [1,5,2], _line_width = 10 }, 40)
               , (def { _line_dashes = [1,5,2,3,4,6,7], _line_width = 10 }, 60)
               ] $ \(ls, offset) -> do
    withLineStyle ls $ do
      strokePath $ moveTo' 10  (410 + offset)
                <> lineTo' 490 (410 + offset)
  -- Support Lines
  withLineStyle supportLineStyle $ do
    strokePath $ moveTo' 10  0 <> lineTo' 10  500
    strokePath $ moveTo' 250 0 <> lineTo' 250 500
    strokePath $ moveTo' 490 0 <> lineTo' 490 500
    strokePath $ moveTo' 375 0 <> lineTo' 375 500
    strokePath $ moveTo' 125 0 <> lineTo' 125 500
