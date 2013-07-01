 

import Data.Monoid
import Data.Default
import Data.Colour
import Data.Colour.Names
import Data.List

import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Backend.Cairo

main :: IO ()
main = (flip mapM_) tests $ \(name, w, h, draw) -> do
  render (name ++ ".png") w h draw
  

render :: FilePath -> Int -> Int -> CRender a -> IO ()
render f w h m = renderToFile m CairoPNG w h f

supportLineStyle :: LineStyle
supportLineStyle = def 
  { line_color_ = withOpacity red 0.15
  , line_width_ = 2
  , line_dashes_ = []
  , line_cap_ = LineCapButt
  , line_join_ = LineJoinBevel
  }

withTestEnv :: (ChartBackend m) => m a -> m a
withTestEnv m = withFillStyle (solidFillStyle $ opaque white) $ fillClip >> m

withCenterRot :: (ChartBackend m) => Double -> Int -> Int -> m a -> m a
withCenterRot a x y m = 
  withTranslation (Point (fromIntegral x) 
                         (fromIntegral y)) $ withRotation a $ m

tests :: (ChartBackend m) => [(String, Int, Int, m ())]
tests = [ ("lines", 500, 500, testLines)
        , ("arcs" , 500, 500, testArcs)
        , ("text" , 500, 500, testText)
        , ("fill" , 500, 500, testFill)
        , ("clip" , 500, 500, testClip)
        ] ++ ( (flip map) testEnvironments 
               $ \(name, env) -> ("environment-" ++ name, 500, 500, env)
             )

testClip :: (ChartBackend m) => m ()
testClip = withTestEnv $ do
  withFillStyle (solidFillStyle $ opaque blue) fillClip
  withClipRegion (Rect (Point 100 100) (Point 300 300)) $ do
    withFillStyle (solidFillStyle $ opaque green) fillClip
    withClipRegion (Rect (Point 200 200) (Point 400 400)) $ do
      withFillStyle (solidFillStyle $ withOpacity red 0.5) fillClip
  withClipRegion (Rect (Point 150 50) (Point 400 150)) $ do
    withFillStyle (solidFillStyle $ opaque red) fillClip

testFill :: (ChartBackend m) => m ()
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
    fillPath $ arcNeg' 125 400 75 0 (1.5 * pi)
            <> lineTo' 125 400

testEnvironments :: (ChartBackend m) => [(String, m ())]
testEnvironments =
  let envs = [ ("fill", withFillStyle $ solidFillStyle $ opaque green)
             , ("font", withFontStyle $ def { font_color_ = opaque red })
             , ("line", withLineStyle $ def { line_color_ = opaque blue, line_width_ = 10 })
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
    

testText :: (ChartBackend m) => m ()
testText = withTestEnv $ do
  drawText (Point 10 50) "No Scale"
  withTranslation (Point 10 70) $ do
    withScale (Vector 2 1) $ drawText (Point 0 0) "Horz. Scale"
  withTranslation (Point 10 100) $ do
    withScale (Vector 1 3) $ drawText (Point 0 0) "Vert. Scale"
  
  drawText (Point 150 50) "Size * 1"
  withFontStyle (def { font_size_ = font_size_ def * 2 }) $ drawText (Point 150 70) "Size * 2"
  withFontStyle (def { font_size_ = font_size_ def * 3 }) $ drawText (Point 150 100) "Size * 3"
  
  (flip mapM_) ([ 10, 12, 14 ] `zip` [0..]) $ \(size, n) -> do
    withFontStyle (def { font_weight_ = FontWeightNormal, font_size_ = size }) $ 
      drawText (Point 10 $ 120 + n * 20) "Normal Weight"
    withFontStyle (def { font_weight_ = FontWeightBold, font_size_ = size   }) $ 
      drawText (Point 150 $ 120 + n * 20) "Bold Weight"
    
    withFontStyle (def { font_slant_ = FontSlantNormal, font_size_ = size }) $ 
      drawText (Point 10 $ 180 + n * 20) "Normal Slant"
    withFontStyle (def { font_slant_ = FontSlantItalic, font_size_ = size }) $ 
      drawText (Point 150 $ 180 + n * 20) "Italic Slant"
    withFontStyle (def { font_slant_ = FontSlantOblique, font_size_ = size }) $ 
      drawText (Point 290 $ 180 + n * 20) "Oblique Slant"
  
  
  (flip mapM_) ([ 10, 12, 14 ] `zip` [0..]) $ \(size, n) -> do
    withFontStyle (def { font_name_ = "sans-serif", font_size_ = size }) $ 
      drawText (Point 10 $ 240 + n * 20) "Sans-Serif"
    withFontStyle (def { font_name_ = "serif", font_size_ = size }) $ 
      drawText (Point 150 $ 240 + n * 20) "Serif"
    withFontStyle (def { font_name_ = "monospace", font_size_ = size }) $ 
      drawText (Point 290 $ 240 + n * 20) "Monospace"
  
  (flip mapM_) ([ (opaque red, 12)
                , (withOpacity blue 0.3, 14)
                , (opaque green, 17) ] `zip` [0..] ) $ \((cl, size), n) -> do
    withFontStyle (def { font_color_ = cl, font_size_ = size }) $
      drawText (Point (10 + (n * 140)) 300) "Colored"
  
  (flip mapM_) [0..7] $ \n -> do
    withTranslation (Point 250 400) $ 
      withRotation (n * 0.125 * 2 * pi) $ 
        drawText (Point 30 0) $ show n
  -- Support Lines
  withLineStyle supportLineStyle $ do
    strokePath $ moveTo' 0 400 <> lineTo' 500 400
    strokePath $ moveTo' 250 300 <> lineTo' 250 500

testArcs :: (ChartBackend m) => m ()
testArcs = withTestEnv $ do
  (flip mapM_) ( [ def { line_cap_ = LineCapButt  , line_join_ = LineJoinMiter, line_width_ = 10 }
                 , def { line_cap_ = LineCapRound , line_join_ = LineJoinRound, line_width_ = 10 }
                 , def { line_cap_ = LineCapSquare, line_join_ = LineJoinBevel, line_width_ = 10 }
                 , def { line_width_ = 10 }
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

testLines :: (ChartBackend m) => m ()
testLines = withTestEnv $ do
  -- Test Line Caps
  (flip mapM_) [ (def { line_cap_ = LineCapButt  , line_width_ = 10 }, 0)
               , (def { line_cap_ = LineCapRound , line_width_ = 10 }, 20)
               , (def { line_cap_ = LineCapSquare, line_width_ = 10 }, 40)
               ] $ \(ls, offset) -> do
    withLineStyle ls $ do
      strokePath $ moveTo' 125 (10 + offset)
                <> lineTo' 375 (10 + offset)
  -- Test Line Joins
  (flip mapM_) [ (def { line_join_ = LineJoinMiter, line_width_ = 10 }, 0)
               , (def { line_join_ = LineJoinRound, line_width_ = 10 }, 60)
               , (def { line_join_ = LineJoinBevel, line_width_ = 10 }, 120)
               ] $ \(ls, offset) -> do
    withLineStyle ls $ do
      strokePath $ moveTo' 125 (70  + offset)
                <> lineTo' 375 (70  + offset)
                <> lineTo' 250 (110 + offset)
                <> close
  -- Test Line Color & Width
  (flip mapM_) [ (def { line_color_ = opaque blue, line_width_ = 1 }, 0)
               , (def { line_color_ = opaque green, line_width_ = 5 }, 10)
               , (def { line_color_ = opaque magenta, line_width_ = 20 }, 35)
               , (def { line_color_ = withOpacity cyan 0.2, line_width_ = 10 }, 60)
               , (def { line_color_ = withOpacity cyan 0.4, line_width_ = 10 }, 80)
               , (def { line_color_ = withOpacity cyan 0.6, line_width_ = 10 }, 100)
               , (def { line_color_ = withOpacity cyan 0.8, line_width_ = 10 }, 120)
               , (def { line_color_ = withOpacity cyan 1.0, line_width_ = 10 }, 140)
               ] $ \(ls, offset) -> do
    withLineStyle ls $ do
      strokePath $ moveTo' 10  (250 + offset)
                <> lineTo' 490 (250 + offset)
  -- Test Line Dashes
  (flip mapM_) [ (def { line_dashes_ = []  , line_width_ = 10 }, 0)
               , (def { line_dashes_ = [10] , line_width_ = 10 }, 20)
               , (def { line_dashes_ = [1,5,2], line_width_ = 10 }, 40)
               , (def { line_dashes_ = [1,5,2,3,4,6,7], line_width_ = 10 }, 60)
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