module Main where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

import System.Environment (getArgs, getProgName)
import qualified GtkTestPicking
import qualified DiagramsTests
import qualified CompareFonts
import qualified Drawing.Cairo
import qualified Drawing.DiagramsCairo

import Control.Monad (void, mapM_)
import Tests

chooseLineWidth :: FileFormat -> Double
chooseLineWidth PNG = 1.0
chooseLineWidth PDF = 0.25
chooseLineWidth PS = 0.25
chooseLineWidth SVG = 0.25

main = do
    args <- getArgs
    run args

run :: [String] -> IO ()
run ("charts-cairo":"--pdf":tests) = renderTestsToFiles PDF ".pdf" tests
run ("charts-cairo":"--svg":tests) = renderTestsToFiles SVG ".svg" tests
run ("charts-cairo":"--ps":tests)  = renderTestsToFiles PS  ".ps"  tests
run ("charts-cairo":"--png":tests) = renderTestsToFiles PNG ".png" tests

run ("charts-diagrams":"--cairo":args) = DiagramsTests.run DiagramsTests.BackendCairo
run ("charts-diagrams":"--svg":args) = DiagramsTests.run DiagramsTests.BackendSVG
run ("charts-diagrams":"--svg-embedded":args) = DiagramsTests.run DiagramsTests.BackendSVGEmbedded
run ("charts-diagrams":"--eps":args) = DiagramsTests.run DiagramsTests.BackendEPS

run ("drawing-cairo":[]) = Drawing.Cairo.main
run ("drawing-diagrams":[]) = Drawing.DiagramsCairo.main

run ("compare-fonts":[]) = CompareFonts.main
run ("gtk-picking":[]) = GtkTestPicking.main

run _ = usage

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn "Usage:"
  mapM_ (\s -> putStrLn (" " ++ progName ++ " " ++ s))
    [ "charts-cairo (--pdf|--svg|--ps|--png) chart-name..."
    , "charts-diagrams (--cairo|--svg|--svg-embedded|--eps) chart-name..."
    , "drawing-cairo"
    , "drawing-diagrams"
    , "compare-fonts"
    , "gtk-picking"
    ]

renderTestsToFiles :: FileFormat -> String -> [String] -> IO ()
renderTestsToFiles fmt suffix tests = mapM_ doTest (getTests tests)
  where
    doTest :: (String, (Int, Int), LineWidth -> Renderable ()) -> IO ()
    doTest (n, size, renderable) = do
      let filename = n ++ suffix
      putStrLn (filename ++ "...")
      void $ renderableToFile (FileOptions size fmt) filename (renderable (chooseLineWidth fmt))
