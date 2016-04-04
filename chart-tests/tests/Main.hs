module Main where


import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

import System.Environment(getArgs,getProgName)
import qualified GtkTestPicking
import qualified DiagramsCairo
import qualified DiagramsSVG
import qualified DiagramsEPS
import qualified CompareFonts
import qualified Drawing.Cairo
import qualified Drawing.DiagramsCairo

import Control.Monad
import Tests

chooseLineWidth PNG = 1.0
chooseLineWidth PDF = 0.25
chooseLineWidth PS = 0.25
chooseLineWidth SVG = 0.25

main = do
    args <- getArgs
    main1 args

main1 :: [String] -> IO ()
main1 ("charts-cairo":"--pdf":tests) = renderTestsToFiles PDF ".pdf" tests
main1 ("charts-cairo":"--svg":tests) = renderTestsToFiles SVG ".svg" tests
main1 ("charts-cairo":"--ps":tests) = renderTestsToFiles PS ".ps" tests
main1 ("charts-cairo":"--png":tests) = renderTestsToFiles PNG ".png" tests

main1 ("charts-diagrams":"--cairo":args) = DiagramsCairo.main1 args
-- main1 ("charts-diagrams":"--svg":args) = DiagramsSVG.main1 args
-- main1 ("charts-diagrams":"--eps":args) = DiagramsEPS.main1 args

main1 ("drawing-cairo":[]) = Drawing.Cairo.main
main1 ("drawing-diagrams":[]) = Drawing.DiagramsCairo.main

main1 ("compare-fonts":[]) = CompareFonts.main
main1 ("gtk-picking":[]) = GtkTestPicking.main

main1 _ = usage

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn "Usage:"
  putStrLn $ "  " ++ progname ++ " charts-cairo (--pdf|--svg|--ps|--png) chart-name..."
  putStrLn $ "  " ++ progname ++ " charts-diagrams (--cairo|--svg|--eps) chart-name..."
  putStrLn $ "  " ++ progname ++ " drawing-cairo"
  putStrLn $ "  " ++ progname ++ " drawing-diagrams"
  putStrLn $ "  " ++ progname ++ " compare-fonts"
  putStrLn $ "  " ++ progname ++ " gtk-picking"

chartsCairo :: [String] -> IO ()
chartsCairo ("--pdf":tests) = renderTestsToFiles PDF ".pdf" tests
chartsCairo ("--svg":tests) = renderTestsToFiles SVG ".svg" tests
chartsCairo ("--ps":tests) = renderTestsToFiles PS ".ps" tests
chartsCairo ("--png":tests) = renderTestsToFiles PNG ".png" tests
chartsCairo (tests) = renderTestsToFiles PNG ".png" tests

renderTestsToFiles :: FileFormat -> String -> [String] -> IO ()
renderTestsToFiles fmt suffix tests = mapM_ doTest (getTests tests)
  where
    doTest (n,sz,rf) = do
      let f = n++suffix
      putStrLn (f ++ "...")
      void $ renderableToFile (FileOptions sz fmt) f (rf (chooseLineWidth fmt))
