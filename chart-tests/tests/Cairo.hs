
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

import System.Environment(getArgs)

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
main1 ("--pdf":tests) = renderTestsToFiles PDF ".pdf" tests
main1 ("--svg":tests) = renderTestsToFiles SVG ".svg" tests
main1 ("--ps":tests) = renderTestsToFiles PS ".ps" tests
main1 ("--png":tests) = renderTestsToFiles PNG ".png" tests
main1 tests = renderTestsToFiles PNG ".png" tests

renderTestsToFiles :: FileFormat -> String -> [String] -> IO ()
renderTestsToFiles fmt suffix tests = mapM_ doTest (getTests tests)
  where
    doTest (n,sz,rf) = do
      let f = n++suffix
      putStrLn (f ++ "...")              
      void $ renderableToFile (FileOptions sz fmt) (rf (chooseLineWidth fmt)) f
