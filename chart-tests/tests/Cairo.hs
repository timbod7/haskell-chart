

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

import System.Environment(getArgs)

import Tests

main = do
    args <- getArgs
    main1 args

main1 :: [String] -> IO ()
main1 ("--pdf":tests) = showTests tests renderToPDF
main1 ("--svg":tests) = showTests tests renderToSVG
main1 ("--ps":tests) = showTests tests renderToPS
main1 ("--png":tests) = showTests tests renderToPNG
main1 tests = showTests tests renderToPNG

renderToPNG (n,(w,h),ir) = renderableToPNGFile (ir PNG) w h (n ++ ".png")
                           >> return ()
renderToPS  (n,(w,h),ir) = renderableToPSFile (ir PS) w h (n ++ ".ps")
renderToPDF (n,(w,h),ir) = renderableToPDFFile (ir PDF) w h (n ++ ".pdf")
renderToSVG (n,(w,h),ir) = renderableToSVGFile (ir SVG) w h (n ++ ".svg")

