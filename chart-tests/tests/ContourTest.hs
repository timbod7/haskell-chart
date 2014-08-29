import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Plot.Contour

import Control.Lens
import Control.Applicative
import Data.Default.Class

--- Test ---

main :: IO ()
main = do
    let f x y = 0.2 * x + 0.2 * y + sin x + sin y
        sz = 7.5
        stp = 100
        rng = (-sz,sz)
        stps = (stp,stp)
        n = 20
        plt =
            plot_contours_styles .~ (solidLine 3 <$> rgbaGradient (0,0,1,1) (1,0,0,1) n)
            $ plot_contours_title .~ "Contour"
            $ contourPlot rng rng stp stp n f
        lyt = toRenderable
            $ layout_title .~ "Contours of a 2D Sin Curve"
            $ layout_plots .~ [toPlot plt]
            $ def
    renderableToFile (FileOptions (1000,700) PNG) "sind2d.png" lyt
    return ()
