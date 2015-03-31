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
        stp = 1000
        rng = (-sz,sz)
        stps = (stp,stp)
        n = 20
        plts = contourPlot rng rng stp stp n f
        stls = solidLine 3 <$> rgbaGradient (0,0,1,1) (1,0,0,1) n
        plts' = zipWith (plot_lines_style .~) stls plts
        lyt = toRenderable
            $ layout_title .~ "Contours of a 2D Sin Curve"
            $ layout_plots .~ map toPlot plts'
            $ def
    renderableToFile (FileOptions (1000,700) PNG) "sind2d.png" lyt
    return ()
