import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Control.Lens
import System.Environment(getArgs)

chart = toRenderable layout
  where
    vals :: [(Double,Double,Double,Double)]
    vals = [ (x,sin (exp x),sin x/2,cos x/10) | x <- [1..20]]
    bars = plot_errbars_values .~ [symErrPoint x y dx dy | (x,y,dx,dy) <- vals]
         $ plot_errbars_title .~"test"
         $ def

    points = plot_points_style .~ filledCircles 2 (opaque red)
	   $ plot_points_values .~ [(x,y) |  (x,y,dx,dy) <- vals]
           $ plot_points_title .~ "test data"
           $ def

    layout = layout_title .~ "Error Bars"
           $ layout_plots .~ [toPlot bars, toPlot points]
           $ def

main = renderableToFile def chart "test10_big.png"
