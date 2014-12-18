import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens
import Control.Applicative
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import qualified Prelude
import Prelude hiding ((^))

(^) :: Num a => a -> Integer -> a
(^) = (Prelude.^)

chart :: Renderable ()
chart = toRenderable layout
  where
    add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
    square a s = [(x,y) | x <- range, y <- range]
      where range = [-a,-a+s..a] :: [Double]
    r' x y z = sqrt $ x^2 + y^2 + z^2
    efield sign x y = ( sign*x/r,sign*y/r) where r = r' x y 10
    bfield sign x y = (-sign*y/r^2,sign*x/r^2) where r = r' x y 10
    eplot = plot_vectors_mapf .~ (\(x,y) ->
                efield 1 (x-20) y `add` efield (-1) (x+20) y)
          $ plot_vectors_grid  .~ square 30 3
          $ plot_vectors_style . vector_line_style . line_color .~ opaque black
          $ plot_vectors_style . vector_head_style . point_color .~ opaque black
          $ plot_vectors_title .~ "Electric field"
          $ def
    bplot = plot_vectors_mapf .~ (\(x,y) ->
                bfield 1 (x-20) y `add` bfield (-1) (x+20) y)
          $ plot_vectors_grid  .~ square 30 5
          $ plot_vectors_style . vector_line_style . line_color .~ opaque blue
          $ plot_vectors_style . vector_head_style . point_color .~ opaque blue
          $ plot_vectors_title .~ "B-field"
          $ def
    layout = layout_title .~ "Positive and Negative Charges"
           $ layout_plots .~ (plotVectorField <$> [eplot,bplot])
           $ def

main = renderableToFile def "example12_big.png" chart
