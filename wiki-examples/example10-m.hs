import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

vals :: [(Double,Double,Double,Double)]
vals = [ (x,sin (exp x),sin x/2,cos x/10) | x <- [1..20]]

errbars title vals = liftEC $ do
  c <- takeColor
  plot_errbars_values .= [symErrPoint x y dx dy | (x,y,dx,dy) <- vals]
  plot_errbars_title .= title
  plot_errbars_line_style . line_color .= c

main = toFile def "example10_big.png" $ do
    setColors (map opaque [blue,red])
    layout_title .= "Error Bars"
    plot $ errbars "" vals
    plot $ points "test data" [(x,y) |  (x,y,dx,dy) <- vals]
  
