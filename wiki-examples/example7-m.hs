import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

circle :: [(Double,Double)]
circle = [ (r a * sin (a*dr),r a * cos (a*dr)) | a <- [0,0.5..360::Double] ]
  where
    dr = 2 * pi / 360
    r a = 0.8 * cos (a * 20 * pi /360)

main = toFile def "example7_big.png" $ do
    layout_title .= "Parametric Plot"
    plot (line "" [circle])
  
