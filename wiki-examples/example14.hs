import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Graphics.Rendering.Chart.Backend.Diagrams
import Control.Lens
import Data.Ratio
import Data.Char.Small

-- Parameterization of an elliptic curve
-- y^2 = x^2 * (x+1)
-- y/x=t => x=t^2+1, y = t * (t^2+1)
elliptic :: Double -> (Double, Double)
elliptic t = (t*t - 1 , t*( t*t - 1 ))

chart = toRenderable layout
  where
    layout = layout_title .~ "Rational Points on Elliptic Curve "
                             ++ "y"++[superscript '2'] ++"=x" ++ [superscript '3'] ++ "+x"
           $ layout_title_style . font_size .~ 20
           $ layout_plots .~ [ toPlot curve
                             , toPlot points
                             , toPlot labels
                             , toPlot rationalLine
                             , toPlot slopeLabel
                             , toPlot comment]
           $ layout_x_axis . laxis_generate .~ scaledAxis def (-1.5,4)
           $ def

    curve = plot_lines_values .~ [[elliptic t | t <- [-2.05,-2..2.05]]]
              $ plot_lines_style  . line_color .~ opaque blue
              $ plot_lines_style . line_width .~ 2
              $ plot_lines_title .~ "Elliptic curve"
              $ def

    points = plot_points_style .~ filledCircles 2 (opaque red)
            $ plot_points_values .~ map fst someRationalPoints
            $ plot_points_title .~ "Some rational points"
            $ def

    someRationalPoints = [ (elliptic t, "(" ++ showRational ((p%q) * (p%q) -1) ++ "," ++ showRational ((p%q) * ((p%q) * (p%q) -1))++")")
                         | p <- [-3..3], q <- [1..2::Integer], gcd p q ==1, let t = fromInteger p / fromInteger q, abs t < 2.1]

    showRational :: Rational -> String
    showRational r = let p = numerator r; q=denominator r
                     in if q==1 then show p else show p ++ "/" ++ show q

    labels = plot_annotation_values .~ map (\((x,y),str) -> (x,y,str)) someRationalPoints
           $ plot_annotation_hanchor .~ HTA_Centre
           $ plot_annotation_vanchor .~ VTA_Bottom
           $ plot_annotation_offset .~ Vector 0 (-10)
           $ def

    rationalLine = plot_lines_values .~ [[ (x,-2*x) | x <- [-1.05,-1..3.25]]]
                 $ plot_lines_style  . line_color .~ opaque purple
                 $ plot_lines_title .~ "A line with a rational slope t"
                 $ def

    slopeLabel  = plot_annotation_values .~ [(-1.0,2.0,"slope t")]
                $ plot_annotation_angle .~ 35
                $ plot_annotation_vanchor .~ VTA_Bottom
                $ def

    comment  = plot_annotation_values .~  [(-0.8,-4.0,parameterization)]
                $ plot_annotation_vanchor .~ VTA_Bottom
                $ plot_annotation_style . font_size .~ 18
                $ plot_annotation_hanchor .~ HTA_Left
                $ plot_annotation_vanchor .~ VTA_Centre
                $ def
    parameterization = "x=t"++[superscript '2']++"-1"
                                           ++"\n"
                                           ++"y=t(t"++[superscript '2']++"-1)"

main = renderableToFile def "example_elliptic.svg" chart