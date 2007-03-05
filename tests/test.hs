import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk

chart = layout 
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

    sinusoid1 = defaultPlotLines {
	plot_lines_values = [[ (Point x (am x)) | x <- [0,(0.5)..400]]]
    }

    sinusoid2 = defaultPlotPoints {
        plot_points_style=filledCircles 2 1 0 0,
	plot_points_values = [ (Point x (am x)) | x <- [0,7..400]]
    }

    layout = defaultLayout1 {
        layout1_title="Amplitude Modulation",			   
        layout1_horizontal_axes=linkedAxes (autoScaledAxis defaultAxis),
	layout1_vertical_axes=linkedAxes (autoScaledAxis defaultAxis),
	layout1_plots = [("am",HA_Bottom,VA_Left,(toPlot sinusoid1)),
			 ("am points", HA_Bottom,VA_Left,(toPlot sinusoid2))]
    }
	      
main = do
    renderableToWindow (toRenderable chart) 640 480
    renderableToPNGFile (toRenderable chart) 640 480 "test.png"