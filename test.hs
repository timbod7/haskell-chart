import qualified Graphics.Rendering.Cairo as C
import Chart
import GtkChart

chart = layout 
  where
    xaxis = defaultAxis {
        axis_viewport=(0,360),
	axis_ticks=[(v,10) | v <- [0,90,180,270,360]],
	axis_labels=[(0.0,"0"),(180,"180"),(360,"360")]
    }

    yaxis = defaultAxis {
        axis_viewport=(-1.2,1.2),
	axis_ticks=[(v,10) | v <- [-1,-0.5,0,0.5,1]],
	axis_labels=[(-1,"-1.0"),(0,"0.0"),(1.0,"1.0")]
    }

    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

    sinusoid1 = defaultPlotLines {
	plot_lines_values = [[ (Point x (am x)) | x <- [0,(0.5)..360]]]
    }

    sinusoid2 = defaultPlotPoints {
        plot_points_style=filledCircles 2 1 0 0,
	plot_points_values = [ (Point x (am x)) | x <- [0,7..360]]
    }

    layout = defaultLayout1 {
        layout1_title="Amplitude Modulation",			   
        layout1_horizontal_axes=linkedAxes (autoScaledAxis xaxis),
	layout1_vertical_axes=linkedAxes (autoScaledAxis yaxis),
	layout1_plots = [("am",HA_Bottom,VA_Left,(PLines sinusoid1)),
			 ("am points", HA_Bottom,VA_Left,(PPoints sinusoid2))]
    }
	      
main = do
    renderableToWindow chart 640 480
    renderableToPNGFile chart 640 480 "chart.png"