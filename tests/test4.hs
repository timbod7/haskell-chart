import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk

chart = layout 
  where

    points = defaultPlotPoints {
        plot_points_style=filledCircles 3 1 0 0,
	plot_points_values = [ Point x (10**x) | x <- [0.5,1,1.5,2,2.5] ]
    }

    lines = defaultPlotLines {
	plot_lines_values = [ [Point x (10**x) | x <- [0,3]] ]
    }

    layout = defaultLayout1 {
        layout1_title="Log/Linear Example",			   
        layout1_horizontal_axes=linkedAxes (autoScaledAxis defaultAxis),
	layout1_vertical_axes=linkedAxes (autoScaledLogAxis defaultAxis),
	layout1_plots = [("values",HA_Bottom,VA_Left,(toPlot points)),
			 ("values",HA_Bottom,VA_Left,(toPlot lines)) ]
    }
	      
main = do
    renderableToWindow (toRenderable chart) 640 480
