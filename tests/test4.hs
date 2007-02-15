import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk

chart = layout 
  where
    decades = [1000,100,10,1]
    vmin = minimum decades
    vmax = maximum decades

    logAxis = defaultAxis {
	axis_viewport = viewfn,
	axis_ticks  = ((head decades),5):
          [ (v*m,5) | v <- [1,2,3,4,5,6,7,8,9], m <- (tail decades) ],
	axis_labels = [ (v*m,show (v*m)) | v <- [1], m <- decades ],
	axis_grid   = [ (v*m) | v <- [1], m <- decades ]
    }

    viewfn (dmin, dmax) v = dmin + log v * (dmax - dmin) / (log vmax - log vmin)

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
	layout1_vertical_axes=linkedAxes (explicitAxis (Just logAxis)),
	layout1_plots = [("values",HA_Bottom,VA_Left,(toPlot points)),
			 ("values",HA_Bottom,VA_Left,(toPlot lines)) ]
    }
	      
main = do
    renderableToWindow (toRenderable chart) 640 480
