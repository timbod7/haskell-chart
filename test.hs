import Graphics.Rendering.Cairo
import Chart

main :: IO ()
main =
    withImageSurface FormatARGB32 width height $ \result -> do
    renderWith result $ chart
    surfaceWriteToPNG result "chart.png"

  where
    width = 640
    height = 480
    dwidth = fromIntegral width
    dheight = fromIntegral height

    chart = do
        fixCoordinates
	save
	setSourceRGB 1 1 1
	paint
	restore
	render layout (Rect (Point 0 0) (Point dwidth dheight))

    fixCoordinates = do
        -- move to centre of pixels so that stroke width of 1 is
        -- exactly one pixel 
        translate 0.5 0.5

    axisLineStyle = do
        setLineWidth 1.0
	setSourceRGB 0 0 0

    plotLineStyle = do
        setLineWidth 1.0
	setSourceRGB 0 0 1.0

    xaxis = Axis {
        axis_viewport=(0,360),
	axis_line_style=CairoLineStyle axisLineStyle,
	axis_label_style=CairoFontStyle (return ()),
	axis_ticks=[(v,10) | v <- [0,90,180,270,360]],
	axis_labels=[(0.0,"0"),(180,"180"),(360,"360")],
	axis_label_gap=10
    }

    yaxis = Axis {
        axis_viewport=(-1.2,1.2),
	axis_line_style=CairoLineStyle axisLineStyle,
	axis_label_style=CairoFontStyle (return ()),
	axis_ticks=[(v,10) | v <- [-1,-0.5,0,0.5,1]],
	axis_labels=[(-1,"-1.0"),(0,"0.0"),(1.0,"1.0")],
	axis_label_gap=10
    }

    sinusoid = Plot {
        plot_line_style=Just (CairoLineStyle plotLineStyle),
	plot_point_style=Nothing,
	plot_values = [ (Point x (sin (x*3.14159/180))) | x <- [0,5..360]]
    }
	

    layout = emptyLayout1 {
        layout1_bottom_axis=Just xaxis,
	layout1_left_axis=Just yaxis,
	layout1_right_axis=Just yaxis,
	layout1_top_axis=Nothing,
	layout1_plots = [(HA_Bottom,VA_Left,sinusoid)]
    }

    asize = 40
    barect = Rect (Point asize 0) (Point (dwidth-asize) asize)
    larect = Rect (Point 0 asize) (Point asize (dheight-asize))
	      
