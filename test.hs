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
        fixYCoordinates
	save
	setSourceRGB 1 1 1
	paint
	restore
	render layout (Rect (Point 0 0) (Point dwidth dheight))

    fixYCoordinates = do
        translate 0 dheight
	scale 1 (-1)

    baxis = Axis {
        axis_viewport=Rect (Point 0 0) (Point 1.0 1.0),
	axis_type=AS_Bottom,
	axis_line_style=CairoLineStyle (return ()),
	axis_label_style=CairoFontStyle (return ()),
	axis_ticks=[(0.0,10),(0.25,10),(0.50,10),(0.75,10),(1.0,10)],
	axis_labels=[],
	axis_label_gap=1
    }

    laxis = baxis { axis_type=AS_Left }
    raxis = baxis { axis_type=AS_Top }
    taxis = baxis { axis_type=AS_Top }

    layout = emptyLayout1 {
        layout1_bottom_axis=Just baxis,
	layout1_left_axis=Just laxis,
	layout1_right_axis=Just raxis,
	layout1_top_axis=Just taxis
    }

    asize = 40
    barect = Rect (Point asize 0) (Point (dwidth-asize) asize)
    larect = Rect (Point 0 asize) (Point asize (dheight-asize))
	      
