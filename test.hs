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
	save
	setSourceRGB 1 1 1
	paint
	restore
	render layout (Rect (Point 0 0) (Point dwidth dheight))

    fixYCoordinates = do
        translate 0 dheight
	scale 1 (-1)

    lineStyle = do
        setLineWidth 1.0
	setSourceRGB 0 0 1.0

    axis = Axis {
        axis_viewport=Rect (Point 0 0) (Point 1.0 1.0),
	axis_line_style=CairoLineStyle lineStyle,
	axis_label_style=CairoFontStyle (return ()),
	axis_ticks=[(0.0,10),(0.25,10),(0.50,10),(0.75,10),(1.0,10)],
	axis_labels=[(0.0,"0.00"),(0.5,"0.50"),(1.0,"1.00")],
	axis_label_gap=10
    }

    layout = emptyLayout1 {
        layout1_bottom_axis=Just axis,
	layout1_left_axis=Just axis,
	layout1_right_axis=Just axis,
	layout1_top_axis=Just axis
    }

    asize = 40
    barect = Rect (Point asize 0) (Point (dwidth-asize) asize)
    larect = Rect (Point 0 asize) (Point asize (dheight-asize))
	      
