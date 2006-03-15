import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import Chart

windowWidth = 640
windowHeight = 480

main = do
    G.initGUI
    window <- G.windowNew
    canvas <- G.drawingAreaNew
    -- fix size
    --   G.windowSetResizable window False
    G.widgetSetSizeRequest window windowWidth windowHeight
    -- press any key to quit
    G.onKeyPress window $ const (do G.widgetDestroy window; return True)
    G.onDestroy window G.mainQuit
    G.onExpose canvas $ const (updateCanvas canvas)
    G.set window [G.containerChild G.:= canvas]
    G.widgetShowAll window
    G.mainGUI

updateCanvas :: G.DrawingArea -> IO Bool
updateCanvas canvas = do
    win <- G.drawingAreaGetDrawWindow canvas
    (width, height) <- G.drawingAreaGetSize canvas
    G.renderWithDrawable win (chart width height)
    return True

chart :: Int -> Int -> C.Render ()
chart width height = do
    fixCoordinates
    C.save
    C.setSourceRGB 1 1 1
    C.paint
    C.restore
    render layout (Rect (Point 0 0) (Point dwidth dheight))
  where
    dwidth = fromIntegral width
    dheight = fromIntegral height


    fixCoordinates = do
        -- move to centre of pixels so that stroke width of 1 is
        -- exactly one pixel 
        C.translate 0.5 0.5

    axisLineStyle = solidLine 1 0 0 0

    plotLineStyle = solidLine 1 0 0 1

    plotPointStyle = filledCircles 2 1 0 0

    xaxis = Axis {
        axis_viewport=(0,360),
	axis_line_style=axisLineStyle,
	axis_label_style=CairoFontStyle (return ()),
	axis_ticks=[(v,10) | v <- [0,90,180,270,360]],
	axis_labels=[(0.0,"0"),(180,"180"),(360,"360")],
	axis_label_gap=10
    }

    yaxis = Axis {
        axis_viewport=(-1.2,1.2),
	axis_line_style=axisLineStyle,
	axis_label_style=CairoFontStyle (return ()),
	axis_ticks=[(v,10) | v <- [-1,-0.5,0,0.5,1]],
	axis_labels=[(-1,"-1.0"),(0,"0.0"),(1.0,"1.0")],
	axis_label_gap=10
    }

    sinusoid1 = PlotLines {
        plot_lines_style=plotLineStyle,
	plot_lines_values = [ (Point x (sin (x*3.14159/180))) | x <- [0,5..360]]
    }

    sinusoid2 = PlotPoints {
        plot_points_style=plotPointStyle,
	plot_points_values = [ (Point x (sin (x*3.14159/180))) | x <- [0,45..360]]
    }

    layout = emptyLayout1 {
        layout1_bottom_axis=Just xaxis,
	layout1_left_axis=Just yaxis,
	layout1_right_axis=Just yaxis,
	layout1_top_axis=Nothing,
	layout1_plots = [(HA_Bottom,VA_Left,(PLines sinusoid1)),
			 (HA_Bottom,VA_Left,(PPoints sinusoid2))]
    }

    asize = 40
    barect = Rect (Point asize 0) (Point (dwidth-asize) asize)
    larect = Rect (Point 0 asize) (Point asize (dheight-asize))
	      
