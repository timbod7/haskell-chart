module GtkChart where

import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import Chart

showChartInWindow :: Renderable a => a -> Int -> Int -> IO ()
showChartInWindow chart windowWidth windowHeight = do
    G.initGUI
    window <- G.windowNew
    canvas <- G.drawingAreaNew
    -- fix size
    --   G.windowSetResizable window False
    G.widgetSetSizeRequest window windowWidth windowHeight
    -- press any key to quit
    G.onKeyPress window $ const (do G.widgetDestroy window; return True)
    G.onDestroy window G.mainQuit
    G.onExpose canvas $ const (updateCanvas chart canvas)
    G.set window [G.containerChild G.:= canvas]
    G.widgetShowAll window
    G.mainGUI

updateCanvas :: Chart.Renderable a => a -> G.DrawingArea  -> IO Bool
updateCanvas chart canvas = do
    win <- G.drawingAreaGetDrawWindow canvas
    (width, height) <- G.drawingAreaGetSize canvas
    let rect = Rect (Point 0 0) (Point (fromIntegral width) (fromIntegral height))
    G.renderWithDrawable win (rfn rect)
    return True
  where
    rfn rect = do
        -- move to centre of pixels so that stroke width of 1 is
        -- exactly one pixel 
        C.translate 0.5 0.5
	 
        -- paint background white
        C.save
	C.setSourceRGB 1 1 1
	C.paint
	C.restore

	render chart rect
