module Graphics.Rendering.Chart.Gtk(
    renderableToWindow
    ) where

import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Renderable

renderableToWindow :: Renderable -> Int -> Int -> IO ()
renderableToWindow chart windowWidth windowHeight = do
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

updateCanvas :: Renderable -> G.DrawingArea  -> IO Bool
updateCanvas chart canvas = do
    win <- G.drawingAreaGetDrawWindow canvas
    (width, height) <- G.drawingAreaGetSize canvas
    let rect = Rect (Point 0 0) (Point (fromIntegral width) (fromIntegral height))
    G.renderWithDrawable win (rfn rect)
    return True
  where
    rfn rect = do
        setupRender
	render chart rect
