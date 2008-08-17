-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Gtk
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Gtk(
    renderableToWindow
    ) where

import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Types

renderableToWindow :: Renderable a -> Int -> Int -> IO ()
renderableToWindow chart windowWidth windowHeight = do
    G.unsafeInitGUIForThreadedRTS
    -- G.initGUI
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

updateCanvas :: Renderable a -> G.DrawingArea  -> IO Bool
updateCanvas chart canvas = do
    win <- G.widgetGetDrawWindow canvas
    (width, height) <- G.widgetGetSize canvas
    let sz = (fromIntegral width,fromIntegral height)
    G.renderWithDrawable win $ runCRender (render chart sz) bitmapEnv
    return True
