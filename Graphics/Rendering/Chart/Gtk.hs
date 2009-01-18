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
import Data.List (isPrefixOf)

-- do action m for any keypress (except meta keys)
anyKey :: (Monad m) => m a -> G.Event -> m Bool
anyKey m (G.Key {G.eventKeyName=key})
    | any (`isPrefixOf` key) ignores = return True
    | otherwise                      = m >> return True
  where ignores = ["Shift","Control","Alt",
                   "Super","Meta","Hyper"]

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
    G.onKeyPress window $ anyKey (G.widgetDestroy window)
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
