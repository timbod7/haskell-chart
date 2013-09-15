-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Gtk
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Gtk(
    renderableToWindow,
    createRenderableWindow,
    updateCanvas
    ) where

import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as GE
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Backend.Cairo
import Data.List (isPrefixOf)
import Data.IORef
import Control.Monad(when)
import System.IO.Unsafe(unsafePerformIO)

-- do action m for any keypress (except meta keys)
anyKey :: (Monad m) => m a -> GE.Event -> m Bool
anyKey m (GE.Key {GE.eventKeyName=key})
    | any (`isPrefixOf` key) ignores = return True
    | otherwise                      = m >> return True
  where ignores = ["Shift","Control","Alt",
                   "Super","Meta","Hyper"]

-- Yuck. But we really want the convenience function
-- renderableToWindow as to be callable without requiring
-- initGUI to be called first. But newer versions of
-- gtk insist that initGUI is only called once
guiInitVar :: IORef Bool
{-# NOINLINE guiInitVar #-}
guiInitVar = unsafePerformIO (newIORef False)

initGuiOnce :: IO ()
initGuiOnce = do
    v <- readIORef guiInitVar
    when (not v) $ do
        -- G.initGUI
        G.unsafeInitGUIForThreadedRTS
        writeIORef guiInitVar True

-- | Display a renderable in a gtk window.
--
-- Note that this is a convenience function that initialises GTK on
-- it's first call, but not subsequent calls. Hence it's 
-- unlikely to be compatible with other code using gtk. In 
-- that case use createRenderableWindow.
renderableToWindow :: Renderable a -> Int -> Int -> IO ()
renderableToWindow chart windowWidth windowHeight = do
    initGuiOnce
    window <- createRenderableWindow chart windowWidth windowHeight
    -- press any key to exit the loop
    G.onKeyPress window $ anyKey (G.widgetDestroy window)
    G.onDestroy window G.mainQuit
    G.widgetShowAll window
    G.mainGUI

-- | Create a new GTK window displaying a renderable.
createRenderableWindow :: Renderable a -> Int -> Int -> IO G.Window
createRenderableWindow chart windowWidth windowHeight = do
    window <- G.windowNew
    canvas <- G.drawingAreaNew
    G.widgetSetSizeRequest window windowWidth windowHeight
    G.onExpose canvas $ const (updateCanvas chart canvas)
    G.set window [G.containerChild G.:= canvas]
    return window


updateCanvas :: Renderable a -> G.DrawingArea  -> IO Bool
updateCanvas chart canvas = do
    win <- G.widgetGetDrawWindow canvas
    (width, height) <- G.widgetGetSize canvas
    regio <- G.regionRectangle $ GE.Rectangle 0 0 width height
    let sz = (fromIntegral width,fromIntegral height)
    G.drawWindowBeginPaintRegion win regio
    G.renderWithDrawable win $ runBackend (defaultEnv bitmapAlignmentFns) (render chart sz) 
    G.drawWindowEndPaint win
    return True
