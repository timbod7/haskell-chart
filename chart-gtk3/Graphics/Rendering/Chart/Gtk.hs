-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Gtk
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Gtk (
    renderableToWindow,
    toWindow,
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
import Graphics.Rendering.Chart.State(EC, execEC)

import Data.List (isPrefixOf)
import Data.IORef
import Data.Default.Class

import Control.Monad(when)
import System.IO.Unsafe(unsafePerformIO)


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
-- its first call, but not subsequent calls. Hence it's 
-- unlikely to be compatible with other code using gtk. In 
-- that case use createRenderableWindow.
renderableToWindow :: Renderable a -> Int -> Int -> IO ()
renderableToWindow chart windowWidth windowHeight = do
    initGuiOnce
    window <- createRenderableWindow chart windowWidth windowHeight
    -- press any key to exit the loop
    window `G.on` G.keyPressEvent $ do
                     C.liftIO (G.widgetDestroy window)
                     return True
    window `G.on` G.objectDestroy $ G.mainQuit
    G.widgetShowAll window
    G.mainGUI

-- | Generate a new GTK window from the state content of
-- an EC computation. The state may have any type that is
-- an instance of `ToRenderable`
toWindow :: (Default r, ToRenderable r) =>Int -> Int -> EC r () -> IO ()
toWindow windowWidth windowHeight ec = renderableToWindow r windowWidth windowHeight where
                       r = toRenderable (execEC ec)

-- | Create a new GTK window displaying a renderable.
createRenderableWindow :: Renderable a -> Int -> Int -> IO G.Window
createRenderableWindow chart windowWidth windowHeight = do
    window <- G.windowNew
    canvas <- G.drawingAreaNew
    G.widgetSetSizeRequest window windowWidth windowHeight
    canvas `G.on` G.draw $ do
      C.liftIO $ updateCanvas chart canvas
      return ()

    G.set window [G.containerChild G.:= canvas]
    return window


updateCanvas :: Renderable a -> G.DrawingArea  -> IO Bool
updateCanvas chart canvas = do
    mwin <- G.widgetGetWindow canvas
    case mwin of
      Nothing -> return False
      Just win -> do
        width  <- C.liftIO $ G.widgetGetAllocatedWidth canvas
        height <- C.liftIO $ G.widgetGetAllocatedHeight canvas
        putStrLn $ "Width = " ++ show width ++ "; Height = " ++ show height
        let rect = GE.Rectangle 0 0 width height
        let sz   = (fromIntegral width, fromIntegral height)
        G.drawWindowBeginPaintRect win rect
        G.renderWithDrawWindow win
          $ runBackend (defaultEnv bitmapAlignmentFns) (render chart sz)
        G.drawWindowEndPaint win
        return True
