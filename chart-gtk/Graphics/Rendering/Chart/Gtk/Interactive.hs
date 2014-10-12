-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Gtk.Interactive
-- Copyright   :  (c) Bryce Anderson 2014
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Gtk.Interactive (
  createZoomableWindow,
  updateCanvas
) where


import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as GE
import qualified Graphics.UI.Gtk.Selectors.FileChooserDialog as FC
import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk.Gdk.GC

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Renderable

import Data.IORef
import Data.Default.Class

-- | bits for generating/modifying/querying the state of zoom
data ZoomState t =  ZoomState { mouse_pos :: Maybe Point
                              , left_press :: Maybe Point
                              , press_complete :: Maybe (Point,Point)
                              , drag_last :: Maybe Point
                              , ts :: [t] }

-- | Remove a zoom from the stack
popZoom :: ZoomState a -> ZoomState a
popZoom h@(ZoomState { ts = [_] }) = h
popZoom h@(ZoomState { ts = (t:ts') }) = zoomZero ts'


zoomZero :: [a] -> ZoomState a
zoomZero ts = ZoomState { mouse_pos = Nothing
                        , left_press = Nothing
                        , press_complete = Nothing
                        , drag_last = Nothing
                        , ts = ts }

-------------------------------------------------------------

zoomTransform :: RenderablePlus z a => ZoomState z -> Range -> PickFn a -> ZoomState z
zoomTransform tss r f = panTransform (clear ts') r f
  where
    ZoomState { ts = z:_ } = tss
    ts' = maybe tss id transformed
    clear tss = tss { press_complete = Nothing }
    transformed = do
      ps <- press_complete tss
      z' <- transformt ps r f z
      return $ tss { ts = z':(ts tss) }

panTransform :: RenderablePlus z a => ZoomState z -> Range -> PickFn a -> ZoomState z
panTransform tss r f = result
  where
    ZoomState { ts = z:t } = tss
    result = maybe tss id transformed
    transformed = do
      p1 <- drag_last tss
      p2 <- mouse_pos tss
      let z' = maybe z id $ dragTransform (p1,p2) r f z
      return $ tss { drag_last = Just p2, ts = z':t }

-----------------------------------------------------------

createZoomableWindow :: RenderablePlus z a => z -> Int -> Int -> IO G.Window
createZoomableWindow z windowWidth windowHeight = do
    zooms <- newIORef $ zoomZero [z]
    pickfn <- newIORef nullPickFn :: IO(IORef (PickFn a) )
    window <- G.windowNew
    vbox <- G.vBoxNew False 0
    canvas <- G.drawingAreaNew
    menu <- makeMenu window canvas zooms pickfn z
    G.widgetSetSizeRequest canvas windowWidth windowHeight
    G.boxPackStart vbox menu G.PackNatural 0
    G.boxPackEnd vbox canvas G.PackGrow 0
    G.set window [G.containerChild G.:= vbox]
    G.widgetSetSizeRequest canvas windowWidth windowHeight
    G.onExpose canvas $ const $ do
      zs <- readIORef zooms
      range <- dsize canvas
      fn <- readIORef pickfn
      let zs'@ ZoomState { ts =z':_ } = zoomTransform zs range fn
      -- updates the canvas and saves the new PickFn
      fn' <- _updateCanvas False (buildRenderable z') canvas
      writeIORef pickfn fn'
      writeIORef zooms zs'
      drawMouseBox zs' canvas
      G.widgetGetDrawWindow canvas >>= G.drawWindowEndPaint -- manually finish canvas
      return True

    G.onButtonPress canvas (onButtonEvent zooms canvas)
    G.onButtonRelease canvas (onButtonEvent zooms canvas)
    G.onMotionNotify canvas True (mouseMotion zooms canvas)
    return window

-- | Draws the mouse selection box
drawMouseBox :: ZoomState a -> G.DrawingArea -> IO Bool
drawMouseBox zoom canvas = do
  case (mouse_pos zoom, left_press zoom) of
    (Just (Point x y), Just (Point x' y')) -> do
      let x'' = round $ min x x'
          y'' = round $ min y y'
          width = round $ abs (x - x')
          height = round $ abs (y - y')

      win <- G.widgetGetDrawWindow canvas
      gc <- gcNew win
      G.drawRectangle win gc False x'' y'' width height
      return True

    -- All other cases we dont draw
    _ -> return True

-- | Saves the mouse position to draw the selection rectangle
mouseMotion :: IORef (ZoomState a) -> G.DrawingArea -> GE.Event -> IO Bool
mouseMotion zooms canvas GE.Motion { GE.eventX = x1, GE.eventY = y1 } = do
  zs <- readIORef zooms
  case (left_press zs, drag_last zs) of
    (Just _, _) -> do
      (w,h) <- dsize canvas
      writeIORef zooms $ zs { mouse_pos = Just $ Point x1 y1 }
      G.widgetQueueDraw canvas
      return True

    (_, Just _) -> do
      writeIORef zooms $ zs { mouse_pos = Just $ Point x1 y1 }
      G.widgetQueueDraw canvas
      return True

    _ -> return True


makeMenu :: RenderablePlus z a => G.Window -> G.DrawingArea -> IORef (ZoomState z) ->
                       IORef (PickFn a) -> z ->  IO G.MenuBar
makeMenu window canvas ref pickfn z = do
  -- File menu
  menuBar <- G.menuBarNew
  filemenu <- G.menuItemNewWithMnemonic "_File"
  G.set menuBar [G.containerChild G.:= filemenu]
  fmenu <- G.menuNew
  filemenu `G.menuItemSetSubmenu` fmenu
  save <- G.menuItemNewWithMnemonic "_Save"
  save `G.on` G.menuItemActivate $ do
    chooser <- FC.fileChooserDialogNew Nothing (Just window)
                          G.FileChooserActionSave
                          [("Save",G.ResponseAccept),("Cancel",G.ResponseCancel)]
    G.fileChooserSetDoOverwriteConfirmation chooser True
    result <- G.dialogRun chooser
    case result of
      G.ResponseCancel -> return ()
      G.ResponseAccept -> do
        m <- G.fileChooserGetFilename chooser
        case m of
          Nothing -> return ()
          Just p  -> do
            zs <- readIORef ref
            sz <- dsize canvas
            fn <- readIORef pickfn
            let ZoomState { ts = r:_ } = zoomTransform zs sz fn
            renderableToFile def p $ buildRenderable r
            return ()
      _ -> return () -- shouldn't get here.

    G.widgetDestroy chooser

  quit <- G.menuItemNewWithMnemonic "_Quit"
  G.on quit G.menuItemActivate G.mainQuit
  G.set fmenu [G.containerChild G.:= save, G.containerChild G.:= quit]

  -- Chart menu
  chartmenu <- G.menuItemNewWithMnemonic "_Chart"
  G.set menuBar [G.containerChild G.:= chartmenu]
  cmenu <- G.menuNew
  chartmenu `G.menuItemSetSubmenu` cmenu
  reset <- G.menuItemNewWithMnemonic "_Reset"
  reset `G.on` G.menuItemActivate $ do
    writeIORef ref (zoomZero [z])
    G.widgetQueueDraw canvas

  G.set cmenu [G.containerChild G.:= reset]

  return menuBar


-- Handles the button events
onButtonEvent :: IORef (ZoomState a) -> G.DrawingArea -> GE.Event -> IO Bool
onButtonEvent ref canvas (e@GE.Button { GE.eventClick = GE.SingleClick,
                                        GE.eventButton = GE.LeftButton }) = do
  (w,h) <- dsize canvas
  modifyIORef ref $ \z -> z { left_press = Just (Point (GE.eventX e) (GE.eventY e)) }

  G.widgetQueueDraw canvas
  return True

onButtonEvent ref canvas (e@GE.Button { GE.eventClick = GE.ReleaseClick,
                                        GE.eventButton = GE.LeftButton }) = do
  (w,h) <- dsize canvas
  r <- onButtonRelease ref $ Point (GE.eventX e) (GE.eventY e)
  G.widgetQueueDraw canvas
  return r

onButtonEvent ref canvas (GE.Button { GE.eventClick = GE.SingleClick,
                                      GE.eventButton = GE.RightButton }) = do
  modifyIORef ref popZoom
  G.widgetQueueDraw canvas
  return True

-- | Pan click
onButtonEvent ref canvas (e@GE.Button { GE.eventClick = GE.SingleClick,
                                        GE.eventButton = GE.MiddleButton }) = do
  (w,h) <- dsize canvas
  let x = GE.eventX e
      y = GE.eventY e
  modifyIORef ref $ \z -> z { drag_last = Just (Point x y) }
  return True

-- | Pan release
onButtonEvent ref _ (GE.Button{ GE.eventClick = GE.ReleaseClick,
                                       GE.eventButton = GE.MiddleButton }) = do
  modifyIORef ref $ \z -> z { drag_last = Nothing, mouse_pos = Nothing }
  return True

-- | Other mouse events
onButtonEvent _ _ _ = return False
-- End of button events

-- | get the dimentions of the canvas as a pair of Double's
dsize :: G.WidgetClass w => w -> IO (Double, Double)
dsize canvas = do
  (w, h) <- G.widgetGetSize canvas
  return (fromIntegral w, fromIntegral h)

onButtonRelease :: IORef (ZoomState a) -> Point -> IO Bool
onButtonRelease ref p1@(Point x y) = readIORef ref >>= go
  where
    go (ZoomState { left_press = Nothing }) = return True
    go (ZoomState { left_press = Just p2@(Point x' y'), ts = ts' }) = do
      let zs = (zoomZero ts') { press_complete = t }
          t  = if dx < 1.0 || dy < 1.0 then Nothing else Just (p1,p2)
          dx = abs (x - x')
          dy = abs (y - y')

      writeIORef ref zs
      return True

updateCanvas :: Renderable a -> G.DrawingArea -> IO Bool
updateCanvas r d = _updateCanvas True r d >> return True

_updateCanvas :: Bool -> Renderable a -> G.DrawingArea -> IO (PickFn a)
_updateCanvas finish chart canvas = do
    win <- G.widgetGetDrawWindow canvas
    (width, height) <- G.widgetGetSize canvas
    regio <- G.regionRectangle $ GE.Rectangle 0 0 width height
    let sz = (fromIntegral width,fromIntegral height)
    G.drawWindowBeginPaintRegion win regio
    a <- G.renderWithDrawable win $ runBackend (defaultEnv bitmapAlignmentFns) (render chart sz)
    if finish
      then G.drawWindowEndPaint win >> return a
      else return a

