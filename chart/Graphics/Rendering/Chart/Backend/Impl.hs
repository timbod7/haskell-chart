
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module provides the implementation details common to all 'ChartBackend's.
module Graphics.Rendering.Chart.Backend.Impl where

import Data.Monoid
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Operational

import Data.Default

import Data.Colour
import Data.Colour.Names

import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Backend.Types

-- -----------------------------------------------------------------------
-- Rendering Backend Environment
-- -----------------------------------------------------------------------

-- | The environment present in the 'ChartBackend'.
data ChartBackendEnv = ChartBackendEnv
  { cbePointAlignFn :: Point -> Point
  -- ^ An adjustment applied immediately prior to points
  --   being displayed in device coordinates.
  --
  --   When device coordinates correspond to pixels, a cleaner
  --   image is created if this transform rounds to the nearest
  --   pixel. With higher-resolution output, this transform can
  --   just be the identity function.

  -- | A adjustment applied immediately prior to coordinates
  --   being transformed.
  , cbeCoordAlignFn :: Point -> Point
  
  -- | The current transformation.
  , cbeTransform :: Matrix
  
  -- | The current 'FontStyle'.
  , cbeFontStyle :: FontStyle
  
  -- | The current 'FillStyle'.
  , cbeFillStyle :: FillStyle
  
  -- | The current 'LineStyle'.
  , cbeLineStyle :: LineStyle
  
  -- | The current clip region, if there is one.
  , cbeClipRegion :: Limit Rect
  }

-- | Produce a environment with no transformation and clipping. 
--   It will use the default styles.
defaultEnv :: (Point -> Point) -- ^ The point alignment function ('cbePointAlignFn')
           -> (Point -> Point) -- ^ The coordinate alignment function ('cbeCoordAlignFn')
           -> ChartBackendEnv
defaultEnv pointAlignFn coordAlignFn = ChartBackendEnv 
  { cbePointAlignFn = pointAlignFn
  , cbeCoordAlignFn = coordAlignFn
  , cbeTransform = identity 
  , cbeFontStyle = def
  , cbeFillStyle = def
  , cbeLineStyle = def
  , cbeClipRegion = LMax
  }

-- -----------------------------------------------------------------------
-- Rendering Backend Class
-- -----------------------------------------------------------------------

data Change a = Change { oldValue :: a, newValue :: a, diffValue :: a }

data ChartBackendInstr m a where
  StrokePath :: ChartBackendEnv -> Path -> ChartBackendInstr m ()
  FillPath   :: ChartBackendEnv -> Path -> ChartBackendInstr m ()
  FillClip   :: ChartBackendEnv -> ChartBackendInstr m ()
  GetTextSize :: ChartBackendEnv -> String -> ChartBackendInstr m TextSize
  DrawText    :: ChartBackendEnv -> Point -> String -> ChartBackendInstr m ()
  WithTransform  :: ChartBackendEnv -> Change Matrix       -> m a -> ChartBackendInstr m a
  WithFontStyle  :: ChartBackendEnv -> Change FontStyle    -> m a -> ChartBackendInstr m a
  WithFillStyle  :: ChartBackendEnv -> Change FillStyle    -> m a -> ChartBackendInstr m a
  WithLineStyle  :: ChartBackendEnv -> Change LineStyle    -> m a -> ChartBackendInstr m a
  WithClipRegion :: ChartBackendEnv -> Change (Limit Rect) -> m a -> ChartBackendInstr m a

type ChartProgram a = ReaderT ChartBackendEnv
                              (Program (ChartBackendInstr ChartBackend)) a

-- | A 'ChartBackend' provides the capability to render a chart somewhere.
--   
--   The coordinate system of the backend has its initial origin (0,0)
--   in the top left corner of the drawing plane. The x-axis points 
--   towards the top right corner and the y-axis points towards 
--   the bottom left corner. The unit used by coordinates, the font size,
--   and lengths is the always the same, but depends on the backend.
--   All angles are measured in radians.
--   
newtype ChartBackend a = ChartBackend {
  toProgram :: ChartProgram a
}

instance Monad ChartBackend where
  (>>=) (ChartBackend ma) f = ChartBackend $ ma >>= toProgram . f
  return = ChartBackend . return

instance MonadReader ChartBackendEnv ChartBackend where
  ask = ChartBackend $ ask
  local f ma = ChartBackend $ (toProgram ma) >>= \a -> local f (return a)
  reader f = ChartBackend $ reader f

instance Functor ChartBackend where
  fmap f ma = ChartBackend $ fmap f (toProgram ma)

instance Applicative ChartBackend where
  pure = ChartBackend . pure
  (<*>) f m = ChartBackend $ (toProgram f) <*> (toProgram m)

chartSingleton :: ChartBackendInstr ChartBackend a -> ChartBackend a
chartSingleton = ChartBackend . lift . singleton


-- | Stroke the outline of the given path using the 
--   current 'LineStyle'. This function does /not/ perform
--   alignment operations on the path.
strokePath :: Path -> ChartBackend ()
strokePath p = do
  env <- ask
  chartSingleton $ StrokePath env p

-- | Fill the given path using the current 'FillStyle'.
--   The given path will be closed prior to filling.
--   This function does /not/ perform
--   alignment operations on the path.
fillPath :: Path -> ChartBackend ()
fillPath p = do
  env <- ask
  chartSingleton $ FillPath env p

-- | Fill the clip region using the current 'FillStyle'.
fillClip :: ChartBackend ()
fillClip = do
  env <- ask
  chartSingleton $ FillClip env

-- | Calculate a 'TextSize' object with rendering information
--   about the given string without actually rendering it.
textSize :: String -> ChartBackend TextSize
textSize text = do
  env <- ask
  chartSingleton $ GetTextSize env text

-- | Draw a single-line textual label anchored by the baseline (vertical) 
--   left (horizontal) point. Uses the current 'FontStyle' for drawing.
drawText :: Point -> String -> ChartBackend ()
drawText p text = do
  env <- ask
  chartSingleton $ DrawText env p text

-- | Apply the given transformation in this local
--   environment when drawing. The given transformation 
--   is applied after the current transformation. This
--   means both are combined.
withTransform :: Matrix -> ChartBackend a -> ChartBackend a
withTransform t m = do
  oldTrans <- getTransform
  let newTrans = t * oldTrans
  env <- (\s -> s { cbeTransform = newTrans }) <$> ask
  chartSingleton $ WithTransform env (Change oldTrans newTrans t)
                 $ local (const env) m

-- | Use the given font style in this local
--   environment when drawing text.
--   
--   An implementing backend is expected to guarentee
--   to support the following font families: @serif@, @sans-serif@ and @monospace@;
--   
--   If the backend is not able to find or load a given font 
--   it is required to fall back to a custom fail-safe font
--   and use it instead.
withFontStyle :: FontStyle -> ChartBackend a -> ChartBackend a
withFontStyle fs m = do
  oldEnv <- ask
  let newEnv = oldEnv { cbeFontStyle = fs }
  chartSingleton $ WithFontStyle newEnv (Change (cbeFontStyle oldEnv) fs fs)
                 $ local (const newEnv) m

-- | Use the given fill style in this local
--   environment when filling paths.
withFillStyle :: FillStyle -> ChartBackend a -> ChartBackend a
withFillStyle fs m = do
  oldEnv <- ask
  let newEnv = oldEnv { cbeFillStyle = fs }
  chartSingleton $ WithFillStyle newEnv (Change (cbeFillStyle oldEnv) fs fs)
                 $ local (const newEnv) m

-- | Use the given line style in this local
--   environment when stroking paths.
withLineStyle :: LineStyle -> ChartBackend a -> ChartBackend a
withLineStyle ls m = do
  oldEnv <- ask
  let newEnv = oldEnv { cbeLineStyle = ls }
  chartSingleton $ WithLineStyle newEnv (Change (cbeLineStyle oldEnv) ls ls)
                 $ local (const newEnv) m

-- | Use the given clipping rectangle when drawing
--   in this local environment. The new clipping region
--   is intersected with the given clip region. You cannot 
--   escape the clip!
withClipRegion :: Rect -> ChartBackend a -> ChartBackend a
withClipRegion c m = do
  oldClip <- getClipRegion
  let newClip = intersectRect oldClip (LValue c)
  env <- (\s -> s { cbeClipRegion = newClip }) <$> ask
  chartSingleton $ WithClipRegion env (Change oldClip newClip (LValue c))
                 $ local (const env) m

-- -----------------------------------------------------------------------
-- Rendering Utility Functions
-- -----------------------------------------------------------------------

-- | Get the current transformation.
getTransform :: ChartBackend Matrix
getTransform = liftM cbeTransform ask

-- | Get the current font style.
getFontStyle :: ChartBackend FontStyle
getFontStyle = liftM cbeFontStyle ask

-- | Get the current fill style.
getFillStyle :: ChartBackend FillStyle
getFillStyle = liftM cbeFillStyle ask

-- | Get the current line style.
getLineStyle :: ChartBackend LineStyle
getLineStyle = liftM cbeLineStyle ask

-- | Get the current clipping region.
--   If no clipping region was set (it is an infinite plane) 
--   'Nothing' is returned.
getClipRegion :: ChartBackend (Limit Rect)
getClipRegion = liftM cbeClipRegion ask

-- | Get the pointAlignmentFunction
getPointAlignFn :: ChartBackend (Point->Point)
getPointAlignFn = liftM cbePointAlignFn ask

-- | Get the coordinate alignment function
getCoordAlignFn :: ChartBackend (Point->Point)
getCoordAlignFn = liftM cbeCoordAlignFn ask


-- | Run the backend monad to get the program instance with all 
--   instructions.
runChartBackend :: ChartBackendEnv 
                -> ChartBackend a 
                -> Program (ChartBackendInstr ChartBackend) a
runChartBackend env m = runReaderT (toProgram m) env

-- | Helper to implement a backend using a custom monoid. This may be useful
--   for pure backends.
--   Implement each effect in form of a function and this will wire everything 
--   together in the right order.
compileBackend :: forall m a. (Monoid m)
  => (ChartBackendEnv -> Path -> m)            -- ^ 'strokePath' operation
  -> (ChartBackendEnv -> Path -> m)            -- ^ 'fillPath' operation
  -> (ChartBackendEnv -> m)                    -- ^ 'fillClip' operation
  -> (ChartBackendEnv -> String -> (m, TextSize))    -- ^ 'textSize' operation
  -> (ChartBackendEnv -> Point -> String -> m) -- ^ 'drawText' operation
  -> (ChartBackendEnv -> Change Matrix -> m -> m) 
     -- ^ 'withTransform' operation. The given 
     --   transformation is the complete transformation, 
     --   not just the next one to apply.
  -> (ChartBackendEnv -> Change LineStyle -> m -> m) -- ^ 'withLineStyle' operation
  -> (ChartBackendEnv -> Change FillStyle -> m -> m) -- ^ 'withFillStyle' operation
  -> (ChartBackendEnv -> Change FontStyle -> m -> m) -- ^ 'withFontStyle' operation
  -> (ChartBackendEnv -> Change (Limit Rect) -> m -> m) -- ^ 'withClipRegion' operation
  -> ChartBackendEnv -> ChartBackend a -> (m, a)
compileBackend
      strokePath' fillPath' fillClip' textSize' drawText' 
      withTransform' withLineStyle' withFillStyle' withFontStyle' withClipRegion'
      e m = eval e $ runChartBackend e m
  where
    compile :: forall x. ChartBackendEnv -> ChartBackend x -> (m, x)
    compile = compileBackend strokePath' fillPath' fillClip' textSize' drawText' 
                             withTransform'  withLineStyle' withFillStyle' 
                             withFontStyle' withClipRegion'
    eval env m = case view m of
      Return x -> (mempty, x)
      (StrokePath env' p) :>>= k ->
        let (m, x) = eval env $ k ()
        in (strokePath' env' p <> m, x)
      (FillPath env' p) :>>= k ->
        let (m, x) = eval env $ k ()
        in (fillPath' env' p <> m, x)
      (FillClip env') :>>= k ->
        let (m, x) = eval env $ k ()
        in (fillClip' env' <> m, x)
      (GetTextSize fs text) :>>= k ->
        let (m1, ts) = textSize' fs text
            (m2, x) = eval env $ k ts
        in (m1 <> m2, x)
      (DrawText env' p text) :>>= k ->
        let (m, x) = eval env $ k ()
        in (drawText' env' p text <> m, x)
      (WithTransform env' c m) :>>= k ->
        let (ma, x) = compile env' m
            m1 = withTransform' env' c ma
            (m2, x') = eval env $ k x
        in (m1 <> m2, x')
      (WithLineStyle env' c m) :>>= k ->
        let (ma, x) = compile env' m
            m1 = withLineStyle' env' c ma
            (m2, x') = eval env $ k x
        in (m1 <> m2, x')
      (WithFillStyle env' c m) :>>= k ->
        let (ma, x) = compile env' m
            m1 = withFillStyle' env' c ma
            (m2, x') = eval env $ k x
        in (m1 <> m2, x')
      (WithFontStyle env' c m) :>>= k ->
        let (ma, x) = compile env' m
            m1 = withFontStyle' env' c ma
            (m2, x') = eval env $ k x
        in (m1 <> m2, x')
      (WithClipRegion env' c m) :>>= k ->
        let (ma, x) = compile env' m
            m1 = withClipRegion' env' c ma
            (m2, x') = eval env $ k x
        in (m1 <> m2, x')

-- | Helper to implement a backend using a custom effectful monad.
--   Implement each effect in form of a function and this will wire everything 
--   together.
compileBackendM :: forall m a. (Monad m) 
  => (ChartBackendEnv -> Path -> m ())            -- ^ 'strokePath' operation
  -> (ChartBackendEnv -> Path -> m ())            -- ^ 'fillPath' operation
  -> (ChartBackendEnv -> m ())                    -- ^ 'fillClip' operation
  -> (ChartBackendEnv -> String -> m TextSize)    -- ^ 'textSize' operation
  -> (ChartBackendEnv -> Point -> String -> m ()) -- ^ 'drawText' operation
  -> (forall b. ChartBackendEnv -> Change Matrix -> m b -> m b) 
     -- ^ 'withTransform' operation. The given 
     --   transformation is the complete transformation, 
     --   not just the next one to apply.
  -> (forall c. ChartBackendEnv -> Change LineStyle -> m c -> m c) -- ^ 'withLineStyle' operation
  -> (forall d. ChartBackendEnv -> Change FillStyle -> m d -> m d) -- ^ 'withFillStyle' operation
  -> (forall e. ChartBackendEnv -> Change FontStyle -> m e -> m e) -- ^ 'withFontStyle' operation
  -> (forall f. ChartBackendEnv -> Change (Limit Rect) -> m f -> m f) -- ^ 'withClipRegion' operation
  -> ChartBackendEnv -> ChartBackend a -> m a
compileBackendM 
      strokePath' fillPath' fillClip' textSize' drawText' 
      withTransform' withLineStyle' withFillStyle' withFontStyle' withClipRegion'
      e m = eval e $ runChartBackend e m
  where
    compile :: ChartBackendEnv -> ChartBackend x -> m x
    compile = compileBackendM strokePath' fillPath' fillClip' textSize' drawText' 
                              withTransform'  withLineStyle' withFillStyle' 
                              withFontStyle' withClipRegion'
    eval env m = case view m of
      Return x -> return x
      (StrokePath env' p) :>>= k -> do
        strokePath' env' p
        eval env $ k ()
      (FillPath env' p) :>>= k -> do
        fillPath' env' p
        eval env $ k ()
      (FillClip env') :>>= k -> do
        fillClip' env'
        eval env $ k ()
      (GetTextSize env' text) :>>= k -> do
        ts <- textSize' env' text
        eval env $ k ts
      (DrawText env' p text) :>>= k -> do
        drawText' env' p text
        eval env $ k ()
      (WithTransform env' c m) :>>= k -> do
        x <- withTransform' env' c $ compile env' m
        eval env $ k x

      (WithLineStyle env' c m) :>>= k -> do
        x <- withLineStyle' env' c $ compile env' m
        eval env $ k x
      (WithFillStyle env' c m) :>>= k -> do
        x <- withFillStyle' env' c $ compile env' m
        eval env $ k x
      (WithFontStyle env' c m) :>>= k -> do
        x <- withFontStyle' env' c $ compile env' m
        eval env $ k x
      (WithClipRegion env' c m) :>>= k -> do
        x <- withClipRegion' env' c $ compile env' m
        eval env $ k x
