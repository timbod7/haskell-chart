
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides the utilities to implement 'ChartBackend's.
module Graphics.Rendering.Chart.Backend.Utils
  ( runChartBackend
  , compileBackend
  , compileBackendM
  ) where

import Data.Monoid

import Control.Monad.Reader
import Control.Monad.Operational

import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Geometry

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
