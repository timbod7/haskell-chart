
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
  => (LineStyle -> Path -> m)            -- ^ 'strokePath' operation
  -> (FillStyle -> Path -> m)            -- ^ 'fillPath' operation
  -> (FillStyle -> m)                    -- ^ 'fillClip' operation
  -> (FontStyle -> String -> (m, TextSize))    -- ^ 'textSize' operation
  -> (FontStyle -> Point -> String -> m) -- ^ 'drawText' operation
  -> (Matrix     -> m -> m) -- ^ 'withTransform' operation. The given 
                                --   transformation is the complete transformation, 
                                --   not just the next one to apply.
  -> (LineStyle  -> m -> m) -- ^ 'withLineStyle' operation
  -> (FillStyle  -> m -> m) -- ^ 'withFillStyle' operation
  -> (FontStyle  -> m -> m) -- ^ 'withFontStyle' operation
  -> (Limit Rect -> m -> m) -- ^ 'withClipRegion' operation
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
      (StrokePath ls p) :>>= k ->
        let (m, x) = eval env $ k ()
        in (strokePath' ls p <> m, x)
      (FillPath fs p) :>>= k ->
        let (m, x) = eval env $ k ()
        in (fillPath' fs p <> m, x)
      (FillClip fs) :>>= k ->
        let (m, x) = eval env $ k ()
        in (fillClip' fs <> m, x)
      (GetTextSize fs text) :>>= k ->
        let (m1, ts) = textSize' fs text
            (m2, x) = eval env $ k ts
        in (m1 <> m2, x)
      (DrawText fs p text) :>>= k ->
        let (m, x) = eval env $ k ()
        in (drawText' fs p text <> m, x)
      (WithTransform env' t m) :>>= k ->
        let (ma, x) = compile env' m
            m1 = withTransform' t ma
            (m2, x') = eval env $ k x
        in (m1 <> m2, x')
      (WithLineStyle env' ls m) :>>= k ->
        let (ma, x) = compile env' m
            m1 = withLineStyle' ls ma
            (m2, x') = eval env $ k x
        in (m1 <> m2, x')
      (WithFillStyle env' fs m) :>>= k ->
        let (ma, x) = compile env' m
            m1 = withFillStyle' fs ma
            (m2, x') = eval env $ k x
        in (m1 <> m2, x')
      (WithFontStyle env' fs m) :>>= k ->
        let (ma, x) = compile env' m
            m1 = withFontStyle' fs ma
            (m2, x') = eval env $ k x
        in (m1 <> m2, x')
      (WithClipRegion env' clip m) :>>= k ->
        let (ma, x) = compile env' m
            m1 = withClipRegion' clip ma
            (m2, x') = eval env $ k x
        in (m1 <> m2, x')

-- | Helper to implement a backend using a custom effectful monad.
--   Implement each effect in form of a function and this will wire everything 
--   together.
compileBackendM :: forall m a. (Monad m) 
  => (LineStyle -> Path -> m ())            -- ^ 'strokePath' operation
  -> (FillStyle -> Path -> m ())            -- ^ 'fillPath' operation
  -> (FillStyle -> m ())                    -- ^ 'fillClip' operation
  -> (FontStyle -> String -> m TextSize)    -- ^ 'textSize' operation
  -> (FontStyle -> Point -> String -> m ()) -- ^ 'drawText' operation
  -> (forall b. Matrix -> m b -> m b) -- ^ 'withTransform' operation. The given 
                                      --   transformation is the complete transformation, 
                                      --   not just the next one to apply.
  -> (forall c. LineStyle  -> m c -> m c) -- ^ 'withLineStyle' operation
  -> (forall d. FillStyle  -> m d -> m d) -- ^ 'withFillStyle' operation
  -> (forall e. FontStyle  -> m e -> m e) -- ^ 'withFontStyle' operation
  -> (forall f. Limit Rect -> m f -> m f) -- ^ 'withClipRegion' operation
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
      (StrokePath ls p) :>>= k -> do
        strokePath' ls p
        eval env $ k ()
      (FillPath fs p) :>>= k -> do
        fillPath' fs p
        eval env $ k ()
      (FillClip fs) :>>= k -> do
        fillClip' fs
        eval env $ k ()
      (GetTextSize fs text) :>>= k -> do
        ts <- textSize' fs text
        eval env $ k ts
      (DrawText fs p text) :>>= k -> do
        drawText' fs p text
        eval env $ k ()
      (WithTransform env' t m) :>>= k -> do
        x <- withTransform' t $ compile env' m
        eval env $ k x

      (WithLineStyle env' ls m) :>>= k -> do
        x <- withLineStyle' ls $ compile env' m
        eval env $ k x
      (WithFillStyle env' fs m) :>>= k -> do
        x <- withFillStyle' fs $ compile env' m
        eval env $ k x
      (WithFontStyle env' fs m) :>>= k -> do
        x <- withFontStyle' fs $ compile env' m
        eval env $ k x
      (WithClipRegion env' clip m) :>>= k -> do
        x <- withClipRegion' clip $ compile env' m
        eval env $ k x
