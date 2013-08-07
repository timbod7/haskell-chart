{-# LANGUAGE GADTs #-}

-- | This module provides the implementation details common to all 'ChartBackend's.
module Graphics.Rendering.Chart.Backend.Impl where

import Data.Monoid
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Operational

import Data.Default.Class

import Data.Colour
import Data.Colour.Names

import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Backend.Types

-- -----------------------------------------------------------------------
-- Rendering Backend Class
-- -----------------------------------------------------------------------

-- | The abstract drawing operation generated when using the
--   the chart drawing API.
--   
--   See the documentation of the different function for the correct semantics
--   of each instruction:
--   
--   * 'strokePath', 'fillPath'
--   
--   * 'drawText', 'textSize'
--   
--   * 'getPointAlignFn', 'getCoordAlignFn', 'AlignmentFns'
--   
--   * 'withTransform', 'withClipRegion'
--   
--   * 'withLineStyle', 'withFillStyle', 'withFontStyle'
--   
data ChartBackendInstr a where
  StrokePath :: Path -> ChartBackendInstr ()
  FillPath   :: Path -> ChartBackendInstr ()
  GetTextSize :: String -> ChartBackendInstr TextSize
  DrawText    :: Point -> String -> ChartBackendInstr ()
  GetAlignments :: ChartBackendInstr AlignmentFns
  WithTransform  :: Matrix ->  Program ChartBackendInstr a -> ChartBackendInstr a
  WithFontStyle  :: FontStyle -> Program ChartBackendInstr a -> ChartBackendInstr a
  WithFillStyle  :: FillStyle -> Program ChartBackendInstr a -> ChartBackendInstr a
  WithLineStyle  :: LineStyle -> Program ChartBackendInstr a -> ChartBackendInstr a
  WithClipRegion :: Rect -> Program ChartBackendInstr a -> ChartBackendInstr a

-- | A 'ChartBackend' provides the capability to render a chart somewhere.
--   
--   The coordinate system of the backend has its initial origin (0,0)
--   in the top left corner of the drawing plane. The x-axis points 
--   towards the top right corner and the y-axis points towards 
--   the bottom left corner. The unit used by coordinates, the font size,
--   and lengths is the always the same, but depends on the backend.
--   All angles are measured in radians.
--   
--   The line, fill and font style are set to their default values 
--   initially.
--   
--   Information about the semantics of the instructions can be 
--   found in the documentation of 'ChartBackendInstr'.
type ChartBackend a = Program ChartBackendInstr a

{-# DEPRECATED CRender  "Use the new name ChartBackend!" #-}
-- | Alias so the old name for rendering code still works.
type CRender a = ChartBackend a

-- | Stroke the outline of the given path using the 
--   current 'LineStyle'. This function does /not/ perform
--   alignment operations on the path. See 'Path' for the exact semantic
--   of paths.
strokePath :: Path -> ChartBackend ()
strokePath p = singleton (StrokePath p)

-- | Fill the given path using the current 'FillStyle'.
--   The given path will be closed prior to filling.
--   This function does /not/ perform
--   alignment operations on the path.
--   See 'Path' for the exact semantic of paths.
fillPath :: Path -> ChartBackend ()
fillPath p = singleton (FillPath p)

-- | Calculate a 'TextSize' object with rendering information
--   about the given string without actually rendering it.
textSize :: String -> ChartBackend TextSize
textSize text = singleton (GetTextSize text)

-- | Draw a single-line textual label anchored by the baseline (vertical) 
--   left (horizontal) point. Uses the current 'FontStyle' for drawing.
drawText :: Point -> String -> ChartBackend ()
drawText p text = singleton (DrawText p text)

-- | Apply the given transformation in this local
--   environment when drawing. The given transformation 
--   is applied after the current transformation. This
--   means both are combined.
withTransform :: Matrix -> ChartBackend a -> ChartBackend a
withTransform t p = singleton (WithTransform t p)

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
withFontStyle fs p = singleton (WithFontStyle fs p)

-- | Use the given fill style in this local
--   environment when filling paths.
withFillStyle :: FillStyle -> ChartBackend a -> ChartBackend a
withFillStyle fs p = singleton (WithFillStyle fs p)

-- | Use the given line style in this local
--   environment when stroking paths.
withLineStyle :: LineStyle -> ChartBackend a -> ChartBackend a
withLineStyle ls p = singleton (WithLineStyle ls p)

-- | Use the given clipping rectangle when drawing
--   in this local environment. The new clipping region
--   is intersected with the given clip region. You cannot 
--   escape the clip!
withClipRegion :: Rect -> ChartBackend a -> ChartBackend a
withClipRegion c p = singleton (WithClipRegion c p)

-- -----------------------------------------------------------------------
-- Rendering Utility Functions
-- -----------------------------------------------------------------------

-- | Get the point alignment function
getPointAlignFn :: ChartBackend (Point->Point)
getPointAlignFn = liftM afPointAlignFn (singleton GetAlignments)

-- | Get the coordinate alignment function
getCoordAlignFn :: ChartBackend (Point->Point)
getCoordAlignFn = liftM afCoordAlignFn (singleton GetAlignments)

