-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.Unit
-- Copyright   :  (c) Tim Docker 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- This module provides the API for drawing operations abstracted
-- to arbitrary 'CBProgram's.

module Graphics.Rendering.Chart.Backend
  (
  -- * The backend Monad
    CBProgram
  
  -- * Backend Operations
  , fillPath
  , strokePath
  , drawText, textSize
  , withTransform
  , withClipRegion
  , withFontStyle, withFillStyle, withLineStyle
  
  -- * Backend Helpers
--  , getTransform
--  , getFillStyle, getFontStyle
--  , getLineStyle, getClipRegion
  , getPointAlignFn, getCoordAlignFn

  -- * Text Metrics
  , TextSize(..)                     
  
  -- * Line Types
  , LineCap(..)
  , LineJoin(..)
  , LineStyle(..)
  
  , line_width
  , line_color
  , line_dashes
  , line_cap
  , line_join
  
  -- * Fill Types
  , FillStyle(..)

  -- * Font and Text Types
  , FontWeight(..)
  , FontSlant(..)
  , FontStyle(..)

  , HTextAnchor(..)
  , VTextAnchor(..)

  , font_name
  , font_size
  , font_slant
  , font_weight
  , font_color
  
  , AlignmentFn
  , AlignmentFns
  , vectorAlignmentFns
  , bitmapAlignmentFns
  ) where

import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Backend.Impl
