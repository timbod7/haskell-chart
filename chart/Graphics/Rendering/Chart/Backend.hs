-- | This module provides the API for drawing operations abstracted
-- to arbitrary 'ChartBackend's.
module Graphics.Rendering.Chart.Backend
  (
  -- * The backend Monad
    ChartBackend(..)
  , CRender
  
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

  , defaultFontStyle
  
  , HTextAnchor(..)
  , VTextAnchor(..)

  , font_name
  , font_size
  , font_slant
  , font_weight
  , font_color
  ) where

import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Backend.Impl
