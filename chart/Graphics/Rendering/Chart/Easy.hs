{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Easy
-- Copyright   :  (c) Tim Docker 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Importing the Easy module brings into scope all core functions and types required
-- for working with the chart library. This includes key external dependencies such as
-- Control.Len and Data.Colour.
--
-- Note that chart backends must still be explicitly imported, as some backends cannot
-- be built on all platforms.

module Graphics.Rendering.Chart.Easy(

  module Control.Lens,
  module Data.Default.Class,
  module Data.Colour,
  module Data.Colour.Names,

  module Graphics.Rendering.Chart,
  module Graphics.Rendering.Chart.State,
  ) where

import Control.Lens
import Data.Default.Class
import Data.Colour hiding (over) -- overlaps with lens over function
import Data.Colour.Names
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.State
