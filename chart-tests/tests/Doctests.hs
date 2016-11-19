{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Test.DocTest

main :: IO ()
main =
  doctest
    [ "../chart/Graphics/Rendering/Chart/Axis/Floating.hs"
    , "../chart/Numeric/Histogram.hs"
    ]
