{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Test.DocTest

main :: IO ()
main =
  doctest
    [ "-ichart"
    , "chart/Graphics/Rendering/Chart/Axis/Floating.hs"
    ]
