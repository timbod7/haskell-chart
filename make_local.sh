#!/bin/sh -x
runghc Setup.hs configure --prefix ~/ghc --user
runghc Setup.hs build
runghc Setup.hs install

