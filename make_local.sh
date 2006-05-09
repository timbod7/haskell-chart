#!/bin/sh -x
runghc Setup.hs configure --prefix ~/ghc --user
runghc Setup.hs build
if [ "$?" -eq 0 ]; then
    runghc Setup.hs install
else
    runghc Setup.hs unregister --user
fi
