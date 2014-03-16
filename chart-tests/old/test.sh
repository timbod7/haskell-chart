#! /bin/sh

#cabal install
cd ./tests-diffs
rm ./*.png 
cd ../tests
runhaskell DiagramsCairo.hs
cd ..
./gen-diffs-png.sh