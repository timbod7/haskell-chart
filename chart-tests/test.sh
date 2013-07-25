#! /bin/sh

#cabal install
cd ./tests-diffs
rm ./*.png 
cd ../tests
runhaskell all_tests.hs
cd ..
./gen-diffs-png.sh