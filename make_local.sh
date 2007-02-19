#!/bin/sh -x
runghc Setup.hs configure --prefix ~/ghc --user
runghc Setup.hs build
if [ "$?" -eq 0 ]; then
    runghc Setup.hs install
else
    runghc Setup.hs unregister --user
fi

(cd tests; rm *.o)
(cd tests; ghc --make -package Chart -o test test.hs)
(cd tests; ghc --make -package Chart -o test2 test2.hs)
(cd tests; ghc --make -package Chart -o test3 test3.hs)
(cd tests; ghc --make -package Chart -o test4 test4.hs)
