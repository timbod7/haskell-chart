#! /bin/bash

FILES=tests-reference/*.png

for f in $FILES
do
  base=$(basename $f)
  compare $f tests/$base tests-diffs/$base
done
