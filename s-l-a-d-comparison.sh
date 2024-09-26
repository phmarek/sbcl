#!/usr/bin/bash

tgt=/tmp/slad-comparison

echo DEFSTRUCT:
time ./run-sbcl.sh --script s-l-a-d-comparison.lisp 1 $tgt
ls -al $tgt

echo DEF-COLUMN-STRUCT:
time ./run-sbcl.sh --script s-l-a-d-comparison.lisp 2 $tgt
ls -al $tgt

echo DEF-COLUMN-STRUCT with initial-size:
time ./run-sbcl.sh --script s-l-a-d-comparison.lisp 3 $tgt
ls -al $tgt

echo DEF-COLUMN-STRUCT with initial-size and batched:
time ./run-sbcl.sh --script s-l-a-d-comparison.lisp 4 $tgt
ls -al $tgt
