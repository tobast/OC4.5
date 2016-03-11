# OC4.5

This project aims to provide a pure OCaml implementation of C4.5 (https://en.wikipedia.org/wiki/C4.5_algorithm). This algorithm is used to generate a decision tree from a dataset and a criteria set, and is usually found in machine learning.

The algorithm description can be found in eg. "Efficient C4.5" by S. Ruggieri in IEEE transactions on knowledge and data engineering, vol. 14, no. 2, march/april 2002.

Building
===

If you do not have opam, install it.

If you do not have obuild, install it: `opam install obuild`.

Then, run
`obuild configure
obuild build`
