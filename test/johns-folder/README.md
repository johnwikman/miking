# Degree Project Artifacts
This directory contains the aritfacts used for the degree project on automatic
GPU optimization through higher-order functions.

## Structure
 - *codegen* contains the code (not part of standard MCore) that is used for
   analyzing the AST and generating the code.
 - *lib* contains various includes with standard functions such as `int2string`
   and `map`. `stdlib` is not used here since the code must be pre-parsed.
 - *target* contains the generated code for each of the test cases and their
   variants. Use the makefile in this directory to build the test cases.
 - *testcases* contains the MCore test cases that were used to evaluate the
   implementation.
 - *tuningcases* contains a separate set of cases that are only used to tune
   the profiling constants in `codegen/cost-profiles.mc`.
 - *tuningtarget* contains the generated code for each of the tuning cases, in
   the same way as for `target`.

## Tuning Method
To be written
