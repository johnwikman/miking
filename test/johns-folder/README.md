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
The mapped function in each tuning test has different time complexities
relative to the total mapped sequence length _n_ as parameter. `vecpo` (vector
plus one) has O(1). `gcdsum` has O(n). `matmaxmul` has O(sqrt(n)). The tunable
constants in `cost-profiles.mc` are then tuned by hand such that the resulting
cost matches the elapsed time difference between CUDA and OCaml.

The CUDA C++ functions appear to have a fixed overhead for when they are
invoked the first time, therefore the 

### Sleipner measurements
Time measurements from running the tuning tests on Sleipner by using the `time`
program. The presented times are the median from running the test 10-20 times
in rapid succession.

#### vecpo
|          | 500t      | 1m        | 10m       | 25m       | 50m       |
|----------|----------:|----------:|----------:|----------:|----------:|
| t_cudax1 | 0m0,285s  | 0m0,412s  | 0m1,902s  | 0m4,416s  | 0m8,371s  |
| t_ocaml  | 0m0,203s  | 0m0,392s  | 0m3,741s  | 0m9,320s  | 0m18,622s |

#### gcdsum
|          | 1024      | 2048      | 4096      | 8192      | 16384     | 32768     |
|----------|----------:|----------:|----------:|----------:|----------:|----------:|
| t_cudax1 | 0m0,176s  | 0m0,172s  | 0m0,185s  | 0m0,193s  | 0m0,239s  | 0m0,320s  |
| t_ocaml  | 0m0,054s  | 0m0,160s  | 0m0,571s  | 0m2,207s  | 0m8,710s  | 0m34,726s |

#### matmaxsum
|          | 128       | 256       | 512       | 1024      | 2048      |
|----------|----------:|----------:|----------:|----------:|----------:|
| t_cudax1 | 0m0,174s  | 0m0,180s  | 0m0,203s  | 0m0,291s  | 0m0,964s  |
| t_ocaml  | 0m0,041s  | 0m0,195s  | 0m2,278s  | 0m16,601s | 5m6,487s  |
