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
invoked the first time, therefore all tuning cases have the operation in
question iterated multiple times.

### Sleipner measurements
Time measurements from running the tuning tests on Sleipner by using the `time`
program. The presented time is the average across all iterations:
`t = (t_user + t_sys) / n_iterations`

#### vecpo (n_iterations = 50)
|                  | 50t      | 500t     | 1m       | 10m      | 25m      |
|------------------|---------:|---------:|---------:|---------:|---------:|
| t_ocaml          |   0.001s |   0.004s |   0.007s |   0.074s |   0.186s |
| t_cuda           |   0.003s |   0.005s |   0.007s |   0.036s |   0.087s |
| t_ocaml / t_cuda |   0.333  |   0.800  |   1.000  |   2.055  |   2.138  |

#### gcdsum (n_iterations = 50)
|                  | 128      | 256      | 512      | 1024     | 2048     | 4096     | 8192     |
|------------------|---------:|---------:|---------:|---------:|---------:|---------:|---------:|
| t_ocaml          |   0.001s |   0.002s |   0.009s |   0.036s |   0.139s |   0.551s |   2.182s |
| t_cuda           |   0.003s |   0.003s |   0.003s |   0.005s |   0.006s |   0.008s |   0.012s |
| t_ocaml / t_cuda |   0.333  |   0.667  |   3.000  |   7.200  |  23.167  |  68.875  | 181.833  |

#### matmaxsum (n_iterations = 20)
|                  | 32       | 64       | 128      | 256      | 512      | 1024     |
|------------------|---------:|---------:|---------:|---------:|---------:|---------:|
| t_ocaml          |   0.001s |   0.003s |   0.021s |   0.169s |   2.104s |  16.296s |
| t_cuda           |   0.007s |   0.007s |   0.007s |   0.008s |   0.012s |   0.062s |
| t_ocaml / t_cuda |   0.143  |   0.429  |   3.000  |  21.125  | 175.333  | 262.838  |

### Sleipner tuning results

vecpo-cudapredict-s50thousand-it50.txt: 0.024 seconds (adj: 0.000) (c_cuda: 30301274) (c_ocaml: 1450000) (c_ocaml / c_cuda: 0.048)
vecpo-cudapredict-s500thousand-it50.txt: 0.167 seconds (adj: 0.003) (c_cuda: 33012714) (c_ocaml: 14500000) (c_ocaml / c_cuda: 0.439)
vecpo-cudapredict-s1million-it50.txt: 0.340 seconds (adj: 0.007) (c_cuda: 36025402) (c_ocaml: 29000000) (c_ocaml / c_cuda: 0.805)
vecpo-cudapredict-s10million-it50.txt: 1.800 seconds (adj: 0.036) (c_cuda: 90253916) (c_ocaml: 290000000) (c_ocaml / c_cuda: 3.213)
vecpo-cudapredict-s25million-it50.txt: 4.329 seconds (adj: 0.087) (c_cuda: 180634790) (c_ocaml: 725000000) (c_ocaml / c_cuda: 4.014)
gcdsum-cudapredict-s128-it50.txt: 0.051 seconds (adj: 0.001) (c_cuda: 30456592) (c_ocaml: 2954240) (c_ocaml / c_cuda: 0.097)
gcdsum-cudapredict-s256-it50.txt: 0.137 seconds (adj: 0.003) (c_cuda: 30913168) (c_ocaml: 11806720) (c_ocaml / c_cuda: 0.382)
gcdsum-cudapredict-s512-it50.txt: 0.149 seconds (adj: 0.003) (c_cuda: 31826320) (c_ocaml: 47206400) (c_ocaml / c_cuda: 1.483)
gcdsum-cudapredict-s1024-it50.txt: 0.267 seconds (adj: 0.005) (c_cuda: 33652624) (c_ocaml: 188784640) (c_ocaml / c_cuda: 5.610)
gcdsum-cudapredict-s2048-it50.txt: 0.299 seconds (adj: 0.006) (c_cuda: 44592032) (c_ocaml: 755056640) (c_ocaml / c_cuda: 16.933)
gcdsum-cudapredict-s4096-it50.txt: 0.403 seconds (adj: 0.008) (c_cuda: 88331200) (c_ocaml: 3020062720) (c_ocaml / c_cuda: 34.190)
gcdsum-cudapredict-s8192-it50.txt: 0.618 seconds (adj: 0.012) (c_cuda: 263250944) (c_ocaml: 12079923200) (c_ocaml / c_cuda: 45.887)
matmaxsum-cudapredict-s32-it20.txt: 0.023 seconds (adj: 0.001) (c_cuda: 30053755) (c_ocaml: 2236416) (c_ocaml / c_cuda: 0.074)
matmaxsum-cudapredict-s64-it20.txt: 0.076 seconds (adj: 0.004) (c_cuda: 30392428) (c_ocaml: 17596416) (c_ocaml / c_cuda: 0.579)
matmaxsum-cudapredict-s128-it20.txt: 0.145 seconds (adj: 0.007) (c_cuda: 32988976) (c_ocaml: 139591680) (c_ocaml / c_cuda: 4.231)
matmaxsum-cudapredict-s256-it20.txt: 0.140 seconds (adj: 0.007) (c_cuda: 53310016) (c_ocaml: 1112014848) (c_ocaml / c_cuda: 20.859)
matmaxsum-cudapredict-s512-it20.txt: 0.224 seconds (adj: 0.011) (c_cuda: 214072960) (c_ocaml: 8877244416) (c_ocaml / c_cuda: 41.468)
matmaxsum-cudapredict-s1024-it20.txt: 1.216 seconds (adj: 0.061) (c_cuda: 1492955008) (c_ocaml: 70942457856) (c_ocaml / c_cuda: 47.518)

