# Degree Project Artifacts
This directory contains the aritfacts used for the degree project on automatic
GPU optimization through higher-order functions.

## Structure
 - `codegen` contains the code (not part of standard MCore) that is used for
   analyzing the AST and generating the code.
 - `lib` contains various includes with standard functions such as `int2string`
   and `map`. `stdlib` is not used here since the code must be pre-parsed.
 - `target` contains the generated code for each of the test cases and their
   variants. Use the makefile in this directory to build the test cases.
 - `testcases` contains the MCore test cases that were used to evaluate the
   implementation.
 - `tuningcases` contains a separate set of cases that are only used to tune
   the profiling constants in `codegen/cost-profiles.mc`.
 - `tuningtarget` contains the generated code for each of the tuning cases, in
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
feature in `bash`. The presented time is the average across all iterations:
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
Resulting costs after tuning, i.e. how much the heuristic predicted that the
operation would cost. The cost is not measured in a specified unit, except that
it should be proportional to execution time.

#### vecpo (n_iterations = 50)
|                  | 50t          | 500t         | 1m           | 10m          | 25m          |
|------------------|-------------:|-------------:|-------------:|-------------:|-------------:|
| c_ocaml          |      2750000 |     27500000 |     55000000 |    550000000 |   1375000000 |
| c_cuda           |     41000528 |     50005216 |     60010432 |    240104176 |    540260432 |
| c_ocaml / c_cuda |        0.067 |        0.550 |        0.917 |        2.291 |        2.545 |

#### gcdsum (n_iterations = 50)
|                  | 128          | 256          | 512          | 1024         | 2048         | 4096         | 8192         |
|------------------|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|
| c_ocaml          |      4481280 |     17908224 |     71599104 |    286328832 |   1145180160 |   4580450304 |  18321260544 |
| c_cuda           |     40264720 |     40529424 |     41058832 |     42117648 |     48409120 |     65165872 |    140417632 |
| c_ocaml / c_cuda |        0.111 |        0.442 |        1.744 |        6.798 |       23.656 |       70.289 |      130.477 |

#### matmaxsum (n_iterations = 20)
|                  | 32           | 64           | 128          | 256          | 512          | 1024         |
|------------------|-------------:|-------------:|-------------:|-------------:|-------------:|-------------:|
| c_ocaml          |      3377152 |     26615808 |    211320832 |   1684144128 |  13447462912 | 107476942848 |
| c_cuda           |     40064379 |     40324273 |     41966345 |     53488489 |    139475689 |    803149033 |
| c_ocaml / c_cuda |        0.084 |        0.660 |        5.035 |       31.486 |       96.414 |      133.819 |


## Baseline Lift CGO2017 results from Sleipner
| benchmark|time|version|size|
|----------|---:|-------|----|
| nbody_nvidia|4.2044|reference|small|
| nbody_nvidia|166.168|reference|large|
| nbody_amd|5.09949|reference|small|
| nbody_amd|313.236|reference|large|
| kmeans|1.116160|reference|small|
| kmeans|4.01502|reference|large|
| nn|0.81664|reference|small|
| nn|3.23483|reference|large|
| mriq|3.5704|reference|small|
| mriq|12.587|reference|large|
| md|0.272101|reference|small|
| md|1.628875|reference|large|
| mm|1.127200|reference|small|
| mm|63.692768|reference|large|
| convolution|4.02531|reference|small|
| convolution|16.0546|reference|large|
| gemv_N|1.087488|reference|small|
| gemv_N|10.868288|reference|large|
| gemv_T|0.527360|reference|small|
| gemv_T|2.094880|reference|large|

