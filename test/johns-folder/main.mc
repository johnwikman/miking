-- main.mc

include "codegen-ocaml.mc"
include "stdlib.mc"
include "testlib.mc"

mexpr
use MExprCGOCaml in
let func_printintln =
  let_ "printintln" (
    lam_ "i" (
      print_ (concat_ (app_ (var_ "int2string")
                            (var_ "i"))
                      (str_ "\n"))
    )
  )
in

let prog = stdlib_ in
--let prog = bind_ prog testlib_ in
let prog = bind_ prog testlibcuda_ in
let prog = bind_ prog func_printintln in
let prog = bind_ prog (let_ "v" (tyint_) (int_ 10)) in
let prog = bind_ prog (let_ "res" (tyint_) (app_ (var_ "factorial")
                                                 (var_ "v"))) in
let prog = bind_ prog (let_ "printstr" (tystr_) (concat_ (str_ "factorial ")
                                                         (concat_ (app_ (var_ "int2string")
                                                                        (var_ "v"))
                                                                  (concat_ (str_ " = ")
                                                                           (concat_ (app_ (var_ "int2string")
                                                                                          (var_ "res"))
                                                                                    (str_ "\n")))))) in
let prog = bind_ prog (let_ "_" (tyunit_) (print_ (var_ "printstr"))) in

-- SAXPY
let prog = bind_ prog (let_ "res" (tyseq_ tyint_) (app3f_ (var_ "mapcuda_saxpy_int")
                                                          (int_ 17)
                                                          (int_ 11)
                                                          (seq_ [int_ 15, int_ 1]))) in

let prog = bind_ prog (let_ "_" (tyunit_) (app2f_ (var_ "printintarr")
                                                  (str_ "saxpy 17 11 [15, 1] result")
                                                  (var_ "res"))) in

-- SAXPY (float)
let prog = bind_ prog (let_ "res" (tyseq_ tyfloat_) (app1f_ (var_ "mapcuda_id2f_ignore2nd")
                                                            (makeseq_ (int_ 50000000) (int_ 0)))) in

let prog = bind_ prog (let_ "res" (tyseq_ tyfloat_) (app3f_ (var_ "mapcuda_saxpy_float")
                                                            (float_ 17.0)
                                                            (float_ 11.0)
                                                            (var_ "res"))) in

--let prog = bind_ prog (let_ "res" (tyseq_ tyfloat_) (app1f_ (var_ "mapi_id2f_ignore2nd")
--                                                            (makeseq_ (int_ 50000000) (int_ 0)))) in
--
--let prog = bind_ prog (let_ "res" (tyseq_ tyfloat_) (app2f_ (var_ "Array.map")
--                                                            (app2f_ (var_ "saxpy_float_single")
--                                                                    (float_ 17.0)
--                                                                    (float_ 11.0))
--                                                            (var_ "res"))) in

--let prog = bind_ prog (let_ "_" (tyunit_) (app2f_ (var_ "printfloatarr")
--                                                  (str_ "saxpy 17.0 11.0 \"res\" result")
--                                                  (var_ "res"))) in

-- SAXPY (Int - full)
let prog = bind_ prog (let_ "res" (tyseq_ tyint_) (app3f_ (var_ "mapcuda_saxpy_intfull")
                                                          (int_ 17)
                                                          (seq_ [int_ 15, int_ 1])
                                                          (seq_ [int_ 9, int_ 8]))) in

let prog = bind_ prog (let_ "_" (tyunit_) (app2f_ (var_ "printintarr")
                                                  (str_ "saxpy_mapfull 17 (a) [15, 1] (x) [9, 8] (y) result")
                                                  (var_ "res"))) in

-- ID (cudaMapi)
let prog = bind_ prog (let_ "res" (tyseq_ tyint_) (app1f_ (var_ "mapcuda_id_ignore2nd")
                                                          (makeseq_ (int_ 70) (int_ 0)))) in

let prog = bind_ prog (let_ "_" (tyunit_) (app2f_ (var_ "printintarr")
                                                  (str_ "mapcuda_id_ignore2nd result")
                                                  (var_ "res"))) in

-- Factorial (cudaMapi)
let prog = bind_ prog (let_ "factidx" (tyarrows_ [tyint_, tyint_, tyint_]) (
                            lam_ "i" tyint_ (lam_ "ignored_arg" tyint_ (
                                 app1f_ (var_ "factorial") (var_ "i"))))) in

let prog = bind_ prog (let_ "res" (tyseq_ tyint_) (cudamapi_ 8
                                                             (var_ "factidx")
                                                             (makeseq_ (int_ 16) (int_ 0)))) in

let prog = bind_ prog (let_ "_" (tyunit_) (app2f_ (var_ "printintarr")
                                                  (str_ "cudaMapi factidx result")
                                                  (var_ "res"))) in

-- Fibonacci (cudaMapi)
let prog = bind_ prog (let_ "fibidx" (tyarrows_ [tyint_, tyint_, tyint_]) (
                            lam_ "i" tyint_ (lam_ "ignored_arg" tyint_ (
                                 app1f_ (var_ "fib") (var_ "i"))))) in

let prog = bind_ prog (let_ "res" (tyseq_ tyint_) (cudamapidx_ 16
                                                               (var_ "fib")
                                                               (int_ 48))) in

let prog = bind_ prog (let_ "_" (tyunit_) (app2f_ (var_ "printintarr")
                                                  (str_ "cudaMapi fibidx result")
                                                  (var_ "res"))) in

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let _ = writeFile "target/cpucode.ml" res.0 in
let _ = writeFile "target/gpucode.cpp" res.1 in

()
