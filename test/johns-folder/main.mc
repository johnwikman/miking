-- main.mc

include "codegen-ocaml.mc"
include "stdlib.mc"
include "testlib.mc"

mexpr
use MExprOCamlCode in
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
let prog = bind_ prog testlib_ in
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

let prog = bind_ prog (let_ "res" (tyseq_ tyint_) (app3f_ (var_ "mapcuda_saxpy_int")
                                                          (int_ 17)
                                                          (int_ 11)
                                                          (seq_ [int_ 15, int_ 1]))) in

let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "Result of saxpy 17 11 [15, 1]\n"))) in
let prog = bind_ prog (let_ "_" (tyunit_) (app_ (var_ "printintln")
                                                (nth_ (var_ "res") (int_ 0)))) in
let prog = bind_ prog (let_ "_" (tyunit_) (app_ (var_ "printintln")
                                                (nth_ (var_ "res") (int_ 1)))) in

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let _ = writeFile "target/cpucode.ml" res.0 in
let _ = writeFile "target/gpucode.cpp" res.1 in

()
