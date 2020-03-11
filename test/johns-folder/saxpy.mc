-- saxpy.mc

include "codegen/ocaml.mc"
include "lib/std.mc"
include "lib/io.mc"

let func_saxpy_int =
  let_ "saxpy_int" (tyarrows_ [tyint_, tyint_, tyint_, tyint_]) (
    lam_ "x" (tyint_) (lam_ "y" (tyint_) (lam_ "a" (tyint_) (
          addi_ (muli_ (var_ "a")
                       (var_ "x"))
                (var_ "y")))))

let func_saxpy_float =
  let_ "saxpy_float" (tyarrows_ [tyfloat_, tyfloat_, tyfloat_, tyfloat_]) (
    lam_ "x" (tyfloat_) (lam_ "y" (tyfloat_) (lam_ "a" (tyfloat_) (
          addf_ (mulf_ (var_ "a")
                       (var_ "x"))
                (var_ "y")))))

let func_saxpy_intseq =
  let_ "saxpy_intseq" (tyarrows_ [tyint_, tyseq_ tyint_, tyint_, tyint_, tyint_]) (
    lam_ "a" (tyint_) (lam_ "y" (tyseq_ tyint_) (lam_ "i" (tyint_) (lam_ "x" (tyint_) (
         addi_ (muli_ (var_ "a")
                      (var_ "x"))
               (nth_ (var_ "y") (var_ "i")))))))

let func_id_ignore2nd =
  let_ "id_ignore2nd" (tyarrows_ [tyint_, tyint_, tyint_]) (
    lam_ "x" (tyint_) (lam_ "y" (tyint_) (
        (var_ "x"))))

let func_id2f_ignore2nd =
  let_ "id2f_ignore2nd" (tyarrows_ [tyint_, tyint_, tyfloat_]) (
    lam_ "x" (tyint_) (lam_ "y" (tyint_) (
        int2float_ (var_ "x"))))

let saxpyfuncs_ = bindall_ [
  func_saxpy_int,
  func_saxpy_float,
  func_saxpy_intseq,
  func_id_ignore2nd,
  func_id2f_ignore2nd
]

mexpr
use MExprCGOCaml in

if neqi (length argv) 4 then
	let _ = dprint argv in
	error "Must specify a target directory."
else
  -- carry on
let targetdir = nth argv 3 in

let prog = libstd_ in
let prog = bind_ prog libio_ in
let prog = bind_ prog saxpyfuncs_ in


--------- SAXPY (Single Ints) ---------
let prog = bind_ prog (let_ "res" (tyseq_ tyint_) (
    cudamap_ 32 -- elem per thread
             (app2f_ (var_ "saxpy_int")
                     (int_ 17)
                     (int_ 11))
             (seq_ [int_ 15, int_ 1])
  )) in

let prog = bind_ prog (let_ "_" (tyunit_) (app2f_ (var_ "printintarr")
                                                  (str_ "saxpy 17 11 [15, 1] result")
                                                  (var_ "res"))) in
---------------------------------------


--------- SAXPY (Single Floats) ---------
let prog = bind_ prog (let_ "res" (tyseq_ tyfloat_) (
    cudamapi_ 512 -- elem per thread
              (var_ "id2f_ignore2nd")
              (makeseq_ (int_ 5000) (int_ 0))
  )) in

let prog = bind_ prog (let_ "res" (tyseq_ tyfloat_) (
    cudamap_ 512 -- elem per thread
             (app2f_ (var_ "saxpy_float")
                     (float_ 17.0)
                     (float_ 11.0))
             (var_ "res")
  )) in

--let prog = bind_ prog (let_ "res" (tyseq_ tyfloat_) (
--    app2f_ (var_ "Array.map")
--           (app2f_ (var_ "saxpy_float")
--                   (float_ 17.0)
--                   (float_ 11.0))
--           (var_ "res")
--  )) in

--let prog = bind_ prog (let_ "_" (tyunit_) (app2f_ (var_ "printfloatarr")
--                                                  (str_ "saxpy 17.0 11.0 \"res\" result")
--                                                  (var_ "res"))) in
-----------------------------------------


--------- SAXPY (Sequence of Ints) ---------
let prog = bind_ prog (let_ "res" (tyseq_ tyint_) (
    cudamapi_ 32 -- elem per thread
              (app2f_ (var_ "saxpy_intseq")
                      (int_ 17)
                      (seq_ [int_ 9, int_ 8]))
              (seq_ [int_ 15, int_ 1])
  )) in

let prog = bind_ prog (let_ "_" (tyunit_) (app2f_ (var_ "printintarr")
                                                  (str_ "saxpy_intseq (a: 17) (x: [15, 1]) (y: [9, 8]) result")
                                                  (var_ "res"))) in
--------------------------------------------

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()
