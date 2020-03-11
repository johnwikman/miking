-- matmul.mc

include "codegen/ocaml.mc"
include "lib/std.mc"
include "lib/io.mc"
include "lib/matrix.mc"

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
let prog = bind_ prog libmatrix_ in

let prog = bind_ prog (let_ "matAinitfun" (tyarrows_ [tyint_, tyint_, tyint_]) (
    lam_ "row" (tyint_) (
      lam_ "col" (tyint_) (
        addi_ (muli_ (var_ "row") (var_ "row"))
              (var_ "col")
      )
    )
  )) in

let prog = bind_ prog (let_ "matBinitfun" (tyarrows_ [tyint_, tyint_, tyint_]) (
    lam_ "row" (tyint_) (
      lam_ "col" (tyint_) (
        modi_ (divi_ (muli_ (addi_ (var_ "row") (int_ 19)) (int_ 17)) (addi_ (var_ "col") (int_ 13)))
              (addi_ (var_ "row") (int_ 11))
      )
    )
  )) in


let prog = bind_ prog (let_ "matA_rows" (tyint_) (int_ 2)) in
let prog = bind_ prog (let_ "matA_cols" (tyint_) (int_ 5)) in

let prog = bind_ prog (let_ "matA" (tymatrixi_) (
    app3f_ (var_ "matrixIniti")
           (var_ "matA_rows")
           (var_ "matA_cols")
           (var_ "matAinitfun")
  )) in

let prog = bind_ prog (let_ "matAstr" (tyint_) (
    app3f_ (var_ "matrix2stri")
           (var_ "matA_rows")
           (var_ "matA_cols")
           (var_ "matA")
  )) in

let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "matA:\n"))) in
let prog = bind_ prog (let_ "_" (tyunit_) (print_ (var_ "matAstr"))) in


let prog = bind_ prog (let_ "matB_rows" (tyint_) (int_ 5)) in
let prog = bind_ prog (let_ "matB_cols" (tyint_) (int_ 3)) in

let prog = bind_ prog (let_ "matB" (tymatrixi_) (
    app3f_ (var_ "matrixIniti")
           (var_ "matB_rows")
           (var_ "matB_cols")
           (var_ "matBinitfun")
  )) in

let prog = bind_ prog (let_ "matBstr" (tyint_) (
    app3f_ (var_ "matrix2stri")
           (var_ "matB_rows")
           (var_ "matB_cols")
           (var_ "matB")
  )) in

let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatB:\n"))) in
let prog = bind_ prog (let_ "_" (tyunit_) (print_ (var_ "matBstr"))) in


------- Matrix Multiplication -------
let prog = bind_ prog (let_ "matAxB" (tymatrixi_) (
    app6f_ (var_ "matrixMuli")
           (var_ "matA_rows")
           (var_ "matA_cols")
           (var_ "matA")
           (var_ "matB_rows")
           (var_ "matB_cols")
           (var_ "matB")
  )) in

let prog = bind_ prog (let_ "matAxB_rows" (tyint_) (var_ "matA_rows")) in
let prog = bind_ prog (let_ "matAxB_cols" (tyint_) (var_ "matB_cols")) in

let prog = bind_ prog (let_ "matAxBstr" (tyint_) (
    app3f_ (var_ "matrix2stri")
           (var_ "matAxB_rows")
           (var_ "matAxB_cols")
           (var_ "matAxB")
  )) in

let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatAxB:\n"))) in
let prog = bind_ prog (let_ "_" (tyunit_) (print_ (var_ "matAxBstr"))) in
-------------------------------------


------- Matrix Multiplication (CUDA) -------
let prog = bind_ prog (let_ "matAxB" (tymatrixi_) (
    app6f_ (var_ "matrixMuliCUDA")
           (var_ "matA_rows")
           (var_ "matA_cols")
           (var_ "matA")
           (var_ "matB_rows")
           (var_ "matB_cols")
           (var_ "matB")
  )) in

let prog = bind_ prog (let_ "matAxB_rows" (tyint_) (var_ "matA_rows")) in
let prog = bind_ prog (let_ "matAxB_cols" (tyint_) (var_ "matB_cols")) in

let prog = bind_ prog (let_ "matAxBstr" (tyint_) (
    app3f_ (var_ "matrix2stri")
           (var_ "matAxB_rows")
           (var_ "matAxB_cols")
           (var_ "matAxB")
  )) in

let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatAxB (CUDA):\n"))) in
let prog = bind_ prog (let_ "_" (tyunit_) (print_ (var_ "matAxBstr"))) in
--------------------------------------------


let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()
