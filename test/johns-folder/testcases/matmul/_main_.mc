-- Matrix multiplication

include "../../codegen/ocaml.mc"
include "../../lib/std.mc"
include "../../lib/io.mc"
include "../../lib/macros.mc"
include "../../lib/matrix.mc"

-- This should define the following variables: matB_rows, matB_cols, matA_rows, matA_cols
-- Should be included by defsize_
include "_size_.mc"

-- This should provide the specific method of performing the matrix multiplication
-- Should define the variable matAxB which contains the result
-- Should be included by defspecific_
include "_specific_.mc"

let func_matAinitfun =
  let_ "matAinitfun" (tyarrows_ [tyint_, tyint_, tyint_]) (
    lam_ "row" (tyint_) (
      lam_ "col" (tyint_) (
        addi_ (muli_ (var_ "row") (var_ "row"))
              (var_ "col")
      )
    )
  )

let func_matBinitfun =
  let_ "matBinitfun" (tyarrows_ [tyint_, tyint_, tyint_]) (
    lam_ "row" (tyint_) (
      lam_ "col" (tyint_) (
        modi_ (divi_ (muli_ (addi_ (var_ "row") (int_ 19)) (int_ 17)) (addi_ (var_ "col") (int_ 13)))
              (addi_ (var_ "row") (int_ 11))
      )
    )
  )

let func_matAinitfun_v2 =
  let_ "matAinitfun_v2" (tyarrows_ [tyint_, tyint_, tyint_]) (
    lam_ "row" (tyint_) (
      lam_ "col" (tyint_) (
        subi_ (modi_ (addi_ (muli_ (var_ "row") (var_ "row"))
                            (var_ "col"))
                     (int_ 3))
              (int_ 1)
      )
    )
  )

let func_matBinitfun_v2 =
  let_ "matBinitfun_v2" (tyarrows_ [tyint_, tyint_, tyint_]) (
    lam_ "row" (tyint_) (
      lam_ "col" (tyint_) (
        modi_ (divi_ (muli_ (addi_ (var_ "row") (int_ 19)) (int_ 17)) (addi_ (var_ "col") (int_ 13)))
              (int_ 2)
      )
    )
  )

let var_matA =
  let_ "matA" (tymatrixi_) (
    app3f_ (var_ "matrixIniti")
           (var_ "matA_rows")
           (var_ "matA_cols")
           (var_ "matAinitfun_v2")
  )

let var_matB =
  let_ "matB" (tymatrixi_) (
    app3f_ (var_ "matrixIniti")
           (var_ "matB_rows")
           (var_ "matB_cols")
           (var_ "matBinitfun_v2")
  )

let var_innerDim =
  let_ "innerDim" (tyint_) (var_ "matA_cols")

let defcommon_ = bindall_ [
  defsize_,
  func_matAinitfun,
  func_matBinitfun,
  func_matAinitfun_v2,
  func_matBinitfun_v2,
  var_matA,
  var_matB,
  var_innerDim
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
let prog = bind_ prog libmatrix_ in
let prog = bind_ prog defcommon_ in

------- Matrix Multiplication -------
let prog = bind_ prog defspecific_ in
-------------------------------------

------- Output Verification -------
--let prog = bind_ prog (let_ "matAxB_rows" (tyint_) (var_ "matA_rows")) in
--let prog = bind_ prog (let_ "matAxB_cols" (tyint_) (var_ "matB_cols")) in
--
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatAxB:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app3f_ (var_ "printMatrixi")
--           (var_ "matAxB_rows")
--           (var_ "matAxB_cols")
--           (var_ "matAxB")
--  )) in
-----------------------------------

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()
