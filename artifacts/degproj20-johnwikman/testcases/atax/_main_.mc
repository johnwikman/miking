-- A^T * A * x

include "../../codegen/ocaml.mc"
include "../../lib/std.mc"
include "../../lib/io.mc"
include "../../lib/macros.mc"
include "../../lib/matrix.mc"

-- This should define the following variables: matA_rows, matA_cols
-- Should be included by defsize_
include "_size_.mc"

-- This should provide the specific method of performing the ATAx
-- Should define the variables matATA and vecATAx which contain the respective results
-- Should be included by defspecific_
include "_specific_.mc"

let func_matAinitfun_v2 =
  let_ "matAinitfun_v2" (tyarrows_ [tyint_, tyint_, tyfloat_]) (
    lam_ "row" (tyint_) (
      lam_ "col" (tyint_) (
        subf_ (divf_ (addf_ (mulf_ (var_ "row") (var_ "row"))
                            (var_ "col"))
                     (float_ 3.0))
              (float_ 0.014)
      )
    )
  )

let func_vecXinitfun =
  let_ "vecXinitfun" (tyarrows_ [tyint_, tyint_, tyfloat_]) (
    lam_ "row" (tyint_) (
      lam_ "col" (tyint_) (
        int2float_ (modi_ (muli_ (var_ "row") (int_ 10657))
                          (int_ 41081))
      )
    )
  )

let var_matA =
  let_ "matA" (tymatrixf_) (
    app3f_ (var_ "matrixInitf")
           (var_ "matA_rows")
           (var_ "matA_cols")
           (var_ "matAinitfun_v2")
  )

let var_vecX_rows =
  let_ "vecX_rows" (tyint_) (var_ "matA_cols")
let var_vecX_cols =
  let_ "vecX_cols" (tyint_) (int_ 1)


let var_vecX =
  let_ "vecX" (tymatrixf_) (
    app3f_ (var_ "matrixInitf")
           (var_ "vecX_rows")
           (var_ "vecX_cols")
           (var_ "vecXinitfun")
  )

let defcommon_ = bindall_ [
  defsize_,
  func_matAinitfun_v2,
  func_vecXinitfun,
  var_matA,
  var_vecX_rows,
  var_vecX_cols,
  var_vecX
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

------- Perform A^T * A and multiply the result by vecX -------
let prog = bind_ prog defspecific_ in
---------------------------------------------------------------

------- Output Verification -------
--let prog = bind_ prog (let_ "matATA_rows" (tyint_) (var_ "matA_cols")) in
--let prog = bind_ prog (let_ "matATA_cols" (tyint_) (var_ "matA_cols")) in
--
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatATA:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app3f_ (var_ "printMatrixf")
--           (var_ "matATA_rows")
--           (var_ "matATA_cols")
--           (var_ "matATA")
--  )) in
--
--let prog = bind_ prog (let_ "vecATAx_rows" (tyint_) (var_ "matATA_rows")) in
--let prog = bind_ prog (let_ "vecATAx_cols" (tyint_) (var_ "vecX_cols")) in
--
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nvecATAx:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app3f_ (var_ "printMatrixf")
--           (var_ "vecATAx_rows")
--           (var_ "vecATAx_cols")
--           (var_ "vecATAx")
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
