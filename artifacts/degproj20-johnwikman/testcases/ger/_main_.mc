-- vecX * vecY^T + A

include "../../codegen/ocaml.mc"
include "../../lib/std.mc"
include "../../lib/io.mc"
include "../../lib/macros.mc"
include "../../lib/matrix.mc"
include "../../lib/benchmark-utils.mc"

-- This should define the following variables: matA_rows, matA_cols
-- Should be included by defsize_
include "_size_.mc"

-- This should provide the specific method of performing the GER
-- Should define the variables matxyA which contains the result
-- Should be included by defspecific_
include "_specific_.mc"

let func_gerWorkerf =
  let_ "gerWorkerf" (tyarrows_ [tyint_, tyint_, tyseq_ tyfloat_, tyseq_ tyfloat_, tyint_, tyfloat_, tyfloat_]) (
    lam_ "rows" (tyint_) (
      lam_ "cols" (tyint_) (
        lam_ "x" (tyseq_ tyfloat_) (
          lam_ "y" (tyseq_ tyfloat_) (
            lam_ "i" (tyint_) (
              lam_ "Aval" (tyfloat_) (
                bindall_ [
                  let_ "row" (tyint_) (divi_ (var_ "i") (var_ "cols")),
                  let_ "col" (tyint_) (modi_ (var_ "i") (var_ "cols")),
                  let_ "xval" (tyfloat_) (nth_ (var_ "x") (var_ "row")),
                  let_ "yval" (tyfloat_) (nth_ (var_ "y") (var_ "col")),
                  addf_ (mulf_ (var_ "xval") (var_ "yval"))
                        (var_ "Aval")
                ]
              )
            )
          )
        )
      )
    )
  )

let func_matAinitfun_v2 =
  let_ "matAinitfun_v2" (tyarrows_ [tyint_, tyint_, tyfloat_]) (
    lam_ "row" (tyint_) (
      lam_ "col" (tyint_) (
        subf_ (divf_ (int2float_ (addi_ (muli_ (var_ "row") (var_ "row"))
                                        (var_ "col")))
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

let func_vecYinitfun =
  let_ "vecYinitfun" (tyarrows_ [tyint_, tyint_, tyfloat_]) (
    lam_ "row" (tyint_) (
      lam_ "col" (tyint_) (
        int2float_ (modi_ (muli_ (var_ "row") (int_ 58437))
                          (int_ 84391))
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

let var_vecY_rows =
  let_ "vecY_rows" (tyint_) (var_ "matA_cols")
let var_vecY_cols =
  let_ "vecY_cols" (tyint_) (int_ 1)


let var_vecX =
  let_ "vecX" (tymatrixf_) (
    app3f_ (var_ "matrixInitf")
           (var_ "vecX_rows")
           (var_ "vecX_cols")
           (var_ "vecXinitfun")
  )

let var_vecY =
  let_ "vecY" (tymatrixf_) (
    app3f_ (var_ "matrixInitf")
           (var_ "vecY_rows")
           (var_ "vecY_cols")
           (var_ "vecYinitfun")
  )

let defcommon_ = bindall_ [
  defsize_,
  func_gerWorkerf,
  func_matAinitfun_v2,
  func_vecXinitfun,
  func_vecYinitfun,
  var_matA,
  var_vecX_rows,
  var_vecX_cols,
  var_vecY_rows,
  var_vecY_cols,
  var_vecX,
  var_vecY
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
let prog = bind_ prog libmatrixf_ in
let prog = bind_ prog defcommon_ in

------- Perform GER -------
--let prog = bind_ prog defspecific_ in
---------------------------

------- Output Verification -------
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatxyA:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app3f_ (var_ "printMatrixf")
--           (var_ "matA_rows")
--           (var_ "matA_cols")
--           (var_ "matxyA")
--  )) in
-----------------------------------

------- Benchmark GEMV -------
let bm =
  benchmark_ {bmparams_ with iters = 15}
             defspecific_
in
let prog = bind_ prog bm in
------------------------------

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()

