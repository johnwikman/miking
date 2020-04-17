-- Matrix addition

include "../../codegen/ocaml.mc"
include "../../lib/std.mc"
include "../../lib/io.mc"
include "../../lib/macros.mc"
include "../../lib/matrix.mc"
include "../../lib/benchmark-utils.mc"

-- This should define the following variables: mat_rows, mat_cols
-- Should be included by defsize_
include "_size_.mc"

-- This should provide the specific method of performing the matrix addition
-- Should define the variable matAplusB which contains the result
-- Should be included by defspecific_
include "_specific_.mc"

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

let func_matBinitfun_v2 =
  let_ "matBinitfun_v2" (tyarrows_ [tyint_, tyint_, tyfloat_]) (
    lam_ "row" (tyint_) (
      lam_ "col" (tyint_) (
        int2float_ (modi_ (divi_ (muli_ (addi_ (var_ "row") (int_ 19)) (int_ 17)) (addi_ (var_ "col") (int_ 13)))
                          (int_ 2))
      )
    )
  )

let func_matrixAddfWorker =
  let_ "matrixAddfWorker" (tyarrows_ [tymatrixf_, tymatrixf_, tyint_, tyfloat_]) (
    lam_ "A" (tymatrixf_) (
      lam_ "B" (tymatrixf_) (
        lam_ "idx" (tyint_) (
          addf_ (nth_ (var_ "A") (var_ "idx"))
                (nth_ (var_ "B") (var_ "idx"))
        )
      )
    )
  )

let var_matA =
  let_ "matA" (tymatrixf_) (
    app3f_ (var_ "matrixInitf")
           (var_ "mat_rows")
           (var_ "mat_cols")
           (var_ "matAinitfun_v2")
  )

let var_matB =
  let_ "matB" (tymatrixf_) (
    app3f_ (var_ "matrixInitf")
           (var_ "mat_rows")
           (var_ "mat_cols")
           (var_ "matBinitfun_v2")
  )

let defcommon_ = bindall_ [
  defsize_,
  func_matAinitfun_v2,
  func_matBinitfun_v2,
  func_matrixAddfWorker,
  var_matA,
  var_matB
]

mexpr
use MExprCGOCaml in

if neqi (length argv) 4 then
  let _ = dprint argv in
  error "Must specify a target directory."
else
  -- carry on
let targetdir = get argv 3 in

let prog = libstd_ in
let prog = bind_ prog libio_ in
let prog = bind_ prog libmatrixf_ in
let prog = bind_ prog defcommon_ in

------- Matrix Addition -------
--let prog = bind_ prog defspecific_ in
-------------------------------

------- Output Verification -------
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatA:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app3f_ (var_ "printMatrixf")
--           (var_ "mat_rows")
--           (var_ "mat_cols")
--           (var_ "matA")
--  )) in
--
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatB:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app3f_ (var_ "printMatrixf")
--           (var_ "mat_rows")
--           (var_ "mat_cols")
--           (var_ "matB")
--  )) in
--
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatAplusB:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app3f_ (var_ "printMatrixf")
--           (var_ "mat_rows")
--           (var_ "mat_cols")
--           (var_ "matAplusB")
--  )) in
-----------------------------------

------- Benchmark Matrix Addition -------
let bm =
  benchmark_ {bmparams_ with iters = 15}
             defspecific_
in
let prog = bind_ prog bm in
-----------------------------------------

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()
