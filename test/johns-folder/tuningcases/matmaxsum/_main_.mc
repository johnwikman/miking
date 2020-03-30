-- Matrix max sum: Same reduction as for matrix multiplication where the
-- multiplication in the dot-product has been replaced with a max operator.

include "../../codegen/ocaml.mc"
include "../../lib/std.mc"
include "../../lib/io.mc"
include "../../lib/macros.mc"
include "../../lib/matrix.mc"

-- This should define the following variables: matB_rows, matB_cols, matA_rows, matA_cols
-- Should be included by defsize_
include "_size_.mc"

-- This should provide the specific method of performing the matrix max sum
-- Should define the variable matAmB which contains the result
-- Should be included by defspecific_
include "_specific_.mc"

let func_maxi =
  let_ "maxi" (tyarrows_ [tyint_, tyint_, tyint_]) (
    lam_ "x" (tyint_) (
      lam_ "y" (tyint_) (
        if_ (lti_ (var_ "x") (var_ "y"))
            (var_ "y")
            (var_ "x")
      )
    )
  )

let func_matrixMaxSumiWorker =
  let_ "matrixMaxSumiWorker" (tyarrows_ [tyint_, tyint_, tyint_, tymatrixi_, tymatrixi_, tyint_, tyint_]) (
    lam_ "innerDim" (tyint_) (
      lam_ "a_rows" (tyint_) (
        lam_ "b_cols" (tyint_) (
          lam_ "a" (tymatrixi_) (
            lam_ "b" (tymatrixi_) (
              lam_ "idx" (tyint_) (
                bindall_ [
                  let_ "row" (tyint_) (divi_ (var_ "idx") (var_ "b_cols")),
                  let_ "col" (tyint_) (modi_ (var_ "idx") (var_ "b_cols")),
                  let_ "a_start_offset" (tyint_) (muli_ (var_ "innerDim") (var_ "row")),
                  let_ "b_start_offset" (tyint_) (var_ "col"),
                  reclets_add "vecmsum" (tyarrows_ [tyint_, tyint_, tyint_, tyint_, tyint_]) (
                    lam_ "acc" (tyint_) (
                      lam_ "p" (tyint_) (
                        lam_ "a_offset" (tyint_) (
                          lam_ "b_offset" (tyint_) (
                            if_ (eqi_ (var_ "p") (var_ "innerDim"))
                                (var_ "acc")
                                (app4f_ (var_ "vecmsum")
                                        (addi_ (var_ "acc")
                                               (app2f_ (var_ "maxi")
                                                       (nth_ (var_ "a") (var_ "a_offset"))
                                                       (nth_ (var_ "b") (var_ "b_offset"))))
                                        (addi_ (var_ "p") (int_ 1))
                                        (addi_ (var_ "a_offset") (int_ 1))
                                        (addi_ (var_ "b_offset") (var_ "b_cols")))
                          )
                        )
                      )
                    )
                  ) (reclets_empty),
                  app4f_ (var_ "vecmsum")
                         (int_ 0)
                         (int_ 0)
                         (var_ "a_start_offset")
                         (var_ "b_start_offset")
                ]
              )
            )
          )
        )
      )
    )
  )

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
  func_maxi,
  func_matrixMaxSumiWorker,
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

------- Matrix Max Sum -------
let prog = bind_ prog defspecific_ in
------------------------------

------- Output Verification -------
let prog = bind_ prog (let_ "matAmB_rows" (tyint_) (var_ "matA_rows")) in
let prog = bind_ prog (let_ "matAmB_cols" (tyint_) (var_ "matB_cols")) in

let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatAmB:\n"))) in
let prog = bind_ prog (let_ "_" (tyunit_) (
    app3f_ (var_ "printMatrixi")
           (var_ "matAmB_rows")
           (var_ "matAmB_cols")
           (var_ "matAmB")
  )) in
-----------------------------------

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()
