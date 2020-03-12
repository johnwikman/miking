-- matmul.mc (Common definitions for the Matrix multiplication tests)

include "macros.mc"
include "matrix.mc"

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

let var_matArows = let_ "matA_rows" (tyint_) (int_ 1024)
let var_matAcols = let_ "matA_cols" (tyint_) (int_ 1024)

let var_matA =
  let_ "matA" (tymatrixi_) (
    app3f_ (var_ "matrixIniti")
           (var_ "matA_rows")
           (var_ "matA_cols")
           (var_ "matAinitfun_v2")
  )

let var_matBrows = let_ "matB_rows" (tyint_) (int_ 1024)
let var_matBcols = let_ "matB_cols" (tyint_) (int_ 1024)

let var_matB =
  let_ "matB" (tymatrixi_) (
    app3f_ (var_ "matrixIniti")
           (var_ "matB_rows")
           (var_ "matB_cols")
           (var_ "matBinitfun_v2")
  )

let libmatmul_ = bindall_ [
  func_matAinitfun,
  func_matBinitfun,
  func_matAinitfun_v2,
  func_matBinitfun_v2,
  var_matArows,
  var_matAcols,
  var_matA,
  var_matBrows,
  var_matBcols,
  var_matB
]
