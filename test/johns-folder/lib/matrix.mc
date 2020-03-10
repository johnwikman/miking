-- matrix.mc (Matrix functionality)

include "macros.mc"

let func_mkmatrixi =
  let_ "mkmatrixi" (tyarrows_ [tyint_, tyint_, tyseq_ tyint_]) (
    lam_ "n_rows" (tyint_) (
      lam_ "n_cols" (tyint_) (
        makeseq_ (muli_ (var_ "n_rows") (var_ "n_cols"))
                 (int_ 0)
      )
    )
  )

let func_mkmatrixf =
  let_ "mkmatrixf" (tyarrows_ [tyint_, tyint_, tyseq_ tyfloat_]) (
    lam_ "n_rows" (tyint_) (
      lam_ "n_cols" (tyint_) (
        makeseq_ (muli_ (var_ "n_rows") (var_ "n_cols"))
                 (float_ 0.0)
      )
    )
  )

let libmatrix_ = bindall_ [
  func_mkmatrixi,
  func_mkmatrixf
]
