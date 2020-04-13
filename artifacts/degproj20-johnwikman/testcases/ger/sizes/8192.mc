-- Matrices with 8192 rows/cols

include "../../lib/macros.mc"

let var_matArows = let_ "matA_rows" (tyint_) (int_ 8192)
let var_matAcols = let_ "matA_cols" (tyint_) (int_ 8192)

let defsize_ = bindall_ [
  var_matArows,
  var_matAcols
]
