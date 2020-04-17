-- Matrices with 2048 rows/cols

include "../../lib/macros.mc"

let var_matArows = let_ "matA_rows" (tyint_) (int_ 2048)
let var_matAcols = let_ "matA_cols" (tyint_) (int_ 2048)

let defsize_ = bindall_ [
  var_matArows,
  var_matAcols
]
