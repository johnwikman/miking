-- Matrices with 1024 rows/cols

include "../../lib/macros.mc"

let var_matArows = let_ "matA_rows" (tyint_) (int_ 1024)
let var_matAcols = let_ "matA_cols" (tyint_) (int_ 1024)

let defsize_ = bindall_ [
  var_matArows,
  var_matAcols
]
