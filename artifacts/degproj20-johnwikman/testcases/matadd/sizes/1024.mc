-- Matrices with 1024 rows/cols

include "../../lib/macros.mc"

let var_mat_rows = let_ "mat_rows" (tyint_) (int_ 1024)
let var_mat_cols = let_ "mat_cols" (tyint_) (int_ 1024)

let defsize_ = bindall_ [
  var_mat_rows,
  var_mat_cols
]
