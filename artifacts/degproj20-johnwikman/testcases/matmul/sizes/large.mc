-- Matrices with 4096 rows/cols

-- This is defined as the "large" input size in the Lift artifact

include "../../lib/macros.mc"

let var_matArows = let_ "matA_rows" (tyint_) (int_ 4096)
let var_matAcols = let_ "matA_cols" (tyint_) (int_ 4096)

let var_matBrows = let_ "matB_rows" (tyint_) (int_ 4096)
let var_matBcols = let_ "matB_cols" (tyint_) (int_ 4096)

let defsize_ = bindall_ [
  var_matArows,
  var_matAcols,
  var_matBrows,
  var_matBcols
]
