-- Array of 16384 elements

include "../../lib/macros.mc"

let var_vecsize = let_ "vecsize" (tyint_) (int_ 16384)

let defsize_ = bindall_ [
  var_vecsize
]
