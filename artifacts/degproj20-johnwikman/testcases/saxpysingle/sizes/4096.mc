-- Array of 4096 elements

include "../../lib/macros.mc"

let var_vecsize = let_ "vecsize" (tyint_) (int_ 4096)

let defsize_ = bindall_ [
  var_vecsize
]
