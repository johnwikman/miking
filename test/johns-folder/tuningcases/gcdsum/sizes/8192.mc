-- Array of 8192 elements

include "../../lib/macros.mc"

let var_vecsize = let_ "vecsize" (tyint_) (int_ 8192)

let defsize_ = bindall_ [
  var_vecsize
]
