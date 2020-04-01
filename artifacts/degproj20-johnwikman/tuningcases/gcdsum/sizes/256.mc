-- Array of 256 elements

include "../../lib/macros.mc"

let var_vecsize = let_ "vecsize" (tyint_) (int_ 256)

let defsize_ = bindall_ [
  var_vecsize
]
