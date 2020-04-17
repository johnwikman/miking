-- Array of 65536 elements

include "../../lib/macros.mc"

let var_vecsize = let_ "vecsize" (tyint_) (int_ 65536)

let defsize_ = bindall_ [
  var_vecsize
]
