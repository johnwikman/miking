-- Array of 2048 elements

include "../../lib/macros.mc"

let var_vecsize = let_ "vecsize" (tyint_) (int_ 2048)

let defsize_ = bindall_ [
  var_vecsize
]
