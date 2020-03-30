-- Array of 500 thousand elements

include "../../lib/macros.mc"

let var_vecsize = let_ "vecsize" (tyint_) (int_ 500000)

let defsize_ = bindall_ [
  var_vecsize
]
