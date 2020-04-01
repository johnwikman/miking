-- Array of 50 thousand elements

include "../../lib/macros.mc"

let var_vecsize = let_ "vecsize" (tyint_) (int_ 50000)

let defsize_ = bindall_ [
  var_vecsize
]
