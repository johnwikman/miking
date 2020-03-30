-- Array of 1024 elements

include "../../lib/macros.mc"

let var_vecsize = let_ "vecsize" (tyint_) (int_ 1024)

let defsize_ = bindall_ [
  var_vecsize
]
