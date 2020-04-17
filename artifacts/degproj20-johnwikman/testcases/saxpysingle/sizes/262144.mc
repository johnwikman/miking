-- Array of 262144 elements

include "../../lib/macros.mc"

let var_vecsize = let_ "vecsize" (tyint_) (int_ 262144)

let defsize_ = bindall_ [
  var_vecsize
]
