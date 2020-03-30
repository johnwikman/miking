-- Array of 1 million elements

include "../../lib/macros.mc"

let var_vecsize = let_ "vecsize" (tyint_) (int_ 1000000)

let defsize_ = bindall_ [
  var_vecsize
]
