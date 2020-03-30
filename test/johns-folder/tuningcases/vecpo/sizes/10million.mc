-- Array of 10 million elements

include "../../lib/macros.mc"

let var_vecsize = let_ "vecsize" (tyint_) (int_ 10000000)

let defsize_ = bindall_ [
  var_vecsize
]
