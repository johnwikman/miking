-- Array of 50million elements

include "../../lib/macros.mc"

let var_vecsize = let_ "vecsize" (tyint_) (int_ 50000000)

let defsize_ = bindall_ [
  var_vecsize
]
