-- GCDsum using using: cudaMapExplicitEpt (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyint_) (
    (cudamapxpept_ (int_ 1) -- elemPerThread
    	           (app2f_ (var_ "gcdsum")
                           (var_ "vecY")
                           (var_ "vecsize"))
                   (var_ "vecX"))
  )
