-- vecpo using using: cudaMapExplicitEpt (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyint_) (
    (cudamapxpept_ (int_ 1) -- elemPerThread
    	           (var_ "plusone")
                   (var_ "vecX"))
  )
