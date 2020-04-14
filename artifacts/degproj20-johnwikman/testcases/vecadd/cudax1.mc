-- Vecadd using using: cudaInitExplicitEpt 1 (size) (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyfloat_) (
    (cudainitxpept_ (int_ 1) -- elemPerThread
    	            (var_ "vecsize")
                    (app2f_ (var_ "vecaddf")
                            (var_ "vecX")
                            (var_ "vecY")))
  )
