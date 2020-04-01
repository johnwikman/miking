-- SAXPY using using: cudaMapiExplicitEpt 1 (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyint_) (
    (cudamapixpept_ (int_ 1) -- elemPerThread
                    (app2f_ (var_ "saxpy")
                            (var_ "scalarA")
                            (var_ "vecY"))
                    (var_ "vecX"))
  )
