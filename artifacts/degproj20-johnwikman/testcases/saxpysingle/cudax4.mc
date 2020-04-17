-- SAXPY using using: cudaMapExplicitEpt 4 (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyfloat_) (
    (cudamapxpept_ (int_ 4) -- elemPerThread
                   (app2f_ (var_ "saxpy")
                           (var_ "scalarA")
                           (var_ "scalarY"))
                   (var_ "vecX"))
  )
