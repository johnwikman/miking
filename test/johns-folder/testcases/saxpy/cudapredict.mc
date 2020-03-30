-- SAXPY using using: cudaMapiPredictive (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyint_) (
    (cudamapipredictive_ (app2f_ (var_ "saxpy")
                                 (var_ "scalarA")
                                 (var_ "vecY"))
                         (var_ "vecX"))
  )
