-- SAXPY using using: cudaMapPredictive (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyint_) (
    (cudamappredictive_ (app2f_ (var_ "saxpy")
                                (var_ "scalarA")
                                (var_ "scalarY"))
                        (var_ "vecX"))
  )
