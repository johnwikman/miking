-- GCDsum using using: cudaMapPredictive (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyint_) (
    (cudamappredictive_ (app2f_ (var_ "gcdsum")
                                (var_ "vecY")
                                (var_ "vecsize"))
                        (var_ "vecX"))
  )
