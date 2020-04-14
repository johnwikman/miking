-- Vecadd using using: cudaInitPredictive (size) (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyfloat_) (
    (cudainitpredictive_ (var_ "vecsize")
                         (app2f_ (var_ "vecaddf")
                                 (var_ "vecX")
                                 (var_ "vecY")))
  )
