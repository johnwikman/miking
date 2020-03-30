-- vecpo using using: cudaMapPredictive (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyint_) (
    (cudamappredictive_ (var_ "plusone")
                        (var_ "vecX"))
  )
