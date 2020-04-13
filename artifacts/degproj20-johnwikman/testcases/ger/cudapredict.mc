-- GER using: cudaMapiPredictive (args...)

include "../../lib/macros.mc"

let defspecific_ = bindall_ [
  let_ "matxyA" (tymatrixf_) (
    (cudamapipredictive_ (app4f_ (var_ "gerWorkerf")
                                 (var_ "matA_rows")
                                 (var_ "matA_cols")
                                 (var_ "vecX")
                                 (var_ "vecY"))
                         (var_ "matA"))
  )
]
