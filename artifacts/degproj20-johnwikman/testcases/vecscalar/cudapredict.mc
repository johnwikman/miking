-- Vecscalar using: cudaMapPredictive (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyfloat_) (
    (cudamappredictive_ (app1f_ (var_ "vecscalef")
                                (var_ "scalarA"))
                        (var_ "vecX"))
  )
