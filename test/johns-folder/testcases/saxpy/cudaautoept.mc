-- SAXPY using using: cudaMapi (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyint_) (
    (cudamapiautoept_ (app2f_ (var_ "saxpy")
                              (var_ "scalarA")
                              (var_ "vecY"))
                      (var_ "vecX"))
  )
