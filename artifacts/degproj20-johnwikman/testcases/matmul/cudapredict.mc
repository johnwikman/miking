-- Matrix multiplcation using: cudaInitPredictive (size) (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "matAxB" (tymatrixi_) (
    (cudainitpredictive_ (muli_ (var_ "matA_rows") (var_ "matB_cols")) -- size
                         (app5f_ (var_ "matrixMuliWorker")
                                 (var_ "innerDim")
                                 (var_ "matA_rows")
                                 (var_ "matB_cols")
                                 (var_ "matA")
                                 (var_ "matB")))
  )
