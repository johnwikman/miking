-- Matrix addition using: cudaInitExplicitEpt 1 (size) (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "matAplusB" (tymatrixf_) (
    (cudainitpredictive_  (muli_ (var_ "mat_rows") (var_ "mat_cols")) -- size
                          (app2f_ (var_ "matrixAddfWorker")
                                  (var_ "matA")
                                  (var_ "matB")))
  )
