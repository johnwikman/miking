-- Matrix addition using: cudaInitPredictive (size) (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "matAplusB" (tymatrixf_) (
    (cudainitxpept_ (int_ 1) -- elemPerThread
                    (muli_ (var_ "mat_rows") (var_ "mat_cols")) -- size
                    (app2f_ (var_ "matrixAddfWorker")
                            (var_ "matA")
                            (var_ "matB")))
  )
