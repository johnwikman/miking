-- Matrix multiplcation using: cudaInitExplicitEpt 1 (size) (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "matAxB" (tymatrixf_) (
    (cudainitxpept_ (int_ 1) -- elemPerThread
                    (muli_ (var_ "matA_rows") (var_ "matB_cols")) -- size
                    (app5f_ (var_ "matrixMulfWorker")
                            (var_ "innerDim")
                            (var_ "matA_rows")
                            (var_ "matB_cols")
                            (var_ "matA")
                            (var_ "matB")))
  )
