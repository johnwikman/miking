-- ATAX using: cudaInitExplicitEpt 1 (size) (args...)

include "../../lib/macros.mc"

let defspecific_ = bindall_ [
  let_ "matATA" (tymatrixi_) (
    (cudainit_ (int_ 1) -- elemPerThread
               (muli_ (var_ "matA_cols") (var_ "matA_cols")) -- size = outerDim^2
               (app3f_ (var_ "matrixATAWorker")
                       (var_ "matA_rows")
                       (var_ "matA_cols")
                       (var_ "matA")))
  ),
  let_ "vecATAx" (tymatrixi_) (
    (cudainit_ (int_ 1) -- elemPerThread
               (muli_ (var_ "matA_cols") (var_ "vecX_cols")) -- size
               (app5f_ (var_ "matrixMuliWorker")
                       (var_ "matA_cols") -- inner dim
                       (var_ "matA_cols")
                       (var_ "vecX_cols")
                       (var_ "matATA")
                       (var_ "vecX")))
  )
]
