-- ATAX using OCaml's: Array.init (size) (args...)

include "../../lib/macros.mc"

let defspecific_ = bindall_ [
  let_ "vecAx" (tymatrixf_) (
    (app2f_ (var_ "seqInit") -- should resolve to OCaml's Array.init
            (muli_ (var_ "matA_rows") (var_ "vecX_cols")) -- size
            (app5f_ (var_ "matrixMulfWorker")
                    (var_ "matA_cols") -- inner dim
                    (var_ "matA_rows")
                    (var_ "vecX_cols")
                    (var_ "matA")
                    (var_ "vecX")))
  ),
  let_ "vecAx_rows" (tyint_) (var_ "matA_rows"),
  let_ "vecAx_cols" (tyint_) (var_ "vecX_cols"),
  let_ "vecATAx" (tymatrixf_) (
    (app2f_ (var_ "seqInit") -- should resolve to OCaml's Array.init
            (muli_ (var_ "matA_cols") (var_ "vecAx_cols")) -- size
            (app6f_ (var_ "matrixMulTransposeLhsfWorker")
                    (var_ "matA_rows")
                    (var_ "matA_cols")
                    (var_ "vecAx_rows")
                    (var_ "vecAx_cols")
                    (var_ "matA")
                    (var_ "vecAx")))
  )
]
