-- Matrix multiplcation using OCaml's: Array.init (size) (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "matAxB" (tymatrixf_) (
    (app2f_ (var_ "seqInit") -- should resolve to OCaml's Array.init
            (muli_ (var_ "matA_rows") (var_ "matB_cols")) -- size
            (app5f_ (var_ "matrixMulfWorker")
                    (var_ "innerDim")
                    (var_ "matA_rows")
                    (var_ "matB_cols")
                    (var_ "matA")
                    (var_ "matB")))
  )
