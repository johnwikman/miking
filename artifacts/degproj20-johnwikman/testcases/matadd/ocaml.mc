-- Matrix addition using OCaml's: Array.init (size) (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "matAplusB" (tymatrixf_) (
    (app2f_ (var_ "seqInit") -- should resolve to OCaml's Array.init
            (muli_ (var_ "mat_rows") (var_ "mat_cols")) -- size
            (app2f_ (var_ "matrixAddfWorker")
                    (var_ "matA")
                    (var_ "matB")))
  )
