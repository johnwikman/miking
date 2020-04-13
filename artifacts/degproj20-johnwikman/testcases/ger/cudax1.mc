-- GER using: cudaMapiExplicitEpt 1 (args...)

include "../../lib/macros.mc"

let defspecific_ = bindall_ [
  let_ "matxyA" (tymatrixf_) (
    (cudamapixpept_ (int_ 1) -- elemPerThread
                    (app4f_ (var_ "gerWorkerf")
                            (var_ "matA_rows")
                            (var_ "matA_cols")
                            (var_ "vecX")
                            (var_ "vecY"))
                    (var_ "matA"))
  )
]
