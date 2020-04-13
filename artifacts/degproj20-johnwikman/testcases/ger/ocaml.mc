-- GER using OCaml's: Array.mapi (args...)

include "../../lib/macros.mc"

let defspecific_ = bindall_ [
  let_ "matxyA" (tymatrixf_) (
    (app2f_ (var_ "mapi") -- should resolve to OCaml's Array.mapi
            (app4f_ (var_ "gerWorkerf")
                    (var_ "matA_rows")
                    (var_ "matA_cols")
                    (var_ "vecX")
                    (var_ "vecY"))
            (var_ "matA"))
  )
]
