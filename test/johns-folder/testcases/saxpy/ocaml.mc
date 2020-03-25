-- SAXPY using OCaml's: Array.mapi (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyint_) (
    (app2f_ (var_ "mapi") -- should resolve to OCaml's Array.mapi
            (app2f_ (var_ "saxpy")
                    (var_ "scalarA")
                    (var_ "vecY"))
            (var_ "vecX"))
  )
