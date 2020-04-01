-- SAXPY using OCaml's: Array.map (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyint_) (
    (app2f_ (var_ "map") -- should resolve to OCaml's Array.map
            (app2f_ (var_ "saxpy")
                    (var_ "scalarA")
                    (var_ "scalarY"))
            (var_ "vecX"))
  )
