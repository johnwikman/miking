-- Vecscalar using OCaml's: Array.map (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyfloat_) (
    (app2f_ (var_ "map") -- should resolve to OCaml's Array.map
            (app1f_ (var_ "vecscalef")
                    (var_ "scalarA"))
            (var_ "vecX"))
  )
