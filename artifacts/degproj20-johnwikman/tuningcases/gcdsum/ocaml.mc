-- GCDsum using OCaml's: Array.map (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyint_) (
    (app2f_ (var_ "map") -- should resolve to OCaml's Array.map
    	    (app2f_ (var_ "gcdsum")
                    (var_ "vecY")
                    (var_ "vecsize"))
            (var_ "vecX"))
  )
