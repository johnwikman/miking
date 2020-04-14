-- Vecadd using OCaml's: Array.init (size) (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyfloat_) (
    (app2f_ (var_ "seqInit") -- should resolve to OCaml's Array.init
    	    (var_ "vecsize")
            (app2f_ (var_ "vecaddf")
                    (var_ "vecX")
                    (var_ "vecY")))
  )
