-- Nearest Neighbour distances using OCaml's: Array.init (size) (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "distances" (tyseq_ tyfloat_) (
    (app2f_ (var_ "seqInit") -- should resolve to OCaml's Array.init
            (var_ "numPoints"))
            (app4f_ (var_ "nnDistance")
                    (var_ "pX")
                    (var_ "pY")
                    (var_ "pointsX")
                    (var_ "pointsY"))
  )
