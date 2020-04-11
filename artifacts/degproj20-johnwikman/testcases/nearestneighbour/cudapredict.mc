-- Nearest Neighbour distances using cudaInitPredictive (size) (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "distances" (tyseq_ tyfloat_) (
    (cudainitpredictive_ (var_ "numPoints"))
                         (app4f_ (var_ "nnDistance")
                                 (var_ "pX")
                                 (var_ "pY")
                                 (var_ "pointsX")
                                 (var_ "pointsY"))
  )
