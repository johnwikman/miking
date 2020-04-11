-- Nearest Neighbour distances using cudaInitExplicitEpt 1 (size) (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "distances" (tyseq_ tyfloat_) (
    (cudainitxpept_ (int_ 1) -- elemPerThread
                    (var_ "numPoints"))
                    (app4f_ (var_ "nnDistance")
                            (var_ "pX")
                            (var_ "pY")
                            (var_ "pointsX")
                            (var_ "pointsY"))
  )
