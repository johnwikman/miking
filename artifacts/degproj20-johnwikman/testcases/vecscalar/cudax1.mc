-- Vecscalar using: cudaMapExplicitEpt 1 (args...)

include "../../lib/macros.mc"

let defspecific_ =
  let_ "vecS" (tyseq_ tyfloat_) (
    (cudamapxpept_ (int_ 1)
    	           (app1f_ (var_ "vecscalef")
                           (var_ "scalarA"))
                   (var_ "vecX"))
  )
