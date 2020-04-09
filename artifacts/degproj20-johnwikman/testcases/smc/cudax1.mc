-- SMC using using: cudaMapExplicitEpt 1 (args...)
-- SMC using using: cudaInitExplicitEpt 1 (size) (args...)

include "../../lib/macros.mc"

let specificmap_ = cudamapxpept_ (int_ 1) -- elemPerThread

let specificinit_ = cudainitxpept_ (int_ 1) -- elemPerThread
