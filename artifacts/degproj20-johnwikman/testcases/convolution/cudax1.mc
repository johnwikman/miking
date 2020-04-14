-- Convolution using: cudaInitExplicitEpt 1 (size) (args...)

include "../../lib/macros.mc"

let defspecific_ = bindall_ [
  let_ "matRes" (tymatrixf_) (
    (cudainitxpept_ (int_ 1) -- elemPerThread
                    (muli_ (var_ "resRows") (var_ "resCols")) -- size
                    (app4f_ (var_ "convolute17Worker")
                            (var_ "filter17")
                            (var_ "resRows")
                            (var_ "resCols")
                            (var_ "matA")))
  )
]
