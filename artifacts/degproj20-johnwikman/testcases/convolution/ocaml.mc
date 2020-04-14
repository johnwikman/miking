-- Convolution using OCaml's: Array.init (size) (args...)

include "../../lib/macros.mc"

let defspecific_ = bindall_ [
  let_ "matRes" (tymatrixf_) (
    (app2f_ (var_ "seqInit") -- should resolve to OCaml's Array.init
            (muli_ (var_ "resRows") (var_ "resCols")) -- size
            (app4f_ (var_ "convolute17Worker")
                    (var_ "filter17")
                    (var_ "resRows")
                    (var_ "resCols")
                    (var_ "matA")))
  )
]
