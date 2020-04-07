-- OCaml versions of map and init

include "../../lib/macros.mc"

let specificmap_ = app2f_ (var_ "map") -- should resolve to Array.map

let specificinit_ = app2f_ (var_ "seqInit") -- should resolve to Array.init
