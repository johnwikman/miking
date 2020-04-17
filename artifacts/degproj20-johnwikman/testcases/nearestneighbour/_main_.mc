-- Compute the nearest neighbour

-- NOTE: The Lift and Rodinia reference points only measure time for the part
--       that computes the distances, never actually finding the nearest
--       neighbour. This is also reflected in this implementation.
--
--       The variable initialization is the same as that of Lift.

include "../../codegen/ocaml.mc"
include "../../lib/std.mc"
include "../../lib/io.mc"
include "../../lib/macros.mc"
include "../../lib/benchmark-utils.mc"

-- This should define the following variable "numPoints"
-- Should be included by defsize_
include "_size_.mc"

-- This should provide the specific method of computing the nearest neighbour
-- distances. Should define the variable "distances" which contains the result.
-- Should be included by defspecific_
include "_specific_.mc"

let func_nnDistance =
  let_ "nnDistance" (tyarrows_ [tyfloat_, tyfloat_, tyseq_ tyfloat_, tyseq_ tyfloat_, tyint_, tyfloat_]) (
    lam_ "x" (tyfloat_) (
      lam_ "y" (tyfloat_) (
        lam_ "pointsX" (tyseq_ tyfloat_) (
          lam_ "pointsY" (tyseq_ tyfloat_) (
            lam_ "i" (tyint_) (
              bindall_ [
                let_ "dxi" (tyfloat_) (subf_ (var_ "x") (nth_ (var_ "pointsX") (var_ "i"))),
                let_ "dyi" (tyfloat_) (subf_ (var_ "y") (nth_ (var_ "pointsY") (var_ "i"))),
                sqrtf_ (addf_ (mulf_ (var_ "dxi") (var_ "dxi"))
                              (mulf_ (var_ "dyi") (var_ "dyi")))
              ]
            )
          )
        )
      )
    )
  )

let func_pointsXInitFunc =
  let_ "pointsXInitFunc" (tyarrows_ [tyint_, tyfloat_]) (
    lam_ "_" (tyint_) (
      randUniformf_ (float_ (negf 10.0)) (float_ (10.0))
    )
  )

let func_pointsYInitFunc =
  let_ "pointsYInitFunc" (tyarrows_ [tyint_, tyfloat_]) (
    lam_ "_" (tyint_) (
      randUniformf_ (float_ (negf 10.0)) (float_ (10.0))
    )
  )

let var_pointsX =
  let_ "pointsX" (tyseq_ tyfloat_) (
    app2f_ (var_ "seqInit") (var_ "numPoints") (var_ "pointsXInitFunc")
  )

let var_pointsY =
  let_ "pointsY" (tyseq_ tyfloat_) (
    app2f_ (var_ "seqInit") (var_ "numPoints") (var_ "pointsYInitFunc")
  )

let var_pX = let_ "pX" (tyfloat_) (float_ 0.5)
let var_pY = let_ "pY" (tyfloat_) (float_ 1.5)


let defcommon_ = bindall_ [
  defsize_,
  func_nnDistance,
  func_pointsXInitFunc,
  func_pointsYInitFunc,
  var_pointsX,
  var_pointsY,
  var_pX,
  var_pY
]

mexpr
use MExprCGOCaml in

if neqi (length argv) 4 then
  let _ = dprint argv in
  error "Must specify a target directory."
else
  -- carry on
let targetdir = get argv 3 in

let prog = libstd_ in
let prog = bind_ prog libio_ in
let prog = bind_ prog defcommon_ in

------- Benchmark Nearest Neighbour -------
let bm =
  benchmark_ {bmparams_ with iters = 15}
             defspecific_
in
let prog = bind_ prog bm in
------------------------------------------

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()
