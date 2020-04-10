-- Saxpysingle: vec(s) = a*vec(x) + y

include "../../codegen/ocaml.mc"
include "../../lib/std.mc"
include "../../lib/io.mc"
include "../../lib/macros.mc"
include "../../lib/benchmark-utils.mc"

-- This determines how many times that saxpy should be applied.
-- Unlike other options, this contains a direct number instead of an AST node.
include "_iter_.mc"

-- This should define the vecsize variable
-- Included by binding defsize_
include "_size_.mc"

-- This should provide the specific saxpy method, binding this multiple times
-- should result in iteration.
-- Should define the variable vecS which contains the result
-- Included by binding defspecific_
include "_specific_.mc"

let func_saxpy =
  let_ "saxpy" (tyarrows_ [tyfloat_, tyfloat_, tyfloat_, tyfloat_]) (
    lam_ "a" (tyfloat_) (
      lam_ "y" (tyfloat_) (
        lam_ "xelem" (tyfloat_) (
          addf_ (mulf_ (var_ "a")
                       (var_ "xelem"))
                (var_ "y")
        )
      )
    )
  )

let func_vecXinitfun =
  let_ "vecXinitfun" (tyarrows_ [tyint_, tyfloat_]) (
    lam_ "i" (tyint_) (
      int2float_ (modi_ (muli_ (muli_ (var_ "i") (var_ "i"))
                               (subi_ (int_ 500000000) (var_ "i")))
                        (int_ 5073))
    )
  )

let var_scalarA =
  let_ "scalarA" (tyfloat_) (float_ 239.0)

let var_vecX =
  let_ "vecX" (tyarrows_ [tyseq_ tyfloat_]) (
    (app2f_ (var_ "seqInit") -- should resolve to OCaml's Array.init
            (var_ "vecsize")
            (var_ "vecXinitfun"))
  )

let var_scalarY =
  let_ "scalarY" (tyfloat_) (float_ 19.0)

let defcommon_ = bindall_ [
  defsize_,
  func_saxpy,
  func_vecXinitfun,
  var_scalarA,
  var_vecX,
  var_scalarY
]

mexpr
use MExprCGOCaml in

if neqi (length argv) 4 then
  let _ = dprint argv in
  error "Must specify a target directory."
else
  -- carry on
let targetdir = nth argv 3 in

let prog = libstd_ in
let prog = bind_ prog libio_ in
let prog = bind_ prog defcommon_ in

------- Perform Saxpy (repeated based on iteration setting) -------
--let prog = bindall_ (cons prog (makeseq defiterations_ defspecific_)) in
-------------------------------------------------------------------

------- Output Verification -------
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nvecS:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app2f_ (var_ "printSeqf")
--           (var_ "vecsize")
--           (var_ "vecS")
--  )) in
-----------------------------------

------- Benchmark Saxpy -------
let bm =
  benchmark_ {bmparams_ with iters = defiterations_}
             defspecific_
in
let prog = bind_ prog bm in
-------------------------------

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()
