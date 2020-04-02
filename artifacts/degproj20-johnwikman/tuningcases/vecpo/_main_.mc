-- VecPlusOne: vecS = [x + 1 for x in vecX]

include "../../codegen/ocaml.mc"
include "../../lib/std.mc"
include "../../lib/io.mc"
include "../../lib/macros.mc"

-- This determines how many times that vecpo should be applied.
-- Unlike other options, this contains a direct number instead of an AST node.
include "_iter_.mc"

-- This should define the vecsize variable
-- Included by binding defsize_
include "_size_.mc"

-- This should provide the specific vecpo method
-- Should define the variable vecS which contains the result
-- Included by binding defspecific_
include "_specific_.mc"

let func_plusone =
  let_ "plusone" (tyarrow_ tyint_ tyint_) (
    lam_ "x" (tyint_) (
      addi_ (var_ "x") (int_ 1)
    )
  )

let func_vecXinitfun =
  let_ "vecXinitfun" (tyarrows_ [tyint_, tyint_]) (
    lam_ "i" (tyint_) (
      var_ "i"
    )
  )

let var_vecX =
  let_ "vecX" (tyarrows_ [tyseq_ tyint_]) (
    (app2f_ (var_ "seqInit") -- should resolve to OCaml's Array.init
            (var_ "vecsize")
            (var_ "vecXinitfun"))
  )

let defcommon_ = bindall_ [
  defsize_,
  func_plusone,
  func_vecXinitfun,
  var_vecX
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

------- Perform vecpo -------
let prog = bindall_ (cons prog (makeseq defiterations_ defspecific_)) in
------------------------------

------- Output Verification -------
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nvecX:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app2f_ (var_ "printSeqi")
--           (var_ "vecsize")
--           (var_ "vecX")
--  )) in
--
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nvecS:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app2f_ (var_ "printSeqi")
--           (var_ "vecsize")
--           (var_ "vecS")
--  )) in
-----------------------------------

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()
