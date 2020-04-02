-- GCDsum: vecS = [sum(gcd(x,y) for y in vecY) for x in vecX]

include "../../codegen/ocaml.mc"
include "../../lib/std.mc"
include "../../lib/io.mc"
include "../../lib/macros.mc"

-- This determines how many times that gcdsum should be applied.
-- Unlike other options, this contains a direct number instead of an AST node.
include "_iter_.mc"

-- This should define the vecsize variable
-- Included by binding defsize_
include "_size_.mc"

-- This should provide the specific gcdsum method
-- Should define the variable vecS which contains the result
-- Included by binding defspecific_
include "_specific_.mc"


let func_gcd =
  reclets_add "gcd" (tyarrows_ [tyint_, tyint_, tyint_]) (
    lam_ "a" (tyint_) (
      lam_ "b" (tyint_) (
        if_ (eqi_ (var_ "b") (int_ 0))
            (var_ "a")
            (app2f_ (var_ "gcd")
                    (var_ "b")
                    (modi_ (var_ "a") (var_ "b")))
      )
    )
  ) (reclets_empty)

let func_gcdsum =
  let_ "gcdsum" (tyarrows_ [tyseq_ tyint_, tyint_, tyint_, tyint_]) (
    lam_ "yvec" (tyseq_ tyint_) (
      lam_ "n" (tyint_) (
        lam_ "x" (tyint_) (
          bindall_ [
            reclets_add "recloop" (tyarrows_ [tyint_, tyint_, tyint_]) (
              lam_ "acc" (tyint_) (
                lam_ "i" (tyint_) (
                  if_ (eqi_ (var_ "i") (var_ "n"))
                      (var_ "acc")
                      (app2f_ (var_ "recloop")
                              (addi_ (var_ "acc") -- acc += gcd(x,y[i])
                                     (app2f_ (var_ "gcd")
                                             (var_ "x")
                                             (nth_ (var_ "yvec") (var_ "i"))))
                              (addi_ (var_ "i") (int_ 1)))
                )
              )
            ) (reclets_empty),
            app2f_ (var_ "recloop")
                   (int_ 0)
                   (int_ 0)
          ]
        )
      )
    )
  )

let func_vecXinitfun =
  let_ "vecXinitfun" (tyarrows_ [tyint_, tyint_]) (
    lam_ "i" (tyint_) (
      modi_ (muli_ (subi_ (int_ 51897342)
                          (var_ "i"))
                   (addi_ (int_ 1)
                          (var_ "i")))
            (int_ 74)
    )
  )

let func_vecYinitfun =
  let_ "vecYinitfun" (tyarrows_ [tyint_, tyint_]) (
    lam_ "i" (tyint_) (
      modi_ (muli_ (subi_ (int_ 923487321)
                          (var_ "i"))
                   (addi_ (int_ 1)
                          (var_ "i")))
            (int_ 53)
    )
  )

let var_vecX =
  let_ "vecX" (tyarrows_ [tyseq_ tyint_]) (
    (app2f_ (var_ "seqInit") -- should resolve to OCaml's Array.init
            (var_ "vecsize")
            (var_ "vecXinitfun"))
  )

let var_vecY =
  let_ "vecY" (tyarrows_ [tyseq_ tyint_]) (
    (app2f_ (var_ "seqInit")
            (var_ "vecsize")
            (var_ "vecYinitfun"))
  )

let defcommon_ = bindall_ [
  defsize_,
  func_gcd,
  func_gcdsum,
  func_vecXinitfun,
  func_vecYinitfun,
  var_vecX,
  var_vecY
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

------- Perform GCDsum -------
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
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nvecY:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app2f_ (var_ "printSeqi")
--           (var_ "vecsize")
--           (var_ "vecY")
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
