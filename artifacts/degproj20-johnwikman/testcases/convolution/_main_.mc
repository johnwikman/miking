-- Convolution, applied in the X-direction with a step size of 1 and window size of 17

include "../../codegen/ocaml.mc"
include "../../lib/std.mc"
include "../../lib/io.mc"
include "../../lib/macros.mc"
include "../../lib/matrix.mc"
include "../../lib/benchmark-utils.mc"

-- This should define the following variables: matA_rows, matA_cols
-- Should be included by defsize_
include "_size_.mc"

-- This should provide the specific method of performing the convolution
-- Should define the variables matRes which contains the result
-- Should be included by defspecific_
include "_specific_.mc"

let func_matAinitfun_v3 =
  let_ "matAinitfun_v3" (tyarrows_ [tyint_, tyint_, tyfloat_]) (
    lam_ "row" (tyint_) (
      lam_ "col" (tyint_) (
        subf_ (divf_ (int2float_ (modi_ (muli_ (var_ "row") (var_ "row"))
                                        (addi_ (var_ "col") (int_ 17))))
                     (float_ 3.991))
              (float_ 0.014)
      )
    )
  )

let func_filter17initfun =
  let_ "filter17initfun" (tyarrow_ tyint_ tyfloat_) (
    lam_ "i" (tyint_) (
      subf_ (float_ 1.25)
            (divf_ (float_ 1.0)
                   (addf_ (int2float_ (var_ "i"))
                          (float_ 1.0)))
    )
  )

let func_convolute17Worker =
  let_ "convolute17Worker" (tyarrows_ [tyseq_ tyfloat_, tyint_, tyint_, tyseq_ tyfloat_, tyint_, tyfloat_]) (
    lam_ "filter" (tyseq_ tyfloat_) (
      lam_ "rows" (tyint_) (
        lam_ "cols" (tyint_) (
          lam_ "mat" (tyseq_ tyfloat_) (
            lam_ "idx" (tyint_) (
              bindall_ [
                let_ "row" (tyint_) (divi_ (var_ "idx") (var_ "cols")),
                let_ "col" (tyint_) (modi_ (var_ "idx") (var_ "cols")),

                let_ "originalRows" (tyint_) (var_ "rows"),
                let_ "originalCols" (tyint_) (addi_ (var_ "cols") (int_ 16)),

                let_ "originalRow" (tyint_) (var_ "row"),
                let_ "originalCol" (tyint_) (addi_ (var_ "col") (int_ 8)),
                let_ "originalOffset" (tyint_) (subi_ (addi_ (muli_ (var_ "originalRow") (var_ "originalCols"))
                                                             (var_ "originalCol"))
                                                      (int_ 8)),
                reclets_add "work" (tyarrows_ [tyfloat_, tyint_, tyfloat_]) (
                  lam_ "acc" (tyfloat_) (
                    lam_ "i" (tyint_) (
                      if_ (eqi_ (var_ "i") (int_ 17))
                          (var_ "acc")
                          (app2f_ (var_ "work")
                                  (addf_ (var_ "acc")
                                         (mulf_ (nth_ (var_ "mat") (addi_ (var_ "originalOffset") (var_ "i")))
                                                (nth_ (var_ "filter") (var_ "i"))))
                                  (addi_ (var_ "i") (int_ 1)))
                    )
                  )
                ) (reclets_empty),
                app2f_ (var_ "work")
                       (float_ 0.0)
                       (int_ 0)
              ]
            )
          )
        )
      )
    )
  )

let var_matA =
  let_ "matA" (tymatrixf_) (
    app3f_ (var_ "matrixInitf")
           (var_ "matA_rows")
           (var_ "matA_cols")
           (var_ "matAinitfun_v3")
  )

let var_filter17 =
  let_ "filter17" (tymatrixf_) (
    app2f_ (var_ "seqInit")
           (int_ 17)
           (var_ "filter17initfun")
  )

let var_resRows = let_ "resRows" (tyint_) (var_ "matA_rows")
let var_resCols = let_ "resCols" (tyint_) (subi_ (var_ "matA_cols") (int_ 16)) -- 8 elements on the left and 8 elements on the right will not have their own mapping


let defcommon_ = bindall_ [
  defsize_,
  func_matAinitfun_v3,
  func_filter17initfun,
  func_convolute17Worker,
  var_matA,
  var_filter17,
  var_resRows,
  var_resCols
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
let prog = bind_ prog libmatrixf_ in
let prog = bind_ prog defcommon_ in

------- Perform Convolution -------
--let prog = bind_ prog defspecific_ in
-----------------------------------

------- Output Verification -------
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nfilter17:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app3f_ (var_ "printMatrixf")
--           (int_ 1)
--           (int_ 17)
--           (var_ "filter17")
--  )) in
--
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatA:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app3f_ (var_ "printMatrixf")
--           (var_ "matA_rows")
--           (var_ "matA_cols")
--           (var_ "matA")
--  )) in
--
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatRes:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app3f_ (var_ "printMatrixf")
--           (var_ "resRows")
--           (var_ "resCols")
--           (var_ "matRes")
--  )) in
-----------------------------------

------- Benchmark Convolution -------
let bm =
  benchmark_ {bmparams_ with iters = 15}
             defspecific_
in
let prog = bind_ prog bm in
-------------------------------------

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()


