-- matmul.mc

include "codegen/ocaml.mc"
include "lib/std.mc"
include "lib/io.mc"
include "lib/matmul.mc"
include "lib/matrix.mc"

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
let prog = bind_ prog libmatrix_ in

let prog = bind_ prog (let_ "matA" (tymatrixi_) (seq_ [int_ 1, int_ 3, int_ 5,
                                                       int_ 2, int_ 4, int_ 6])) in
let prog = bind_ prog (let_ "matA_rows" (tyint_) (int_ 2)) in
let prog = bind_ prog (let_ "matA_cols" (tyint_) (int_ 3)) in

let prog = bind_ prog (let_ "vecX" (tymatrixi_) (seq_ [int_ 11,
                                                       int_ 31,
                                                       int_ 17])) in
let prog = bind_ prog (let_ "vecX_rows" (tyint_) (int_ 3)) in
let prog = bind_ prog (let_ "vecX_cols" (tyint_) (int_ 1)) in

-- Uncomment this to verify output
let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatA:\n"))) in
let prog = bind_ prog (let_ "_" (tyunit_) (
    app3f_ (var_ "printMatrixi")
           (var_ "matA_rows")
           (var_ "matA_cols")
           (var_ "matA")
  )) in

let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nvecX:\n"))) in
let prog = bind_ prog (let_ "_" (tyunit_) (
    app3f_ (var_ "printMatrixi")
           (var_ "vecX_rows")
           (var_ "vecX_cols")
           (var_ "vecX")
  )) in


------- Matrix Multiplication (A^T * A) -------
let prog = bind_ prog (let_ "matATA" (tymatrixi_) (
    (app2f_ (var_ "seqInit") -- should resolve to OCaml's Array.init
            (muli_ (var_ "matA_cols") (var_ "matA_cols")) -- size = outerDim^2
            (app3f_ (var_ "matrixATAWorker")
                    (var_ "matA_rows")
                    (var_ "matA_cols")
                    (var_ "matA")))
  )) in

let prog = bind_ prog (let_ "matATA_rows" (tyint_) (var_ "matA_cols")) in
let prog = bind_ prog (let_ "matATA_cols" (tyint_) (var_ "matA_cols")) in

-- Uncomment this to verify output
let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatATA:\n"))) in
let prog = bind_ prog (let_ "_" (tyunit_) (
    app3f_ (var_ "printMatrixi")
           (var_ "matATA_rows")
           (var_ "matATA_cols")
           (var_ "matATA")
  )) in
-------------------------------------


------- Matrix Multiplication (ATA * x) -------
let prog = bind_ prog (let_ "matATAx" (tymatrixi_) (
    (app2f_ (var_ "seqInit") -- should resolve to OCaml's Array.init
            (muli_ (var_ "matATA_rows") (var_ "vecX_cols")) -- size
            (app5f_ (var_ "matrixMuliWorker")
                    (var_ "matATA_cols") -- inner dim
                    (var_ "matATA_rows")
                    (var_ "vecX_cols")
                    (var_ "matATA")
                    (var_ "vecX")))
  )) in

let prog = bind_ prog (let_ "matATAx_rows" (tyint_) (var_ "matATA_rows")) in
let prog = bind_ prog (let_ "matATAx_cols" (tyint_) (var_ "vecX_cols")) in

-- Uncomment this to verify output
let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatATAx:\n"))) in
let prog = bind_ prog (let_ "_" (tyunit_) (
    app3f_ (var_ "printMatrixi")
           (var_ "matATAx_rows")
           (var_ "matATAx_cols")
           (var_ "matATAx")
  )) in
-------------------------------------

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()
