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
let prog = bind_ prog libmatmul_ in

-- libmatmul_ binds the following variables:
--  - matArows
--  - matAcols
--  - matA
--  - matBrows
--  - matBcols
--  - matB


------- Matrix Multiplication (CUDA) -------
let prog = bind_ prog (let_ "matAxB" (tymatrixi_) (
    (cudainit_ (int_ 64) -- elemPerThread
               (muli_ (var_ "matA_rows") (var_ "matB_cols")) -- size
               (app5f_ (var_ "matrixMuliWorker")
                       (var_ "matA_cols")
                       (var_ "matA_rows")
                       (var_ "matB_cols")
                       (var_ "matA")
                       (var_ "matB")))
  )) in

let prog = bind_ prog (let_ "matAxB_rows" (tyint_) (var_ "matA_rows")) in
let prog = bind_ prog (let_ "matAxB_cols" (tyint_) (var_ "matB_cols")) in

-- Uncomment this to verify output
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nmatAxB:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app3f_ (var_ "printMatrixi")
--           (var_ "matAxB_rows")
--           (var_ "matAxB_cols")
--           (var_ "matAxB")
--  )) in
-------------------------------------


let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()
