-- fibonacci.mc

include "codegen/ocaml.mc"
include "lib/std.mc"
include "lib/io.mc"
include "lib/arith.mc"

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
let prog = bind_ prog libarith_ in

-- Fibonacci (cudaMapidx)
let prog = bind_ prog (let_ "res" (tyseq_ tyint_) (cudainit_ (int_ 16)
                                                             (int_ 48)
                                                             (var_ "fib"))) in

let prog = bind_ prog (let_ "_" (tyunit_) (app2f_ (var_ "printintarr")
                                                  (str_ "cudaMapidx 48 fib result")
                                                  (var_ "res"))) in

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()
