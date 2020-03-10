-- matmul.mc

include "codegen/ocaml.mc"
include "lib/std.mc"
include "lib/io.mc"
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

-- TEST TO BE WRITTEN

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()
