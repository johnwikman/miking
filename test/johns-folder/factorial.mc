-- factorial.mc

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

-- let v = 10 in
-- let res = factorial v in
-- let printstr = strJoin "" ["factorial ", int2string v, " = ", int2string res, "\n"] in
-- let _ = print printstr in
let prog = bind_ prog (let_ "v" (tyint_) (int_ 10)) in
let prog = bind_ prog (let_ "res" (tyint_) (app_ (var_ "factorial")
                                                 (var_ "v"))) in
let prog = bind_ prog (let_ "printstr" (tystr_) (
    app2f_ (var_ "strJoin")
           (str_ "")
           (seq_ [str_ "factorial ",
                  app_ (var_ "int2string")
                       (var_ "v"),
                  str_ " = ",
                  app_ (var_ "int2string")
                       (var_ "res"),
                  str_ "\n"])
  )) in

let prog = bind_ prog (let_ "_" (tyunit_) (print_ (var_ "printstr"))) in

-- Factorial (cudaMapi)
let prog = bind_ prog (let_ "factidx" (tyarrows_ [tyint_, tyint_, tyint_]) (
                            lam_ "i" tyint_ (lam_ "ignored_arg" tyint_ (
                                 app1f_ (var_ "factorial") (var_ "i"))))) in

let prog = bind_ prog (let_ "res" (tyseq_ tyint_) (cudamapi_ 8
                                                             (var_ "factidx")
                                                             (makeseq_ (int_ 16) (int_ 0)))) in

let prog = bind_ prog (let_ "_" (tyunit_) (app2f_ (var_ "printintarr")
                                                  (str_ "cudaMapi factidx result")
                                                  (var_ "res"))) in

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()
