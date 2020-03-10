-- saxpy.mc

include "codegen/ocaml.mc"
include "lib/stdlib.mc"
include "lib/testlib.mc"

mexpr
use MExprCGOCaml in

if neqi (length argv) 4 then
	let _ = dprint argv in
	error "Must specify a target directory."
else
	-- carry on
let targetdir = nth argv 3 in

let prog = stdlib_ in
let prog = bind_ prog testlibcuda_ in

-- SAXPY (Int)
let prog = bind_ prog (let_ "res" (tyseq_ tyint_) (app3f_ (var_ "mapcuda_saxpy_int")
                                                          (int_ 17)
                                                          (int_ 11)
                                                          (seq_ [int_ 15, int_ 1]))) in

let prog = bind_ prog (let_ "_" (tyunit_) (app2f_ (var_ "printintarr")
                                                  (str_ "saxpy 17 11 [15, 1] result")
                                                  (var_ "res"))) in

-- SAXPY (float)
let prog = bind_ prog (let_ "res" (tyseq_ tyfloat_) (app1f_ (var_ "mapcuda_id2f_ignore2nd")
                                                            (makeseq_ (int_ 5000) (int_ 0)))) in

let prog = bind_ prog (let_ "res" (tyseq_ tyfloat_) (app3f_ (var_ "mapcuda_saxpy_float")
                                                            (float_ 17.0)
                                                            (float_ 11.0)
                                                            (var_ "res"))) in

--let prog = bind_ prog (let_ "res" (tyseq_ tyfloat_) (app2f_ (var_ "Array.map")
--                                                            (app2f_ (var_ "saxpy_float_single")
--                                                                    (float_ 17.0)
--                                                                    (float_ 11.0))
--                                                            (var_ "res"))) in

--let prog = bind_ prog (let_ "_" (tyunit_) (app2f_ (var_ "printfloatarr")
--                                                  (str_ "saxpy 17.0 11.0 \"res\" result")
--                                                  (var_ "res"))) in

-- SAXPY (Int - mapfull)
let prog = bind_ prog (let_ "res" (tyseq_ tyint_) (app3f_ (var_ "mapcuda_saxpy_intfull")
                                                          (int_ 17)
                                                          (seq_ [int_ 15, int_ 1])
                                                          (seq_ [int_ 9, int_ 8]))) in

let prog = bind_ prog (let_ "_" (tyunit_) (app2f_ (var_ "printintarr")
                                                  (str_ "saxpy_mapfull 17 (a) [15, 1] (x) [9, 8] (y) result")
                                                  (var_ "res"))) in

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()
