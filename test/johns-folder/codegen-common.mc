-- Common definitions for code generations

include "mexpr/ast.mc"
include "mexpr/pprint.mc"
include "string.mc"

type StringSet = [String]

let strset_new : StringSet = []
let strset_in : String -> StringSet -> Bool = lam s. lam ss.
    any (eqstr s) ss
let strset_add : String -> StringSet -> StringSet = lam s. lam ss.
    if strset_in s ss
    then ss
    else cons s ss
let strset_union : StringSet -> StringSet -> StringSet = lam ss1. lam ss2.
    foldl (lam ssacc. lam s. strset_add s ssacc) ss1 ss2

-- ocamlcodegen return type
-- opens: The open expressions such as in 'open Printf'
-- code: The main ocaml code
-- externs: The external function definitions (OCaml Code)
-- devicefuncs: Code that make up the GPU functions (CUDA C++ Code)
-- hostfuncs: Code that starts the GPU functions (CUDA C++)
type CodegenRet = {opens          : StringSet,
                   code           : String,
                   externs        : StringSet,
                   devicefuncs    : StringSet,
                   globalfuncs    : StringSet,
                   hostprototypes : StringSet,
                   hostfuncs      : StringSet}

let cgr_new = {opens = strset_new,
               code = "",
               externs = strset_new,
               devicefuncs = strset_new,
               globalfuncs = strset_new,
               hostprototypes = strset_new,
               hostfuncs = strset_new}
let cgr_merge = lam newcode. lam cgrs.
    let mergefun = lam acc. lam cgr.
        {{{{{{acc with opens = strset_union cgr.opens acc.opens}
                  with externs = strset_union cgr.externs acc.externs}
                  with devicefuncs = strset_union cgr.devicefuncs acc.devicefuncs}
                  with globalfuncs = strset_union cgr.globalfuncs acc.globalfuncs}
                  with hostprototypes = strset_union cgr.hostprototypes acc.hostprototypes}
                  with hostfuncs = strset_union cgr.hostfuncs acc.hostfuncs}
    in
    {foldl mergefun cgr_new cgrs with code = newcode}

-- indent: The indentation level on the OCaml code
-- env: Lookup for bound expressions.
type CodegenState = {indent      : Int,
                     env         : [{key : String, value : Expr}],
                     cudagentype : String}

let cgs_new = {indent = 0, env = [], cudagentype = "none"}
let cgs_envAdd = lam key. lam value. lam state.
    {state with env = cons {key = key, value = value} state.env}

let cgsspacing = lam state. makeseq state.indent ' '
let cgsnewline = lam state. concat "\n" (cgsspacing state)

let cgsincri = lam i. lam state. {state with indent = addi state.indent i}
let cgsincr = lam state. cgsincri 4 state