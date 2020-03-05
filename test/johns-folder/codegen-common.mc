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
type CodegenState = {indent : Int,
                     env    : [{key : String, value : Expr}]}

let cgs_new = {indent = 0, env = []}
let cgs_envAdd = lam key. lam value. lam state.
    {state with env = cons {key = key, value = value} state.env}
let cgs_envLookup = lam key. lam state.
    match find (lam e. eqstr key e.key) state.env with Some t then
      t.value
    else
      let errstr = strJoin "" ["Could not find a binding for \"", key, "\""] in
      error errstr

let cgsspacing = lam state. makeseq state.indent ' '
let cgsnewline = lam state. concat "\n" (cgsspacing state)

let cgsincri = lam i. lam state. {state with indent = addi state.indent i}
let cgsincr = lam state. cgsincri 4 state

lang FloatCGExt
    syn Const =
    | CFloat {val : Float}

    sem getConstStringCode (indent : Int) =
    | CFloat f -> float2string f.val
end

lang ArithIntCGExt = ArithIntAst
    syn Const =
    | CModi {}
    | CDivi {}
    | CNegi {}

    sem getConstStringCode (indent : Int) =
    | CModi _ -> "modi"
    | CDivi _ -> "divi"
    | CNegi _ -> "negi"
end

lang ArithFloatCGExt
    syn Const =
    | CAddf {}
    | CSubf {}
    | CMulf {}
    | CDivf {}

    sem getConstStringCode (indent : Int) =
    | CAddf _ -> "addf"
    | CSubf _ -> "subf"
    | CMulf _ -> "mulf"
    | CDivf _ -> "divf"
end

lang CharCGExt = CharAst
    syn Const =
    | CChar2int {}
    | CInt2char {}

    sem getConstStringCode (indent : Int) =
    | CChar2int _ -> "char2int"
    | CInt2char _ -> "int2char"
end

lang SeqCGExt = SeqAst
    syn Const =
    | CLength {}
    | CCons {}
    | CConcat {}
    | CSlice {}
    | CReverse {}
    | CMakeseq {}

    sem getConstStringCode (indent : Int) =
    | CLength _ -> "length"
    | CCons _ -> "cons"
    | CConcat _ -> "concat"
    | CSlice _ -> "slice"
    | CReverse _ -> "reverse"
    | CMakeseq _ -> "makeseq"
end

lang ArithTypeCGExt
    syn Type =
    | TyFloat {}

    sem getTypeStringCode (indent : Int) =
    | TyFloat _ -> "Float"
end

lang CUDACGExt
    syn Expr =
    | TmCUDAMap {elemPerThread : Int,
                 includeIndexArg : Bool,
                 func : Expr,
                 array : Expr}

    sem pprintCode (indent : Int) =
    | TmCUDAMap t ->
      let suffix = if t.includeIndexArg then "i" else "" in
      strJoin "" [
        "cudaMap", suffix, " ", int2string t.elemPerThread,
        " (", pprintCode indent t.func, ")",
        " (", pprintCode indent t.array, ")"
      ]
end

lang MainCGExt
    syn Expr =
    | TmMain {body : Expr}

    syn Const =
    | CPrint {}

    sem getConstStringCode (indent : Int) =
    | CPrint _ -> "print"
end

lang MExprCGExt = MExprAst + FloatCGExt + ArithIntCGExt + ArithFloatCGExt +
                  CharCGExt + SeqCGExt + ArithTypeCGExt + CUDACGExt +
                  MainCGExt +
                  DynTypeAst + UnitTypeAst + CharTypeAst + SeqTypeAst +
                  TupleTypeAst + RecordTypeAst + DataTypeAst + ArithTypeAst +
                  BoolTypeAst + AppTypeAst
