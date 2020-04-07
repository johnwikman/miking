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
type CodegenRet = {opens            : StringSet,
                   code             : String,
                   externs          : StringSet,
                   deviceprototypes : StringSet,
                   devicefuncs      : StringSet,
                   globalfuncs      : StringSet,
                   hostprototypes   : StringSet,
                   hostfuncs        : StringSet,
                   deviceNeedsRng   : Bool}

let cgr_new = {opens = strset_new,
               code = "",
               externs = strset_new,
               deviceprototypes = strset_new,
               devicefuncs = strset_new,
               globalfuncs = strset_new,
               hostprototypes = strset_new,
               hostfuncs = strset_new,
               deviceNeedsRng = false}
let cgr_merge = lam newcode. lam cgrs.
    let mergefun = lam acc. lam cgr.
        {{{{{{{{acc with opens = strset_union cgr.opens acc.opens}
                    with externs = strset_union cgr.externs acc.externs}
                    with deviceprototypes = strset_union cgr.deviceprototypes acc.deviceprototypes}
                    with devicefuncs = strset_union cgr.devicefuncs acc.devicefuncs}
                    with globalfuncs = strset_union cgr.globalfuncs acc.globalfuncs}
                    with hostprototypes = strset_union cgr.hostprototypes acc.hostprototypes}
                    with hostfuncs = strset_union cgr.hostfuncs acc.hostfuncs}
                    with deviceNeedsRng = or cgr.deviceNeedsRng acc.deviceNeedsRng}
    in
    {foldl mergefun cgr_new cgrs with code = newcode}

-- indent: The indentation level on the OCaml code
-- env: Lookup for bound expressions.
-- cudaNestedLevel: Used by the CUDA codegen to determine how a Let expression is nested
type CodegenState = {indent        : Int,
                     env           : [{key : String, value : Expr}],
                     cudaNestedLevel : Int}

let cgs_new = {indent = 0, env = [], cudaNestedLevel = 0}
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

lang RecLetsCGExt = RecLetsAst
    -- Reference to a TmRecLets statement, should never be part of the original
    -- AST.
    syn Expr =
    | TmRecLetsRef {ident : String,
                    tpe   : Option,
                    body  : Expr}
end

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
    | CNegf {}
    | CAddf {}
    | CSubf {}
    | CMulf {}
    | CDivf {}
    | CFloorfi {}
    | CCeilfi {}
    | CRoundfi {}
    | CExpf {}
    | CLogf {}
    | CRandUniformf {}
    | CRandNormalf {}
    | CLogpdfNormalf {}
    | CInt2float {}
    | CString2float {}

    sem getConstStringCode (indent : Int) =
    | CNegf _ -> "negf"
    | CAddf _ -> "addf"
    | CSubf _ -> "subf"
    | CMulf _ -> "mulf"
    | CDivf _ -> "divf"
    | CFloorfi _ -> "floorfi"
    | CCeilfi _ -> "ceilfi"
    | CRoundfi _ -> "roundfi"
    | CExpf _ -> "expf"
    | CLogf _ -> "logf"
    | CRandUniformf _ -> "randUniformf"
    | CRandNormalf _ -> "randNormalf"
    | CLogpdfNormalf _ -> "logpdfNormalf"
    | CInt2float _ -> "int2float"
    | CString2float _ -> "string2float"
end

lang CmpCGExt = CmpAst
    syn Const =
    | CNeqi {}
    | CGti {}
    | CGeqi {}
    | CLeqi {}
    | CEqf {}
    | CLtf {}
    | CGtf {}

    sem getConstStringCode (indent : Int) =
    | CNeqi _ -> "neqi"
    | CGti _ -> "gti"
    | CGeqi _ -> "geqi"
    | CLeqi _ -> "leqi"
    | CEqf _ -> "eqf"
    | CLtf _ -> "ltf"
    | CGtf _ -> "gtf"
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
    | CTraverse {} -- (this is not a standard MCore intrinsic)

    sem getConstStringCode (indent : Int) =
    | CLength _ -> "length"
    | CCons _ -> "cons"
    | CConcat _ -> "concat"
    | CSlice _ -> "slice"
    | CReverse _ -> "reverse"
    | CMakeseq _ -> "makeseq"
    | CTraverse _ -> "traverse"
end

lang ArithTypeCGExt
    syn Type =
    | TyFloat {}

    sem getTypeStringCode (indent : Int) =
    | TyFloat _ -> "Float"
end

lang CUDACGExt
    syn Expr =
    | TmCUDAMap {elemPerThread : Expr,
                 autoDetermineParallelization : Bool,
                 autoScanElemPerThread : Bool,
                 includeIndexArg : Bool,
                 onlyIndexArg : Bool,
                 onlyIndexArgSize : Expr,
                 func : Expr,
                 array : Expr,
                 -- metadata used by codegen
                 packedInts : [{pos : Int, val : Expr}],
                 packedFloats : [{pos : Int, val : Expr}],
                 nonPackedArgs : [Expr]}

    sem lamlift (state : LiftState) =
    | TmCUDAMap t ->
      let eptret = lamlift state t.elemPerThread in
      let eptstate = {eptret.0 with env = state.env} in
      let oiasret = lamlift eptstate t.onlyIndexArgSize in
      let oiasstate = {oiasret.0 with env = state.env} in
      let funcret = lamlift oiasstate t.func in
      let funcstate = {funcret.0 with env = state.env} in
      let arrayret = lamlift funcstate t.array in
      let arraystate = {arrayret.0 with env = state.env} in
      (arraystate, TmCUDAMap {{{{t with elemPerThread = eptret.1}
                                   with onlyIndexArgSize = oiasret.1}
                                   with func = funcret.1}
                                   with array = arrayret.1})

    sem lamliftReplaceIdentifiers (newnames : [{ident : String, replacement : Expr}]) =
    | TmCUDAMap t -> TmCUDAMap {{{{t with elemPerThread = lamliftReplaceIdentifiers newnames t.elemPerThread}
                                     with onlyIndexArgSize = lamliftReplaceIdentifiers newnames t.onlyIndexArgSize}
                                     with func = lamliftReplaceIdentifiers newnames t.func}
                                     with array = lamliftReplaceIdentifiers newnames t.array}

    sem pprintCode (indent : Int) =
    | TmCUDAMap t ->
      let fname =
        if t.onlyIndexArg then
          "cudaInit"
        else
          if t.includeIndexArg then
            "cudaMapi"
          else
            "cudaMap"
      in
      let fname =
        if t.autoDetermineParallelization then
          concat fname "Predictive"
        else if t.autoScanElemPerThread then
          concat fname "AutoEpt"
        else
          concat fname "ExplicitEpt"
      in

      let args = [t.func, t.array] in
      let args = if t.onlyIndexArg then cons t.onlyIndexArgSize args else args in
      let args =
        if or t.autoScanElemPerThread t.autoDetermineParallelization then
          args
        else
          cons t.elemPerThread args
      in

      let argstrs = map (lam arg. strJoin "" ["(", pprintCode indent arg, ")"]) args in

      strJoin " " (cons fname argstrs)
end

lang MainCGExt
    syn Expr =
    | TmMain {body : Expr}

    syn Const =
    | CPrint {}
    | CError {}

    sem getConstStringCode (indent : Int) =
    | CPrint _ -> "print"
    | CError _ -> "error"
end

lang MExprCGExt = MExprAst + RecLetsCGExt+ FloatCGExt + ArithIntCGExt +
                  ArithFloatCGExt + CmpCGExt + CharCGExt + SeqCGExt +
                  ArithTypeCGExt + CUDACGExt + MainCGExt +
                  DynTypeAst + UnitTypeAst + CharTypeAst + SeqTypeAst +
                  TupleTypeAst + RecordTypeAst + DataTypeAst + ArithTypeAst +
                  BoolTypeAst + AppTypeAst
