-- OCaml code generator
-- This assumes that Lambda lifting has been performed and all identifiers are globally unique...

--
-- What needs to happen:
--  The lambdas need to appear on the left side of the equals sign

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


let mapi = lam f. lam arr.
  recursive let mapi_helper = lam i. lam arr2.
    if null arr2
    then []
    else cons (f i (head arr2)) (mapi_helper (addi i 1) (tail arr2))
  in
  mapi_helper 0 arr

lang VarOCamlCode = VarAst + LetAst + FunAst + ArithTypeAst
    sem ocamlcodegen (state : CodegenState) =
    | TmVar x -> {cgr_new with code = x.ident}

    sem cudacodegen (state : CodegenState) =
    | TmVar x ->
      let cgs_envLookup = lam key. lam state.
        match find (lam e. eqstr key e.key) state.env with Some t then
          t.value
        else
          let errstr = strJoin "" ["Could not find a binding for \"", key, "\""] in
          error errstr
          -- If not bound, probably an argument. We know since we have done the
          -- lambda lifting that identifiers cannot be shadowed.
          --TmVar {ident = key}
      in
      let v = cgs_envLookup x.ident state in
      match v with TmLet t then
        cudacodegen state v
      else match v with TmLam t then
        {cgr_new with code = t.ident}
      else match v with TmVar t then
        {cgr_new with code = t.ident}
      else
        let _ = dprint v in
        error (strJoin "" ["\nVariable \"", x.ident, "\" is not bound to a TmLet or TmVar"])
end

lang AppOCamlCode = AppAst
    sem ocamlcodegen (state : CodegenState) =
    | TmApp t ->
      let lhs = ocamlcodegen state t.lhs in
      let rhs = ocamlcodegen state t.rhs in
      let newcode = strJoin "" [lhs.code, " (", rhs.code, ")"] in
      cgr_merge newcode [lhs, rhs]

    sem cudacodegen (state : CodegenState) =
    | TmApp t ->
      recursive let unwrap_app = lam cgr. lam argacc. lam e.
        match e with TmApp t1 then
          let rhscgr = cudacodegen state t1.rhs in
          unwrap_app (cgr_merge "" [cgr, rhscgr]) (cons rhscgr.code argacc) t1.lhs
        else
          let lhscgr = cudacodegen state e in
          (cgr_merge "" [cgr, lhscgr], cons lhscgr.code argacc)
      in
      let ret = unwrap_app cgr_new [] (TmApp t) in
      let cgr = ret.0 in
      let func = head ret.1 in
      let args = tail ret.1 in
      let funcall = strJoin "" [func, "(", strJoin ", " args, ")"] in
      {cgr with code = funcall}
end

lang FunOCamlCode = FunAst
    sem ocamlcodegen (state : CodegenState) =
    | TmLam t -> let _ = dprint t in error "\nEncountered an isolated lambda in ocamlcodegen"

    sem cudacodegen (state : CodegenState) =
    | TmLam t -> let _ = dprint t in error "\nEncountered an isolated lambda in cudacodegen"
end

lang LetOCamlCode = LetAst + FunAst + ConstAst + UnitAst
    sem ocamlcodegen (state : CodegenState) =
    | TmLet t ->
      -- Find all the chained lambdas
      --   ret.0: The variable names
      --   ret.1: The trailing expression
      --   ret.2: The new internal state
      recursive let chainlambdas = lam acc. lam expr.
        match expr with TmLam t1 then
          chainlambdas (concat acc.0 [t1.ident], cgs_envAdd t1.ident (TmLam t1) acc.1) t1.body
        else
          (acc.0, expr, acc.1)
      in
      let argres = chainlambdas ([], state) t.body in
      let args = argres.0 in
      let letexpr = argres.1 in
      let internalstate = argres.2 in
      -- Set the inexpr of bound expression to be CUnit as we only care about the body when
      -- doing the lookup in the CUDA code
      let instate = cgs_envAdd t.ident (TmLet {t with inexpr = TmConst {val = CUnit ()}}) state in
      let internalret = ocamlcodegen (cgsincr internalstate) letexpr in
      let inexprret = ocamlcodegen instate t.inexpr in
      let newcode = strJoin "" ["let ", t.ident, " ", strJoin " " args, " =", cgsnewline (cgsincr state),
                                internalret.code, cgsnewline state,
                                "in", cgsnewline state, inexprret.code] in
      cgr_merge newcode [internalret, inexprret]

    sem cudacodegen (state : CodegenState) =
    | TmLet t ->
      -- Find all the chained lambdas
      --   ret.0: The variable names
      --   ret.1: The trailing expression
      --   ret.2: The new internal state
      recursive let chainlambdas = lam acc. lam expr.
        match expr with TmLam t1 then
          chainlambdas (concat acc.0 [t1.ident], cgs_envAdd t1.ident (TmLam t1) acc.1) t1.body
        else
          (acc.0, expr, acc.1)
      in
      let argres = chainlambdas ([], state) t.body in
      let args = argres.0 in
      let letexpr = argres.1 in
      let internalstate = argres.2 in
      let ty = state.cudagentype in

      let cudaret = cudacodegen internalstate letexpr in

      let devicebody = strJoin "" [
        "__device__ ", ty, " gpudevice_", t.ident, "(", strJoin ", " (map (lam s. concat ty (cons ' ' s)) args), ")\n",
        "{\n",
        "\treturn ", cudaret.code, ";\n",
        "}"
      ] in
      {cudaret with devicefuncs = strset_add devicebody cudaret.devicefuncs}
end

lang RecLetsOCamlCode = RecLetsAst + LetAst + FunAst + ConstAst + UnitAst
    sem ocamlcodegen (state : CodegenState) =
    | TmRecLets t ->
      -- Create the instate which have access to all the bound expressions
      -- in the recursive scope
      let instate = foldl (lam st. lam b. cgs_envAdd b.ident (TmLet {ident = b.ident, tpe = b.tpe, body = b.body, inexpr = TmConst {val = CUnit ()}}) st) state t.bindings in
      recursive let generatelets = lam genacc. lam l.
        if null l then
          genacc
        else
          -- Find all the chained lambdas
          --   ret.0: The variable names
          --   ret.1: The trailing expression
          --   ret.2: The new internal state
          recursive let chainlambdas = lam acc. lam expr.
            match expr with TmLam t1 then
              chainlambdas (concat acc.0 [t1.ident], cgs_envAdd t1.ident (TmLam t1) acc.1) t1.body
            else
              (acc.0, expr, acc.1)
          in
          let argres = chainlambdas ([], instate) (head l).body in
          let name = (head l).ident in
          let args = argres.0 in
          let letexpr = argres.1 in
          let internalstate = argres.2 in
          let prefix = if null genacc.code then "let rec " else "    and " in
          let letret = ocamlcodegen (cgsincri 4 (cgsincr instate)) letexpr in
          let updatedcode = strJoin "" [
            genacc.code, prefix, name, " ", strJoin " " args, " =",
            cgsnewline (cgsincri 4 (cgsincr instate)), letret.code, cgsnewline instate
          ]
          in
          generatelets (cgr_merge updatedcode [genacc, letret]) (tail l)
      in
      let genret = generatelets cgr_new t.bindings in
      let inret = ocamlcodegen instate t.inexpr in
      let newcode = strJoin "" [genret.code, "in", cgsnewline instate, inret.code] in
      cgr_merge newcode [genret, inret]
end

lang ConstOCamlCode = ConstAst
    sem ocamlconstgen (state : CodegenState) =
    -- intentionally left blank

    sem ocamlcodegen (state : CodegenState) =
    | TmConst c -> ocamlconstgen state c.val

    sem cudacodegen (state : CodegenState) =
    | TmConst c -> cudacodegen state c.val
end

lang UnitOCamlCode = UnitAst
    sem ocamlconstgen (state : CodegenState) =
    | CUnit _ -> {cgr_new with code = "()"}
end

lang IntOCamlCode = IntAst
    sem ocamlconstgen (state : CodegenState) =
    | CInt i -> {cgr_new with code = int2string i.val}

    sem cudacodegen (state : CodegenState) =
    | CInt i -> {cgr_new with code = int2string i.val}
end

lang ArithIntOCamlCode = ArithIntAst
    syn Const =
    | CModi {}
    | CDivi {}
    | CNegi {}

    sem ocamlconstgen (state : CodegenState) =
    | CAddi _ -> {cgr_new with code = "( + )"}
    | CSubi _ -> {cgr_new with code = "( - )"}
    | CMuli _ -> {cgr_new with code = "( * )"}
    | CDivi _ -> {cgr_new with code = "( / )"}
    | CModi _ -> {cgr_new with code = "( mod )"}
    | CNegi _ -> {cgr_new with code = "( ~- )"}

    sem cudacodegen (state : CodegenState) =
    | CAddi _ -> {{cgr_new with code = "gpu_addi"}
                           with devicefuncs = strset_add "__device__ int gpu_addi(int x, int y) {return x + y;}" strset_new}
    | CSubi _ -> {{cgr_new with code = "gpu_subi"}
                           with devicefuncs = strset_add "__device__ int gpu_subi(int x, int y) {return x - y;}" strset_new}
    | CMuli _ -> {{cgr_new with code = "gpu_muli"}
                           with devicefuncs = strset_add "__device__ int gpu_muli(int x, int y) {return x * y;}" strset_new}

    sem getConstStringCode (indent : Int) =
    | CModi _ -> "modi"
    | CDivi _ -> "divi"
    | CNegi _ -> "negi"
end

lang BoolOCamlCode = BoolAst
    sem ocamlconstgen (state : CodegenState) =
    | CBool b -> {cgr_new with code = if b.val then "true" else "false"}
    | CNot _ -> {cgr_new with code = "not"}
    | CAnd _ -> {cgr_new with code = "( && )"}
    | COr _ -> {cgr_new with code = "( || )"}

    sem ocamlcodegen (state : CodegenState) =
    | TmIf t ->
      let cond = ocamlcodegen state t.cond in
      let thn = ocamlcodegen (cgsincr state) t.thn in
      let els = ocamlcodegen (cgsincr state) t.els in
      let newcode = strJoin "" ["if ", cond.code, " then",
        cgsnewline (cgsincr state), thn.code, cgsnewline state,
        "else", cgsnewline (cgsincr state), els.code] in
      cgr_merge newcode [cond, thn, els]
end

lang CmpOCamlCode = CmpAst
    sem ocamlconstgen (state : CodegenState) =
    | CEqi _ -> {cgr_new with code = "( = )"}
    | CLti _ -> {cgr_new with code = "( < )"}
end

lang CharOCamlCode = CharAst
    syn Const =
    | CChar2int {}
    | CInt2char {}

    sem ocamlconstgen (state : CodegenState) =
    | CChar c ->
      let cstr = if eqchar c.val (head "\n") then
                   [head "'", head "\\", 'n', head "'"]
                 else
                   [head "'", c.val, head "'"]
      in
      {cgr_new with code = cstr}
    | CChar2int _ -> {cgr_new with code = "int_of_char"}
    | CInt2char _ -> {cgr_new with code = "char_of_int"}

    sem getConstStringCode (indent : Int) =
    | CChar2int _ -> "char2int"
    | CInt2char _ -> "int2char"
end

lang SeqOCamlCode = SeqAst
    syn Const =
    | CLength {}
    | CCons {}
    | CConcat {}
    | CSlice {}
    | CReverse {}
    | CMakeseq {}

    sem ocamlconstgen (state : CodegenState) =
    | CNth _ -> {{cgr_new with code = "Array.get"}
                          with opens = strset_add "Array" strset_new}
    | CLength _ -> {{cgr_new with code = "Array.length"}
                             with opens = strset_add "Array" strset_new}
    | CCons _ -> {{cgr_new with code = "(fun x xs -> Array.append [|x|] xs)"}
                           with opens = strset_add "Array" strset_new}
    | CConcat _ -> {{cgr_new with code = "Array.append"}
                             with opens = strset_add "Array" strset_new}
    | CSlice _ -> {{cgr_new with code = "(fun xs start len -> Array.sub xs (min ((Array.length xs) - 1) start) (min ((Array.length xs) - start) len))"}
                            with opens = strset_add "Array" strset_new}
    | CReverse _ -> {{cgr_new with code = "(fun xs -> Array.of_list (List.rev (Array.to_list xs)))"}
                              with opens = strset_add "Array" (strset_add "List" strset_new)}
    | CMakeseq _ -> {{cgr_new with code = "Array.make"}
                              with opens = strset_add "Array" strset_new}
    | CSeq t ->
      let cgrs = map (ocamlcodegen state) t.tms in
      let newcode = strJoin "" ["[|", strJoin "; " (map (lam cgr. cgr.code) cgrs), "|]"] in
      cgr_merge newcode cgrs

    sem ocamlcodegen (state : CodegenState) =
    | TmSeq t -> 
      let cgrs = map (ocamlcodegen state) t.tms in
      let newcode = strJoin "" ["[|", strJoin "; " (map (lam cgr. cgr.code) cgrs), "|]"] in
      cgr_merge newcode cgrs

    sem getConstStringCode (indent : Int) =
    | CLength _ -> "length"
    | CCons _ -> "cons"
    | CConcat _ -> "concat"
    | CSlice _ -> "slice"
    | CReverse _ -> "reverse"
    | CMakeseq _ -> "makeseq"
end

lang TupleOCamlCode = TupleAst
    sem ocamlcodegen (state : CodegenState) =
    | TmTuple t ->
      let cgrs = map (ocamlcodegen state) t.tms in
      let newcode = strJoin ", " (map (lam cgr. cgr.code) cgrs) in
      cgr_merge newcode cgrs
    | TmProj t ->
      error "TmProj is not yet implemented. Cannot implement this as a native OCaml product type"
end

-- Syntax fragments
lang CUDAOptOCamlCode = VarAst + AppAst + ConstAst + IntAst
    syn Expr =
    -- A map of a function that only takes integer arguments and returns an integer argument
    | TmCUDAMapIntArray {elemPerThread : Int,
                         func : Expr,
                         array : Expr}

    sem ocamlcodegen (state : CodegenState) =
    | TmCUDAMapIntArray t ->
      -- acc: List of applied arguments
      -- e: Expression to extract
      recursive let extract_args: [Expr] -> Expr -> ([Expr], String) = lam acc. lam e.
          match e with TmApp t1 then
            extract_args (cons t1.rhs acc) t1.lhs
          else match e with TmVar t1 then
            (acc, t1.ident)
          else
            error "TmCUDAMapIntArray: Mapped function is not lifted! (Expected TmVar)"
      in
      let arg2string = lam arg.
        let perror = lam _.
          let _ = dprint arg in
          let _ = print "\n" in
          error "TmCUDAMapIntArray: Above argument is invalid."
        in
        match arg with TmConst t1 then
          match t1.val with CInt i then
            int2string i
          else
            perror ()
        else match arg with TmVar t1 then
          t1.ident
        else
          perror ()
      in
      let res = extract_args [] t.func in
      let args = res.0 in
      let hostfuncname = concat "gpuhost_" res.1 in
      let externdef =
        strJoin "" ["external ", hostfuncname, ": ",
                    strJoin " -> " (concat (map (lam _. "int") args) ["int array", "int array"]),
                    " = \"", hostfuncname, "\""]
      in
      let cudaret = cudacodegen state (TmCUDAMapIntArray t) in
      {{cudaret with code = strJoin " " [hostfuncname, strJoin " " (map arg2string args), arg2string t.array]}
                with externs = strset_add externdef cudaret.externs}

    -- Generate C++ host functions that interfaces with the OCaml types
    sem cudacodegen (state : CodegenState) =
    | TmCUDAMapIntArray t ->
      -- acc: List of applied arguments
      -- e: Expression to extract
      recursive let extract_args: [Expr] -> Expr -> ([Expr], String) = lam acc. lam e.
          match e with TmApp t1 then
            extract_args (cons t1.rhs acc) t1.lhs
          else match e with TmVar t1 then
            (acc, t1.ident)
          else
            error "TmCUDAMapIntArray: Mapped function is not lifted! (Expected TmVar)"
      in
      let res = extract_args [] t.func in
      let args = res.0 in
      let mappedfuncname = res.1 in
      let hostfuncname = concat "gpuhost_" mappedfuncname in
      let globalfuncname = concat "gpuglobal_" mappedfuncname in
      let devicefuncname = concat "gpudevice_" mappedfuncname in
      let intargs = mapi (lam i. lam _. concat "arg" (int2string i)) args in
      let intargsconved = map (lam s. strJoin "" ["Int_val(", s, ")"]) intargs in
      let inarr = "inarr" in
      let outarr = "outarr" in
      let cuda_inarr = concat "cuda_" inarr in
      let cuda_outarr = concat "cuda_" outarr in
      let numargs = addi (length intargs) 1 in
      if gti numargs 5 then
        let _ = print (strJoin "" ["Number of arguments to ", hostfuncname, ": ", int2string numargs]) in
        error "Number of arguments cannot exceed 5."
      else -- carry on


      -- Generate code for the mapped function
      let cudaret = cudacodegen {state with cudagentype = "int"} (TmVar {ident = mappedfuncname}) in

      -- Generate the global device body
      let globalbody = strJoin "" [
        "__global__ void ", globalfuncname, "(", strJoin ", " (concat (map (concat "int ") intargs)
                                                                      (map (concat "value *") [inarr, outarr])),
                                                 ", int n)\n",
        "{\n",
        "\tint i;\n",
        "\tint start = threadIdx.x;\n",
        "\tint end = start + ", int2string t.elemPerThread, ";\n",
        "\tif (end > n)\n",
        "\t\tend = n;\n\n",
        "\tfor (i = start; i < end; ++i) {\n",
        "\t\tint v = Int_val(", inarr, "[i]);\n",
        "\t\t", outarr, "[i] = Val_int(", devicefuncname, "(", strJoin ", " (concat intargs ["v"]), "));\n",
        "\t}\n",
        "}"
      ] in

      -- Generate the prototype
      let prototype = strJoin "" [
        "value ", hostfuncname, "(",
        strJoin ", " (map (concat "value ") (concat intargs [inarr])),
        ")"
      ] in
      -- Generate the host body
      let hostbody = strJoin "" [
        prototype,
        "\n{\n",
        -- The CAML interface for the parameters
        "\tCAMLparam", int2string numargs, "(",
           strJoin ", " (concat intargs [inarr]), ");\n",
        "\tCAMLlocal1(", outarr, ");\n",
        "\tint n = Wosize_val(", inarr, ");\n\n",
        "\tvalue *cuda_", inarr, ";\n",
        "\tvalue *cuda_", outarr, ";\n",
        "\tcudaMalloc(&cuda_", inarr, ", n * sizeof(value));\n",
        "\tcudaMalloc(&cuda_", outarr, ", n * sizeof(value));\n",
        "\tcudaMemcpy(cuda_", inarr, ", Op_val(", inarr, "), n * sizeof(value), cudaMemcpyHostToDevice);\n\n",
        "\t", globalfuncname, "<<<1,(n + ", int2string (subi t.elemPerThread 1),
                              ") / ", int2string t.elemPerThread, ">>>(",
                              strJoin ", " (concat intargsconved [cuda_inarr, cuda_outarr, "n"]), ");\n",
        "\t", outarr, " = caml_alloc(n, Tag_val(", inarr, "));\n",
        "\tcudaDeviceSynchronize();\n\n",
        "\t// The ", outarr, " must reside on the minor heap for the following cudaMemcpy to succeed.\n",
        "\t// (Possible garbage collector issues otherwise...)\n",
        "\t// (Though should never be any issues since neither the previous or new value are blocks...)\n",
        "\tCAMLassert(Is_young((value) ", outarr, "));\n",
        "\tcudaMemcpy(Op_val(", outarr, "), cuda_", outarr, ", n * sizeof(value), cudaMemcpyDeviceToHost);\n\n",
        "\tcudaFree(cuda_", inarr, ");\n",
        "\tcudaFree(cuda_", outarr, ");\n\n",
        "\tCAMLreturn(", outarr, ");\n",
        "}"
      ] in
      {{{cudaret with globalfuncs = strset_add globalbody strset_new}
                 with hostprototypes = strset_add prototype strset_new}
                 with hostfuncs = strset_add hostbody strset_new}

      sem pprintCode (indent : Int) =
      | TmCUDAMapIntArray t ->
        strJoin "" [
          "cudaMapIntArray ", int2string t.elemPerThread,
          " (", pprintCode indent t.func, ")",
          " (", pprintCode indent t.array, ")"
        ]
end

lang MExprOCamlCode = VarOCamlCode + AppOCamlCode + FunOCamlCode + LetOCamlCode +
                      RecLetsOCamlCode + ConstOCamlCode + UnitOCamlCode + IntOCamlCode +
                      ArithIntOCamlCode + BoolOCamlCode + CmpOCamlCode + CharOCamlCode +
                      SeqOCamlCode + TupleOCamlCode + CUDAOptOCamlCode + MExprPrettyPrint
    syn Expr =
    | TmMain {body : Expr}

    syn Const =
    | CPrint {}

    sem ocamlcodegen (state : CodegenState) =
    | TmMain t ->
      let mainret = ocamlcodegen (cgsincr state) t.body in
      let newcode = strJoin "" ["let main =", cgsnewline (cgsincr state), mainret.code] in
      cgr_merge newcode [mainret]

    sem ocamlconstgen (state : CodegenState) =
    | CPrint _ -> {{cgr_new with code = "(fun s -> printf \"%s\" (String.of_seq (Array.to_seq s)))"}
                            with opens = strset_add "Printf" strset_new}

    sem getConstStringCode (indent : Int) =
    | CPrint _ -> "print"
end

let codegen =
    use MExprOCamlCode in
    lam ast.
    let res = ocamlcodegen cgs_new (TmMain {body = ast}) in
    -- OCaml Code
    let opens = strJoin "\n" (map (concat "open ") res.opens) in
    let externs = strJoin "\n" res.externs in
    let ocaml = strJoin "\n\n" [opens, externs, res.code] in
    -- CUDA Code
    let includeheaders = ["caml/alloc.h", "caml/memory.h",
                          "caml/mlvalues.h", "stdio.h", "stdlib.h"] in
    let includes = strJoin "\n" (map (lam s. strJoin "" ["#include <", s, ">"]) includeheaders) in
    let hostprototypes = strJoin "\n" ["extern \"C\" {", strJoin "\n" (map (lam s. strJoin "" ["\t", s, ";"]) res.hostprototypes), "}"] in
    let devicecode = strJoin "\n\n" res.devicefuncs in
    let globalcode = strJoin "\n\n" res.globalfuncs in
    let hostcode = strJoin "\n\n" res.hostfuncs in
    let cuda = strJoin "\n\n" [includes, hostprototypes, devicecode, globalcode, hostcode] in
    -- Return code
    (ocaml, cuda)
