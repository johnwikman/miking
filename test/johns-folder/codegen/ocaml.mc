-- OCaml code generator
-- This assumes that Lambda lifting has been performed and all identifiers are globally unique...

--
-- What needs to happen:
--  The lambdas need to appear on the left side of the equals sign

include "mexpr/pprint.mc"

include "common.mc"
include "cuda.mc"
include "types.mc"

lang VarCGOCaml = MExprCGExt
    sem codegenOCaml (state : CodegenState) =
    | TmVar x -> {cgr_new with code = x.ident}
end

lang AppCGOCaml = MExprCGExt
    sem codegenOCaml (state : CodegenState) =
    | TmApp t ->
      let lhs = codegenOCaml state t.lhs in
      let rhs = codegenOCaml state t.rhs in
      let newcode = strJoin "" [lhs.code, " (", rhs.code, ")"] in
      cgr_merge newcode [lhs, rhs]
end

lang FunCGOCaml = MExprCGExt
    sem codegenOCaml (state : CodegenState) =
    | TmLam t ->
      let _ = dprint t in
      let _ = print "\n" in
      error "Encountered an isolated lambda in codegenOCaml"
end

lang LetCGOCaml = MExprCGExt
    sem codegenOCaml (state : CodegenState) =
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
      let internalret = codegenOCaml (cgsincr internalstate) letexpr in
      let inexprret = codegenOCaml instate t.inexpr in
      let newcode = strJoin "" ["let ", t.ident, " ", strJoin " " args, " =", cgsnewline (cgsincr state),
                                internalret.code, cgsnewline state,
                                "in", cgsnewline state, inexprret.code] in
      cgr_merge newcode [internalret, inexprret]
end

lang RecLetsCGOCaml = MExprCGExt
    sem codegenOCaml (state : CodegenState) =
    | TmRecLets t ->
      -- Create the instate which have access to all the bound expressions
      -- in the recursive scope
      let instate = foldl (lam st. lam b. cgs_envAdd b.ident (TmRecLetsRef b) st) state t.bindings in
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
          let letret = codegenOCaml (cgsincri 4 (cgsincr instate)) letexpr in
          let updatedcode = strJoin "" [
            genacc.code, prefix, name, " ", strJoin " " args, " =",
            cgsnewline (cgsincri 4 (cgsincr instate)), letret.code, cgsnewline instate
          ]
          in
          generatelets (cgr_merge updatedcode [genacc, letret]) (tail l)
      in
      let genret = generatelets cgr_new t.bindings in
      let inret = codegenOCaml instate t.inexpr in
      let newcode = strJoin "" [genret.code, "in", cgsnewline instate, inret.code] in
      cgr_merge newcode [genret, inret]
end

lang ConstCGOCaml = MExprCGExt
    sem ocamlconstgen (state : CodegenState) =
    -- intentionally left blank

    sem codegenOCaml (state : CodegenState) =
    | TmConst c -> ocamlconstgen state c.val
end

lang UnitCGOCaml = MExprCGExt
    sem ocamlconstgen (state : CodegenState) =
    | CUnit _ -> {cgr_new with code = "()"}
end

lang IntCGOCaml = MExprCGExt
    sem ocamlconstgen (state : CodegenState) =
    | CInt i -> {cgr_new with code = int2string i.val}
end

lang FloatCGOCaml = MExprCGExt
    sem ocamlconstgen (state : CodegenState) =
    | CFloat f -> {cgr_new with code = float2string f.val}
end

lang ArithIntCGOCaml = MExprCGExt
    sem ocamlconstgen (state : CodegenState) =
    | CAddi _ -> {cgr_new with code = "( + )"}
    | CSubi _ -> {cgr_new with code = "( - )"}
    | CMuli _ -> {cgr_new with code = "( * )"}
    | CDivi _ -> {cgr_new with code = "( / )"}
    | CModi _ -> {cgr_new with code = "( mod )"}
    | CNegi _ -> {cgr_new with code = "( ~- )"}
end

lang ArithFloatCGOCaml = MExprCGExt
    sem ocamlconstgen (state : CodegenState) =
    | CAddf _ -> {cgr_new with code = "( +. )"}
    | CSubf _ -> {cgr_new with code = "( -. )"}
    | CMulf _ -> {cgr_new with code = "( *. )"}
    | CDivf _ -> {cgr_new with code = "( /. )"}
    | CFloorfi _ -> {{cgr_new with code = "(fun x -> int_of_float (Float.floor x))"}
                              with opens = strset_add "Float" strset_new}
    | CCeilfi _ -> {{cgr_new with code = "(fun x -> int_of_float (Float.ceil x))"}
                             with opens = strset_add "Float" strset_new}
    | CRoundfi _ -> {{cgr_new with code = "(fun x -> int_of_float (Float.round x))"}
                              with opens = strset_add "Float" strset_new}
    | CInt2float _ -> {cgr_new with code = "float_of_int"}
    | CString2float _ -> {{cgr_new with code = "(fun s -> float_of_string (String.of_seq (Array.to_seq s)))"}
                                   with opens = strset_add "String" (strset_add "Array" strset_new)}
end

lang BoolCGOCaml = MExprCGExt
    sem ocamlconstgen (state : CodegenState) =
    | CBool b -> {cgr_new with code = if b.val then "true" else "false"}
    | CNot _ -> {cgr_new with code = "not"}
    | CAnd _ -> {cgr_new with code = "( && )"}
    | COr _ -> {cgr_new with code = "( || )"}

    sem codegenOCaml (state : CodegenState) =
    | TmIf t ->
      let cond = codegenOCaml state t.cond in
      let thn = codegenOCaml (cgsincr state) t.thn in
      let els = codegenOCaml (cgsincr state) t.els in
      let newcode = strJoin "" ["if ", cond.code, " then",
        cgsnewline (cgsincr state), thn.code, cgsnewline state,
        "else", cgsnewline (cgsincr state), els.code] in
      cgr_merge newcode [cond, thn, els]
end

lang CmpCGOCaml = MExprCGExt
    sem ocamlconstgen (state : CodegenState) =
    | CEqi _ -> {cgr_new with code = "( = )"}
    | CLti _ -> {cgr_new with code = "( < )"}
    | CNeqi _ -> {cgr_new with code = "( <> )"}
    | CGti _ -> {cgr_new with code = "( > )"}
    | CGeqi _ -> {cgr_new with code = "( >= )"}
    | CLeqi _ -> {cgr_new with code = "( <= )"}
end

lang CharCGOCaml = MExprCGExt
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
end

lang SeqCGOCaml = MExprCGExt
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
      let cgrs = map (codegenOCaml state) t.tms in
      let newcode = strJoin "" ["[|", strJoin "; " (map (lam cgr. cgr.code) cgrs), "|]"] in
      cgr_merge newcode cgrs

    sem codegenOCaml (state : CodegenState) =
    | TmSeq t -> 
      let cgrs = map (codegenOCaml state) t.tms in
      let newcode = strJoin "" ["[|", strJoin "; " (map (lam cgr. cgr.code) cgrs), "|]"] in
      cgr_merge newcode cgrs
end

lang TupleCGOCaml = MExprCGExt
    sem codegenOCaml (state : CodegenState) =
    | TmTuple t ->
      let cgrs = map (codegenOCaml state) t.tms in
      let newcode = strJoin ", " (map (lam cgr. cgr.code) cgrs) in
      cgr_merge newcode cgrs
    | TmProj t ->
      error "TmProj is not yet implemented. Cannot implement this as a native OCaml product type"
end

-- Syntax fragments
lang CUDACGOCaml = MExprCGExt
    sem codegenGetExprType (state : CodegenState) =
    -- Intentionally left blank

    sem codegenCUDA (state : CodegenState) =
    -- Intentionally left blank

    sem ocamlconstgen (state : CodegenState) =
    -- Intentionally left blank

    sem codegenOCaml (state : CodegenState) =
    | TmCUDAMap t ->
      -- acc: List of applied arguments
      -- e: Expression to extract
      recursive let extract_args: [Expr] -> Expr -> ([Expr], String) = lam acc. lam e.
          match e with TmApp t1 then
            extract_args (cons t1.rhs acc) t1.lhs
          else match e with TmVar t1 then
            (acc, t1.ident)
          else
            error "TmCUDAMap: Mapped function is not lifted! (Expected TmVar)"
      in
      recursive let findarrow_endtpe = lam tpe.
        match tpe with TyArrow t1 then
          findarrow_endtpe t1.to
        else
          tpe
      in
      let res = extract_args [] t.func in
      let args = concat res.0 (if t.onlyIndexArg then [t.onlyIndexArgSize] else [t.array]) in
      let hostfuncname = concat "gpuhost_" res.1 in
      let argtypes = map (codegenGetExprType state) args in
      let funrettype = findarrow_endtpe (codegenGetExprType state (TmVar {ident = res.1})) in
      let rettype = TySeq {tpe = funrettype} in

      -- Pack integer and floating point arguments together. (Temporarily commented out) --
      let packedArgs = foldl (lam acc. lam e.
        let pos = acc.0 in
        let intacc = acc.1 in
        let floatacc = acc.2 in
        let miscacc = acc.3 in
        let etype = codegenGetExprType state e in
        match etype with TyInt () then
          (addi pos 1, cons {pos = pos, val = e} intacc, floatacc, miscacc)
        else match etype with TyFloat () then
          (addi pos 1, intacc, cons {pos = pos, val = e} floatacc, miscacc)
        else
          (addi pos 1, intacc, floatacc, cons e miscacc)
      ) (0, [],[],[]) args in

      let packedInts = reverse packedArgs.1 in
      let packedFloats = reverse packedArgs.2 in
      let nonPackedArgs = reverse packedArgs.3 in
      let nonPackedArgTypes = map (codegenGetExprType state) nonPackedArgs in

      let externdef =
        strJoin "" ["external ", hostfuncname, ": ",
                    "int array -> float array -> ",
                    strJoin " -> " (map type2ocamlstring (concat nonPackedArgTypes [rettype])),
                    " = \"", hostfuncname, "\""]
      in

      let packedintcgr = codegenOCaml state (TmSeq {tms = map (lam r. r.val) packedInts}) in
      let packedfloatcgr = codegenOCaml state (TmSeq {tms = map (lam r. r.val) packedFloats}) in
      let nonpackedcgr = map (codegenOCaml state) nonPackedArgs in
      let nonpackedcode = strJoin " " (map (lam r. strJoin "" ["(", r.code, ")"]) nonpackedcgr) in

      let cudacgr = codegenCUDA state (TmCUDAMap {{{t with packedInts = packedInts}
                                                      with packedFloats = packedFloats}
                                                      with nonPackedArgs = nonPackedArgs}) in
      let retcgr = cgr_merge "" (concat [cudacgr, packedintcgr, packedfloatcgr] nonpackedcgr) in
      {{retcgr with code = strJoin " " [hostfuncname, packedintcgr.code, packedfloatcgr.code, nonpackedcode]}
               with externs = strset_add externdef retcgr.externs}
      ---------------------------------------------------------

----      let externdef =
----        strJoin "" ["external ", hostfuncname, ": ",
----                    strJoin " -> " (map type2ocamlstring (concat argtypes [rettype])),
----                    " = \"", hostfuncname, "\""]
----      in
----
----      let argcgr = map (codegenOCaml state) args in
----      let argcode = strJoin " " (map (lam r. strJoin "" ["(", r.code, ")"]) argcgr) in
----
----      let cudacgr = codegenCUDA state (TmCUDAMap t) in
----      let retcgr = cgr_merge "" (cons cudacgr argcgr) in
----      {{retcgr with code = strJoin " " [hostfuncname, argcode]}
----               with externs = strset_add externdef retcgr.externs}
end

lang MainGCOCaml = MExprCGExt
    sem codegenOCaml (state : CodegenState) =
    | TmMain t ->
      let mainret = codegenOCaml (cgsincr state) t.body in
      let newcode = strJoin "" ["let main =", cgsnewline (cgsincr state), mainret.code] in
      cgr_merge newcode [mainret]

    sem ocamlconstgen (state : CodegenState) =
    | CPrint _ -> {{cgr_new with code = "(fun s -> printf \"%s\" (String.of_seq (Array.to_seq s)))"}
                            with opens = strset_add "Printf" strset_new}
    | CError _ -> {cgr_new with code = "(fun s -> printf \"ERROR: %s\n\" (String.of_seq (Array.to_seq s)); exit 1)"}
end

lang MExprCGOCaml = VarCGOCaml + AppCGOCaml + FunCGOCaml + LetCGOCaml +
                    RecLetsCGOCaml + ConstCGOCaml + UnitCGOCaml + IntCGOCaml +
                    ArithIntCGOCaml + FloatCGOCaml + ArithFloatCGOCaml +
                    BoolCGOCaml + CmpCGOCaml + CharCGOCaml + SeqCGOCaml +
                    TupleCGOCaml + CUDACGOCaml + MainGCOCaml + MExprCGType +
                    MExprCGCUDA + MExprPrettyPrint

let codegen =
    use MExprCGOCaml in
    lam ast.
    let res = codegenOCaml cgs_new (TmMain {body = ast}) in
    -- OCaml Code
    let opens = strJoin "\n" (map (concat "open ") res.opens) in
    let externs = strJoin "\n" res.externs in
    let ocaml = strJoin "\n\n" [opens, externs, res.code] in
    -- CUDA Code
    let includeheaders = ["caml/alloc.h", "caml/memory.h",
                          "caml/mlvalues.h", "stdio.h", "stdlib.h"] in
    let preprocessorchecks = [
      "#ifndef FLAT_FLOAT_ARRAY",
      "#error OCaml floats are not stored in flat array, cannot GPU optimize them.",
      "#endif",
      "#ifdef ARCH_ALIGN_DOUBLE",
      "#error Doubles are not same size as OCaml values, cannot GPU optimize them.",
      "#endif"
    ] in
    let includes = strJoin "\n" (map (lam s. strJoin "" ["#include <", s, ">"]) includeheaders) in
    let preprocessor = strJoin "\n" preprocessorchecks in
    let hostprototypes = strJoin "\n" ["extern \"C\" {", strJoin "\n" (map (lam s. strJoin "" ["\t", s, ";"]) res.hostprototypes), "}"] in
    let deviceprototypes = strJoin "\n" (map (lam s. concat s ";") res.deviceprototypes) in
    let devicecode = strJoin "\n\n" res.devicefuncs in
    let globalcode = strJoin "\n\n" res.globalfuncs in
    let hostcode = strJoin "\n\n" res.hostfuncs in
    let cuda = strJoin "\n\n" [includes, preprocessor, hostprototypes, deviceprototypes, devicecode, globalcode, hostcode] in
    -- Return code
    (ocaml, cuda)
