-- OCaml code generator
-- This assumes that Lambda lifting has been performed and all identifiers are globally unique...

--
-- What needs to happen:
--  The lambdas need to appear on the left side of the equals sign

include "codegen-common.mc"

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

lang ConstCGOCaml = ConstAst
    sem ocamlconstgen (state : CodegenState) =
    -- intentionally left blank

    sem codegenOCaml (state : CodegenState) =
    | TmConst c -> ocamlconstgen state c.val
end

lang UnitCGOCaml = UnitAst
    sem ocamlconstgen (state : CodegenState) =
    | CUnit _ -> {cgr_new with code = "()"}
end

lang IntCGOCaml = IntAst
    sem ocamlconstgen (state : CodegenState) =
    | CInt i -> {cgr_new with code = int2string i.val}
end

lang ArithIntOCamlCode = ArithIntCGExt
    sem ocamlconstgen (state : CodegenState) =
    | CAddi _ -> {cgr_new with code = "( + )"}
    | CSubi _ -> {cgr_new with code = "( - )"}
    | CMuli _ -> {cgr_new with code = "( * )"}
    | CDivi _ -> {cgr_new with code = "( / )"}
    | CModi _ -> {cgr_new with code = "( mod )"}
    | CNegi _ -> {cgr_new with code = "( ~- )"}
end

lang BoolOCamlCode = BoolAst
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

lang CmpOCamlCode = CmpAst
    sem ocamlconstgen (state : CodegenState) =
    | CEqi _ -> {cgr_new with code = "( = )"}
    | CLti _ -> {cgr_new with code = "( < )"}
end

lang CharOCamlCode = CharCGExt
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

lang SeqOCamlCode = SeqCGExt
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

lang TupleOCamlCode = TupleAst
    sem codegenOCaml (state : CodegenState) =
    | TmTuple t ->
      let cgrs = map (codegenOCaml state) t.tms in
      let newcode = strJoin ", " (map (lam cgr. cgr.code) cgrs) in
      cgr_merge newcode cgrs
    | TmProj t ->
      error "TmProj is not yet implemented. Cannot implement this as a native OCaml product type"
end

-- Syntax fragments
lang CUDAOCamlCode = VarAst + AppAst + ConstAst + IntAst + FunAst +
                     SeqTypeAst + ArithTypeAst + CUDACGExt
    sem codegenFindExprType (state : CodegenState) =
    -- Intentionally left blank

    sem codegenCUDA (state : CodegenState) =
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
      let arg2string = lam arg.
        let perror = lam _.
          let _ = dprint arg in
          let _ = print "\n" in
          error "TmCUDAMap: Above argument is invalid."
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
      recursive let type2string = lam tpe.
        let perror = lam _.
          let _ = dprint tpe in
          let _ = print "\n" in
          error "TmCUDAMap: Above type is invalid."
        in
        match tpe with TyInt () then
          "int"
        else match tpe with TySeq t1 then
          strJoin " " [type2string t1.tpe, "array"]
        else
          perror ()
      in
      recursive let findarrow_endtpe = lam tpe.
        match tpe with TyArrow t1 then
          findarrow_endtpe t1.to
        else
          tpe
      in
      let res = extract_args [] t.func in
      let args = res.0 in
      let hostfuncname = concat "gpuhost_" res.1 in
      let argtypes = map (codegenFindExprType state) args in
      let arrtype = codegenFindExprType state t.array in
      let funrettype = findarrow_endtpe (codegenFindExprType state (TmVar {ident = res.1})) in

      -- Must map an a sequence of something
      let arrtype = match arrtype with TySeq t1 then arrtype else error "TmCUDAMap: must map a sequence" in

      let rettype = TySeq {tpe = funrettype} in

      let externdef =
        strJoin "" ["external ", hostfuncname, ": ",
                    strJoin " -> " (map type2string (concat argtypes [arrtype, rettype])),
                    " = \"", hostfuncname, "\""]
      in
      let cudaret = codegenCUDA state (TmCUDAMap t) in
      {{cudaret with code = strJoin " " [hostfuncname, strJoin " " (map arg2string args), arg2string t.array]}
                with externs = strset_add externdef cudaret.externs}
end

lang MExprOCamlCode = VarOCamlCode + AppOCamlCode + FunOCamlCode + LetOCamlCode +
                      RecLetsOCamlCode + ConstOCamlCode + UnitOCamlCode + IntOCamlCode +
                      ArithIntOCamlCode + BoolOCamlCode + CmpOCamlCode + CharOCamlCode +
                      SeqOCamlCode + TupleOCamlCode + CUDAOptOCamlCode + MExprPrettyPrint + MainCGExt

    sem codegenOCaml (state : CodegenState) =
    | TmMain t ->
      let mainret = codegenOCaml (cgsincr state) t.body in
      let newcode = strJoin "" ["let main =", cgsnewline (cgsincr state), mainret.code] in
      cgr_merge newcode [mainret]

    sem ocamlconstgen (state : CodegenState) =
    | CPrint _ -> {{cgr_new with code = "(fun s -> printf \"%s\" (String.of_seq (Array.to_seq s)))"}
                            with opens = strset_add "Printf" strset_new}
end

let codegen =
    use MExprOCamlCode in
    lam ast.
    let res = codegenOCaml cgs_new (TmMain {body = ast}) in
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
