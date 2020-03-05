-- Type finder, get the type of any particular expression

include "codegen-common.mc"

lang VarCGType = MExprCGExt
    sem codegenGetExprType (state : CodegenState) =
    | TmVar x ->
      let perror = lam _.
        error (strJoin "" ["codegenGetExprType: identifier \"", x.ident, "\" could not be associated with a type."])
      in
      let cgs_envLookup = lam key. lam state.
        match find (lam e. eqstr key e.key) state.env with Some t then
          t.value
        else
          error (strJoin "" ["Could not find a binding for \"", key, "\""])
      in
      let getsome = lam opt. match opt with Some t then t else perror () in
      let v = cgs_envLookup x.ident state in
      match v with TmLet t then
        getsome t.tpe
      else match v with TmLam t then
        getsome t.tpe
      else perror ()
end

lang LetCGType = MExprCGExt
    sem codegenGetExprType (state : CodegenState) =
    | TmLet t ->
      let getsome = lam opt. match opt with Some t then t else let _ = dprint (TmLet t) in error "No tpe" in
      getsome t.tpe
end

lang FunCGType = MExprCGExt
    sem codegenGetExprType (state : CodegenState) =
    | TmLam t ->
      let getsome = lam opt. match opt with Some t then t else let _ = dprint (TmLam t) in error "No tpe" in
      getsome t.tpe
end

lang ConstCGType = MExprCGExt
    sem codegenGetConstType (state : CodegenState) =
    -- intentionally left blank

    sem codegenGetExprType (state : CodegenState) =
    | TmConst c -> codegenGetConstType state c.val
end

lang IntCGType = MExprCGExt
    sem codegenGetConstType (state : CodegenState) =
    | CInt _ -> TyInt ()
end

lang CharCGType = MExprCGExt
    sem codegenGetConstType (state : CodegenState) =
    | CChar _ -> TyChar ()
end

lang SeqCGType = MExprCGExt
    sem codegenGetExprType (state : CodegenState) =
    -- intentionally left blank

    sem codegenGetConstType (state : CodegenState) =
    | CSeq t -> if gti (length t.tms) 0 then
                  TySeq {tpe = codegenGetExprType state (head t.tms)}
                else
                  TySeq {tpe = TyDyn ()}
end

lang MExprCGType = VarCGType + LetCGType + FunCGType + ConstCGType +
                   IntCGType + CharCGType + SeqCGType

-- Helper functions
let type2cudastr = use MExprCGType in
    lam tpe.
    let perror = lam _.
      let _ = dprint tpe in
      let _ = print "\n" in
      error "type2cudastr: Above type is invalid."
    in
    match tpe with TyInt () then
      "int "
    else match tpe with TySeq t1 then
      match t1.tpe with TyInt () then
        "value *"
      else perror ()
    else perror ()

recursive let type2ocamlstring = use MExprCGType in
    lam tpe.
    let perror = lam _.
      let _ = dprint tpe in
      let _ = print "\n" in
      error "type2ocamlstring: Above type is invalid."
    in
    match tpe with TyInt () then
      "int"
    else match tpe with TySeq t1 then
      strJoin " " [type2ocamlstring t1.tpe, "array"]
    else
      perror ()
end

recursive let getRetType = use MExprCGType in
    lam tpe.
    match tpe with TyArrow t then
      getRetType t.to
    else
      tpe
end

let getSeqType = use MExprCGType in
    lam tpe.
    let perror = lam _.
      let _ = dprint tpe in
      let _ = print "\n" in
      error "getSeqType: Above type is not a seq."
    in
    match tpe with TySeq t then
      t.tpe
    else perror ()
