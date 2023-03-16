-- MLang AST symbolization.

include "mexpr/symbolize.mc"
include "mlang/ast.mc"


lang UseSym = Sym + UseAst
  sem symbolizeExpr (env : SymEnv) =
  | TmUse t ->
    match env with {langEnv = langEnv} in
    if nameHasSym t.ident then
      TmUse {t with inexpr = symbolizeExpr env t.inexpr}
    else
      let str = nameGetStr t.ident in
      let ident =
        match mapLookup str langEnv with Some ident then ident
        else if env.allowFree then t.ident
        else errorSingle [t.info] (concat "Unknown language in symbolizeExpr: " str)
      in
      TmUse {t with ident = ident}

  sem addTopNames (env : SymEnv) =
  | TmUse t ->
    -- we are not introducing any definitions here yet, so do nothing
    addTopNames env t.inexpr
end


type SymDeclEnv
type SymDeclEnvDict = {
    mexprEnv: SymEnv,
    synEnv: Map String [Name],
    semEnv: Map String [Name],
    fileIncludeEnvs: Map String SymDeclEnv,
    langIncludeEnvs: Map Name SymDeclEnv
}
con SymDeclEnvDef : SymDeclEnvDict -> SymDeclEnv


-- Symbolization for declarations
lang SymDecl = Sym + DeclAst
  sem symbolizeDecl: SymDeclEnv -> Decl -> (SymDeclEnv, Decl)
end

lang LangDeclSym = SymDecl + LangDeclAst
  sem symbolizeDecl (env : SymDeclEnv) =
  | DeclLang t ->
    match mapAccumL (lam env: SymDeclEnv. lam inclIdent.
        match env with SymDeclEnvDef (sde & {mexprEnv = mexprEnv}) in
        let ident =
          if nameHasSym inclIdent then ident
          else
            let str = nameGetStr inclIdent in
            match mapLookup str mexprEnv.langEnv with Some ident then ident
            else if mexprEnv.allowFree then inclIdent
            else errorSingle [t.info] (concat "Unknown language include in symbolizeDecl: " str)
        in
        if nameHasSym ident then
          match mapLookup ident sde.langIncludeEnvs with Some incEnvdef then
            match incEnvdef with SymDeclEnvDef {mexprEnv = incMexprEnv} in
            (SymDeclEnvDef {sde with mexprEnv = symEnvOverride mexprEnv incMexprEnv}, ident)
          else
            let str = nameGetStr ident in
            errorSingle [t.info] (concat "No environment for symbolized language include in symbolizeDecl: " str)
        else
          (env, ident)
      ) env t.includes
    with (env, includes) in
    match mapAccumL symbolizeDecl env t.decls with (env, decls) in
    match
      if nameHasSym t.ident then (env, t.ident)
      else
        match env with SymDeclEnvDef (sde & {mexprEnv = mexprEnv}) in
        let ident = nameSetNewSym t.ident in
        let str = nameGetStr ident in
        let langIncludeEnvs = mapInsert ident env sde.langIncludeEnvs in
        let langEnv = mapInsert str ident mexprEnv.langEnv in
        let mexprEnv = {mexprEnv with langEnv = langEnv} in
        (SymDeclEnvDef {sde with mexprEnv = mexprEnv,
                                 langIncludeEnvs = langIncludeEnvs}, ident)
    with (env, langIdent) in
    (env, DeclLang {t with ident = langIdent,
                           includes = includes,
                           decls = decls})
end

lang SynDeclSym = SymDecl + SynDeclAst
  sem symbolizeDecl (env : SymDeclEnv) =
  -- {ident : Name,
  --  extends : [Name],
  --  defs : [{ident : Name, tyIdent : Type}],
  --  info : Info}
  | DeclSyn t ->
    match
      if nameHasSym t.ident then (env, t)
      else
        match env with SymDeclEnvDef (sde & {mexprEnv = mexprEnv, synEnv = synEnv}) in
        let ident = nameSetNewSym t.ident in
        let str = nameGetStr t.ident in
        let oldExtends = mapLookupOr [] str synEnv in
        let newExtends = cons ident oldExtends in
        let synEnv = mapInsert str newExtends synEnv in
        let t = {t with ident = ident, extends = oldExtends} in
        let tyConEnv = mapInsert str ident mexprEnv.tyConEnv in
        let mexprEnv = {mexprEnv with tyConEnv = tyConEnv} in
        (SymDeclEnvDef {sde with mexprEnv = mexprEnv, synEnv = synEnv}, t)
    with (env, t) in

    match
      mapAccumL (lam env: SymDeclEnv. lam condef.
        match env with SymDeclEnvDef (sde & {mexprEnv = mexprEnv}) in
        let tyIdent = symbolizeType mexprEnv condef.tyIdent in
        if nameHasSym condef.ident then
          (env, {condef with tyIdent = tyIdent})
        else
          let str = nameGetStr condef.ident in
          let ident = nameSetNewSym condef.ident in
          let conEnv = mapInsert str ident mexprEnv.conEnv in
          let mexprEnv = {mexprEnv with conEnv = conEnv} in
          let env = SymDeclEnvDef {sde with mexprEnv = mexprEnv} in
          (env, {condef with ident = ident, tyIdent = tyIdent})
      ) env t.defs
    with (env, defs) in
    (env, {t with defs = defs})
end

lang SemDeclSym = SymDecl + SemDeclAst
  sem symbolizeDecl (env : SymDeclEnv) =
  -- {ident : Name,
  --  extends : [Name],
  --  tyAnnot : Type,
  --  tyBody : Type,
  --  args : [{ident : Name, tyAnnot : Type}],
  --  cases : [{pat : Pat, thn : Expr}],
  --  info : Info}
  | DeclSem t ->
    match
      if nameHasSym t.ident then (env, t)
      else
        match env with SymDeclEnvDef (sde & {mexprEnv = mexprEnv, semEnv = semEnv}) in
        let ident = nameSetNewSym t.ident in
        let str = nameGetStr t.ident in
        let oldExtends = mapLookupOr [] str semEnv in
        let newExtends = cons ident oldExtends in
        let semEnv = mapInsert str newExtends semEnv in
        let t = {t with ident = ident, extends = oldExtends} in
        let varEnv = mapInsert str ident mexprEnv.varEnv in
        let mexprEnv = {mexprEnv with varEnv = varEnv} in
        (SymDeclEnvDef {sde with mexprEnv = mexprEnv, semEnv = semEnv}, t)
    with (env, t) in

    match env with SymDeclEnvDef {mexprEnv = mexprEnv} in
    let tyAnnot = symbolizeType mexprEnv t.tyAnnot in
    let tyBody = symbolizeType mexprEnv t.tyBody in
    let args = map (lam arg: {ident: Name, tyAnnot: Type}.
        let tyAnnot = symbolizeType mexprEnv arg.tyAnnot in
        if nameHasSym arg.ident then
            {ident = arg.ident, tyAnnot = tyAnnot}
        else
            {ident = nameSetNewSym arg.ident, tyAnnot = tyAnnot}
    ) t.args in
    let t = {t with tyAnnot = tyAnnot, tyBody = tyBody, args = args} in

    let casesEnv = foldl (lam casesEnv. lam arg.
      let str = nameGetStr arg.ident in
      let varEnv = mapInsert str arg.ident casesEnv.varEnv in
      {casesEnv with varEnv = varEnv}
    ) mexprEnv t.args in
    let cases = map (lam c: {pat: Pat, thn: Expr}.
      let pat = symbolizePat casesEnv (mapEmpty cmpString) c.pat in
      let thn = symbolizeExpr casesEnv thn in
      {pat = pat, thn = thn}
    ) t.cases in
    (env, DeclSem {t with cases = cases})
end
