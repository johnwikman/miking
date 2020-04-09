-- CUDA Function Runtime/Complexity Estimate (Heuristic)

-- For each function application: f x1 x2 ... xn, find the definition of the
-- function: f a1 a2 ... an, and apply the name replacement [ai -> xi]. For
-- subsequent calls, mark the function f as scanned. If a function is scanned
-- recursively, then we mark it as having constant cost (heustistic
-- assumption). This is the case even if the name replacement for the function
-- call would be different (another heuristic assumption). If a recursive call
-- has been encountered, then we mark that function as being recursive.

-- For each scanned function, we collect all variables & constants (c1, ... cn)
-- that are part of if-conditions in the function body. If that function is
-- recursive, then we claim that the function will be recursively called the
-- number of times corresponding to the largest variable part of any of the
-- if-conditions (another heuristic assumption). If len([c1,...,cn]) > 0, then
-- the total cost of the function is max(c1,...,cn) * (internal cost). If
-- len([c1,...,cn]) = 0, then we say that the total cost is equal to the
-- internal cost.

-- NOTE: The only valid entrypoint is a TmApp expression.

include "common.mc"

-- Cost profile for performing a specific operation
type CostProfile = {c_addi : Int,
                    c_muli : Int,
                    c_subi : Int,
                    c_divi : Int,
                    c_modi : Int,
                    c_addf : Int,
                    c_mulf : Int,
                    c_subf : Int,
                    c_divf : Int,
                    c_negf : Int,
                    c_expf : Int,
                    c_logf : Int,
                    c_eqf : Int,
                    c_ltf : Int,
                    c_gtf : Int,
                    c_randUniformf : Int,
                    c_randNormalf : Int,
                    c_logpdfNormalf : Int,
                    c_floorfi : Int,
                    c_ceilfi : Int,
                    c_or : Int,
                    c_and : Int,
                    c_eqi : Int,
                    c_lti : Int,
                    c_gti : Int,
                    c_nth : Int,
                    c_int2float : Int,
                    c_if : Int,
                    c_seqoverhead : Int,
                    c_argapply : Int,
                    c_reccall : Int,
                    c_varaccess : Int,
                    c_intaccess : Int,
                    c_floataccess : Int,
                    c_unitaccess : Int,
                    c_boolaccess : Int}

let costprof_vanilla = {c_addi = 1, c_muli = 1, c_subi = 1, c_divi = 1, c_modi = 1,
                        c_addf = 1, c_mulf = 1, c_subf = 1, c_divf = 1, c_negf = 1,
                        c_expf = 1, c_logf = 1, c_eqf = 1, c_ltf = 1, c_gtf = 1,
                        c_randUniformf = 1, c_randNormalf = 1, c_logpdfNormalf = 1,
                        c_floorfi = 1, c_ceilfi = 1, c_or = 1, c_and = 1,
                        c_eqi = 1, c_lti = 1, c_gti = 1, c_nth = 1, c_int2float = 1, c_if = 1,
                        c_argapply = 1, c_reccall = 1, c_varaccess = 1,
                        c_intaccess = 1, c_floataccess = 1, c_unitaccess = 0,
                        c_boolaccess = 1}

type CostAnalysisState = {scannedFunctions   : StringSet,
                          argNameReplacement : [(String, Expr)],
                          recFuncsConds      : [(String, [Expr])],
                          inCondBranch       : Bool,
                          profile            : CostProfile}

let costas_new = {scannedFunctions = strset_new,
                  argNameReplacement = [],
                  recFuncsConds = [],
                  inCondBranch = false,
                  profile = costprof_vanilla}

-- recursiveFunctions: List of all recursive function.
-- recFuncsConds: The expressions in which functions are recursive to.
type CostAnalysisRecRet = {recursiveFunctions : StringSet,
                           recFuncsConds : [(String, [Expr])],
                           scopeCondExprs : [Expr]}

let costarr_new = {recursiveFunctions = strset_new,
                   recFuncsConds = [],
                   scopeCondExprs = []}

let costarr_merge = lam rets.
    foldl (lam acc. lam e.
      let newrfc =
        foldl (lam rfcacc. lam rfc.
          if any (lam x. eqstr rfc.0 x.0) rfcacc then
            rfcacc
          else
            cons rfc rfcacc
        ) acc.recFuncsConds e.recFuncsConds
      in
      {{{acc with recursiveFunctions = strset_union acc.recursiveFunctions e.recursiveFunctions}
             with recFuncsConds = newrfc}
             with scopeCondExprs = concat acc.scopeCondExprs e.scopeCondExprs}
    ) costarr_new rets

let is_tmconst_cint = use MExprCGExt in
    lam e.
    match e with TmConst c then
      match c.val with CInt _ then
        true
      else false
    else false

let get_tmconst_cint = use MExprCGExt in
    lam e.
    match e with TmConst c then
      match c.val with CInt i then
        i.val
      else error "Not TmConst CInt"
    else error "Not TmConst CInt"

lang WrapperCGCostEstimate = MExprCGExt
    syn Expr =
    | TmMaxCost {exprs : [Expr]}
    | TmSumCost {exprs : [Expr]}
    | TmProdCost {exprs : [Expr]}

    sem codegenCostEstimate_COSTSCAN (state : CodegenState) (cas : CostAnalysisState) =
    | TmMaxCost t ->
      let partint = partition is_tmconst_cint t.exprs in
      let precalmax = foldl (lam acc. lam e. let v = (get_tmconst_cint e) in if lti acc v then v else acc) 1 partint.0 in
      foldl (lam acc. lam e.
        TmIf {cond = TmApp {lhs = TmApp {lhs = TmConst {val = CLti ()},
                                         rhs = acc},
                            rhs = e},
              thn = e,
              els = acc}
      ) (TmConst {val = CInt {val = precalmax}}) partint.1

    | TmSumCost t ->
      let partint = partition is_tmconst_cint t.exprs in
      let precalsum = foldl (lam acc. lam e. addi acc (get_tmconst_cint e)) 0 partint.0 in
      foldl (lam acc. lam e.
        TmApp {lhs = TmApp {lhs = TmConst {val = CAddi ()},
                            rhs = acc},
               rhs = e}
      ) (TmConst {val = CInt {val = precalsum}}) partint.1

    | TmProdCost t ->
      let partint = partition is_tmconst_cint t.exprs in
      let precalprod = foldl (lam acc. lam e. muli acc (get_tmconst_cint e)) 1 partint.0 in
      foldl (lam acc. lam e.
        TmApp {lhs = TmApp {lhs = TmConst {val = CMuli ()},
                            rhs = acc},
               rhs = e}
      ) (TmConst {val = CInt {val = precalprod}}) partint.1
end

lang VarCGCostEstimate = MExprCGExt
    sem codegenCostEstimate_RECSCAN (state : CodegenState) (cas : CostAnalysisState) =
    | TmVar t ->
      let resolvedexpr =
        match findAssoc (eqstr t.ident) cas.argNameReplacement with Some t1 then
          t1
        else
          TmVar t
      in
      if cas.inCondBranch then
        (resolvedexpr, {costarr_new with scopeCondExprs = [resolvedexpr]})
      else
        (resolvedexpr, costarr_new)

    sem codegenCostEstimate_COSTSCAN (state : CodegenState) (cas : CostAnalysisState) =
    | TmVar t -> TmConst {val = CInt {val = cas.profile.c_varaccess}}
end

-- Shared helper functions for the AppCGCostEstimate fragment
recursive let unwrap_app = use MExprCGExt in
  lam argacc. lam e.
  match e with TmApp t1 then
    unwrap_app (cons t1.rhs argacc) t1.lhs
  else match e with TmVar t1 then 
    (e, t1.ident, argacc)
  else match e with TmConst t1 then
    (e, "", argacc)
  else
    let _ = dprint e in
    let _ = print "\n" in
    error "unwrap_app: Applied function is not lifted."
end

recursive let find_func = use MExprCGExt in
  lam state. lam prev. lam name.
  if any (eqstr name) prev then
    error "Infinite identifier loop."
  else
    let f = cgs_envLookup name state in
    match f with TmVar t1 then
        find_func (cons name prev) t1.ident
    else
        f
end

recursive let extract_lambody = use MExprCGExt in
  lam func. lam acc. lam e.
  match e with TmLam t1 then
    extract_lambody func (concat acc [t1.ident]) t1.body
  else
    match func with TmLet t1 then
      (TmLet {t1 with body = e}, acc)
    else match func with TmRecLetsRef t1 then
      (TmRecLetsRef {t1 with body = e}, acc)
    else error "extract_lambody: expression is not TmLet or TmRecLetsRef"
end

lang AppCGCostEstimate = MExprCGExt + WrapperCGCostEstimate
    -- ENTRYPOINT
    sem codegenCostEstimate (state : CodegenState) (profile : CostProfile) =
    | TmApp t ->
      -- Scan for which functions are recursive
      let recret = codegenCostEstimate_RECSCAN state costas_new (TmApp t) in

      let cas_cost = {{costas_new with recFuncsConds = (recret.1).recFuncsConds}
                                  with profile = profile} in

      codegenCostEstimate_COSTSCAN state cas_cost (TmApp t)

    sem codegenCostEstimate_NEEDSFLOAT2INTCONV =
    -- intentionally left blank

    -- Scan the recursive nature of functions
    sem codegenCostEstimate_RECSCAN (state : CodegenState) (cas : CostAnalysisState) =
    | TmApp t ->
      -- Extract arguments here
      let res = unwrap_app [] (TmApp t) in
      let funcexpr = res.0 in
      let funcname = res.1 in
      let scannedArgs = map (codegenCostEstimate_RECSCAN state cas) res.2 in
      let args = map (lam t. t.0) scannedArgs in
      let argret = costarr_merge (map (lam t. t.1) scannedArgs) in
      let retexpr = foldl (lam acc. lam e. TmApp {lhs = acc, rhs = e}) funcexpr args in
      match funcexpr with TmConst t1 then
        -- If this TmApp is on a constant function, then we only care about
        -- what is contained inside the arguments.
        match t1.val with CNth _ then
          -- if we are performing an array access, then we most certainly do
          -- not want the sequence to be listed as an integer.
          let nthcond =
            if gti (length argret.scopeCondExprs) 0 then
              [TmConst {val = CInt {val = 1}}]
            else
              []
          in
          (TmConst {val = CInt {val = 1}}, {argret with scopeCondExprs = nthcond})
        else if codegenCostEstimate_NEEDSFLOAT2INTCONV t1.val then
          -- Returns a float, convert it to an integer
          (TmApp {lhs = TmConst {val = CFloorfi ()}, rhs = retexpr}, argret)
        else
          (retexpr, argret)
      else -- continue

      -- EXPECTED: funcname now refers to a `let` or `recursive let`.
      let func = find_func state [] funcname in
      let extracted =
        match func with TmLet t1 then
          extract_lambody func [] t1.body
        else match func with TmRecLetsRef t1 then
          extract_lambody func [] t1.body
        else
          let _ = dprint func in
          let _ = print "\n" in
          error "codegenCostEstimate_RECSCAN (TmApp): extracted: function is not TmLet or TmRecLetsRef."
      in
      let extexpr = extracted.0 in
      let argnames = extracted.1 in
      let replacements = zipWith (lam a. lam b. (a,b)) argnames args in
      let newcas = {{cas with argNameReplacement = replacements} with inCondBranch = false} in
      let ret = codegenCostEstimate_RECSCAN state newcas extexpr in
      (retexpr, costarr_merge [argret, ret.1])

    sem codegenCostEstimate_COSTSCAN (state : CodegenState) (cas : CostAnalysisState) =
    | TmApp t ->
      let lookup_replace = lam e.
        match e with TmVar x then
          match findAssoc (eqstr x.ident) cas.argNameReplacement with Some t1 then
            t1
          else e
        else e
      in
      -- Extract arguments here
      let res = unwrap_app [] (TmApp t) in
      let funcexpr = res.0 in
      let funcname = res.1 in
      let args = res.2 in
      let argsindividualcost = map (codegenCostEstimate_COSTSCAN state cas) args in
      let argscost = codegenCostEstimate_COSTSCAN state cas (TmSumCost {exprs = argsindividualcost}) in
      match funcexpr with TmConst _ then
        -- If this TmApp is on a constant function, then the cost of applying
        -- the arguments are ignored.
        let constcost = codegenCostEstimate_COSTSCAN state cas funcexpr in
        codegenCostEstimate_COSTSCAN state cas (TmSumCost {exprs = [constcost, argscost]})
      else -- continue

      -- EXPECTED: funcname now refers to a `let` or `recursive let`.

      let func = find_func state [] funcname in
      let extracted =
        match func with TmLet t1 then
          extract_lambody func [] t1.body
        else match func with TmRecLetsRef t1 then
          extract_lambody func [] t1.body
        else
          let _ = dprint func in
          let _ = print "\n" in
          error "codegenCostEstimate_COSTSCAN (TmApp): extracted: function is not TmLet or TmRecLetsRef."
      in
      let extexpr = extracted.0 in
      let argnames = extracted.1 in
      let replacements = zipWith (lam a. lam b. (a,b)) argnames args in
      let newcas = {cas with argNameReplacement = replacements} in
      let retcost = codegenCostEstimate_COSTSCAN state newcas extexpr in
      codegenCostEstimate_COSTSCAN state cas (TmSumCost {exprs = [TmConst {val = CInt {val = length args}}, retcost, argscost]})
end

lang LetCGCostEstimate = MExprCGExt + WrapperCGCostEstimate
    sem codegenCostEstimate_RECSCAN (state : CodegenState) (cas : CostAnalysisState) =
    | TmLet t ->
      if strset_in t.ident cas.scannedFunctions then
        (TmLet t, {costarr_new with recursiveFunctions = strset_add t.ident strset_new})
      else -- continue
      let newcas = {cas with scannedFunctions = strset_add t.ident cas.scannedFunctions} in
      let bodyret = codegenCostEstimate_RECSCAN state newcas t.body in
      -- if this is a variable, make sure that we bind the variable name in the in-expression
      let incas = {newcas with argNameReplacement = cons (t.ident, bodyret.0) newcas.argNameReplacement} in
      let inret = codegenCostEstimate_RECSCAN state incas t.inexpr in

      let ret = costarr_merge [bodyret.1, inret.1] in
      let newlet = TmLet {{t with body = bodyret.0} with inexpr = inret.0} in
      if strset_in t.ident ret.recursiveFunctions then
        (newlet, {ret with recFuncsConds = cons (t.ident, ret.scopeCondExprs) ret.recFuncsConds})
      else
        (newlet, ret)

    sem codegenCostEstimate_COSTSCAN (state : CodegenState) (cas : CostAnalysisState) =
    | TmLet t ->
      if strset_in t.ident cas.scannedFunctions then
        -- this is a recursive call, cost should be multiplicatively handled at
        -- a higher level
        TmConst {val = CInt {val = cas.profile.c_reccall}}
      else
      let newcas = {cas with scannedFunctions = strset_add t.ident cas.scannedFunctions} in
      let internalcost = codegenCostEstimate_COSTSCAN state newcas t.body in
      let incost = codegenCostEstimate_COSTSCAN state newcas t.inexpr in
      match findAssoc (eqstr t.ident) cas.recFuncsConds with Some t1 then
        -- This function is recursively called with a set of conditionals (?)
        let ccexpr = TmMaxCost {exprs = t1} in
        let cccost = codegenCostEstimate_COSTSCAN state newcas ccexpr in
        let prodexpr = TmProdCost {exprs = [cccost, internalcost]} in
        let prodcost = codegenCostEstimate_COSTSCAN state newcas prodexpr in
        let sumexpr = TmSumCost {exprs = [prodcost, incost]} in
        let sumcost = codegenCostEstimate_COSTSCAN state newcas sumexpr in
        sumcost
      else
        -- not recursively called
        let sumexpr = TmSumCost {exprs = [internalcost, incost]} in
        codegenCostEstimate_COSTSCAN state newcas sumexpr
end

lang RecLetsCGCostEstimate = MExprCGExt
    sem codegenCostEstimate_RECSCAN (state : CodegenState) (cas : CostAnalysisState) =
    | TmRecLetsRef t ->
      let letexpr = TmLet {ident = t.ident,
                           tpe = t.tpe,
                           body = t.body,
                           inexpr = TmConst {val = CUnit ()}}
      in
      codegenCostEstimate_RECSCAN state cas letexpr

    sem codegenCostEstimate_COSTSCAN (state : CodegenState) (cas : CostAnalysisState) =
    | TmRecLetsRef t ->
      let letexpr = TmLet {ident = t.ident,
                           tpe = t.tpe,
                           body = t.body,
                           inexpr = TmConst {val = CUnit ()}}
      in
      codegenCostEstimate_COSTSCAN state cas letexpr
end

lang ConstCGCostEstimate = MExprCGExt
    sem codegenCostEstimate_RECSCAN (state : CodegenState) (cas : CostAnalysisState) =
    | TmConst c ->
      if cas.inCondBranch then
        (TmConst c, {costarr_new with scopeCondExprs = [TmConst c]})
      else
        (TmConst c, costarr_new)

    sem codegenCostEstimate_NEEDSFLOAT2INTCONV =
    | CFloat _ -> true
    | CNegf _ -> true
    | CAddf _ -> true
    | CSubf _ -> true
    | CMulf _ -> true
    | CDivf _ -> true
    | CEqf _ -> true
    | CLtf _ -> true
    | CGtf _ -> true
    | CRandUniformf _ -> true
    | CRandNormalf _ -> true
    | CExpf _ -> true
    | CLogf _ -> true
    | CLogpdfNormalf _ -> true
    | _ -> false

    sem codegenCostEstimate_COSTSCAN (state : CodegenState) (cas : CostAnalysisState) =
    | TmConst c -> codegenCostEstimate_CONSTCOSTSCAN cas c.val

    sem codegenCostEstimate_CONSTCOSTSCAN (cas : CostAnalysisState) =
    | CInt _ -> TmConst {val = CInt {val = cas.profile.c_intaccess}}
    | CFloat _ -> TmConst {val = CInt {val = cas.profile.c_floataccess}}
    | CBool _ -> TmConst {val = CInt {val = cas.profile.c_boolaccess}}
    | CUnit _ -> TmConst {val = CInt {val = cas.profile.c_unitaccess}}
    | COr _ -> TmConst {val = CInt {val = cas.profile.c_or}}
    | CAnd _ -> TmConst {val = CInt {val = cas.profile.c_and}}
    | CAddi _ -> TmConst {val = CInt {val = cas.profile.c_addi}}
    | CSubi _ -> TmConst {val = CInt {val = cas.profile.c_subi}}
    | CMuli _ -> TmConst {val = CInt {val = cas.profile.c_muli}}
    | CDivi _ -> TmConst {val = CInt {val = cas.profile.c_divi}}
    | CModi _ -> TmConst {val = CInt {val = cas.profile.c_modi}}
    | CEqi _ -> TmConst {val = CInt {val = cas.profile.c_eqi}}
    | CLti _ -> TmConst {val = CInt {val = cas.profile.c_lti}}
    | CGti _ -> TmConst {val = CInt {val = cas.profile.c_gti}}
    | CNth _ -> TmConst {val = CInt {val = cas.profile.c_nth}}
    | CInt2float _ -> TmConst {val = CInt {val = cas.profile.c_int2float}}
    | CNegf _ -> TmConst {val = CInt {val = cas.profile.c_negf}}
    | CAddf _ -> TmConst {val = CInt {val = cas.profile.c_addf}}
    | CSubf _ -> TmConst {val = CInt {val = cas.profile.c_subf}}
    | CMulf _ -> TmConst {val = CInt {val = cas.profile.c_mulf}}
    | CDivf _ -> TmConst {val = CInt {val = cas.profile.c_divf}}
    | CEqf _ -> TmConst {val = CInt {val = cas.profile.c_eqf}}
    | CLtf _ -> TmConst {val = CInt {val = cas.profile.c_ltf}}
    | CGtf _ -> TmConst {val = CInt {val = cas.profile.c_gtf}}
    | CRandUniformf _ -> TmConst {val = CInt {val = cas.profile.c_randUniformf}}
    | CRandNormalf _ -> TmConst {val = CInt {val = cas.profile.c_randNormalf}}
    | CFloorfi _ -> TmConst {val = CInt {val = cas.profile.c_floorfi}}
    | CCeilfi _ -> TmConst {val = CInt {val = cas.profile.c_ceilfi}}
    | CExpf _ -> TmConst {val = CInt {val = cas.profile.c_expf}}
    | CLogf _ -> TmConst {val = CInt {val = cas.profile.c_logf}}
    | CLogpdfNormalf _ -> TmConst {val = CInt {val = cas.profile.c_logpdfNormalf}}
end

lang BoolCGCostEstimate = MExprCGExt + WrapperCGCostEstimate
    sem codegenCostEstimate_RECSCAN (state : CodegenState) (cas : CostAnalysisState) =
    | TmIf t ->
      let condret = codegenCostEstimate_RECSCAN state {cas with inCondBranch = true} t.cond in
      let thnret = codegenCostEstimate_RECSCAN state cas t.thn in
      let elsret = codegenCostEstimate_RECSCAN state cas t.els in
      (TmIf {{{t with cond = condret.0}
                 with thn = thnret.0}
                 with els = elsret.0},
       costarr_merge [condret.1, thnret.1, elsret.1])

    sem codegenCostEstimate_COSTSCAN (state : CodegenState) (cas : CostAnalysisState) =
    | TmIf t ->
      let condret = codegenCostEstimate_COSTSCAN state cas t.cond in
      let thnret = codegenCostEstimate_COSTSCAN state cas t.thn in
      let elsret = codegenCostEstimate_COSTSCAN state cas t.els in
      let branchcost = codegenCostEstimate_COSTSCAN state cas (TmMaxCost {exprs = [thnret, elsret]}) in
      codegenCostEstimate_COSTSCAN state cas (TmSumCost {exprs = [TmConst {val = CInt {val = cas.profile.c_if}},
                                                                  condret,
                                                                  branchcost]})
end

lang SeqCGCostEstimate = MExprCGExt + WrapperCGCostEstimate
    sem codegenCostEstimate_RECSCAN (state : CodegenState) (cas : CostAnalysisState) =
    | TmSeq t ->
      let rets = map (codegenCostEstimate_RECSCAN state cas) t.tms in
      (TmSeq {t with tms = map (lam t. t.0) rets}, costarr_merge (map (lam t. t.1) rets))

    sem codegenCostEstimate_COSTSCAN (state : CodegenState) (cas : CostAnalysisState) =
    | TmSeq t ->
      let retcosts = map (codegenCostEstimate_COSTSCAN state cas) t.tms in
      codegenCostEstimate_COSTSCAN state cas (TmSumCost {exprs = retcosts})
end

lang MExprCGCostEstimate = WrapperCGCostEstimate + VarCGCostEstimate + AppCGCostEstimate +
                           LetCGCostEstimate + RecLetsCGCostEstimate + ConstCGCostEstimate +
                           BoolCGCostEstimate + SeqCGCostEstimate
