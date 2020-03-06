-- CUDA Code generation

include "codegen-common.mc"
include "codegen-types.mc"

let mapi = lam f. lam seq.
  recursive let work = lam i. lam f. lam seq.
      if null seq then []
      else cons (f i (head seq)) (work (addi i 1) f (tail seq))
  in
  work 0 f seq

let hostname = lam s. concat "gpuhost_" s
let gblname = lam s. concat "gpuglobal_" s
let devname = lam s. concat "gpudevice_" s

lang VarCGCUDA = MExprCGExt
    sem codegenCUDA (state : CodegenState) =
    | TmVar x ->
      let v = cgs_envLookup x.ident state in
      match v with TmLet t then
        codegenCUDA state v
      else match v with TmRecLetsRef t then
        codegenCUDA state v
      else match v with TmLam t then
        {cgr_new with code = t.ident}
      else match v with TmVar t then
        {cgr_new with code = t.ident}
      else
        let _ = dprint v in
        error (strJoin "" ["\nVariable \"", x.ident, "\" is not bound to a TmLet or TmVar"])
end

lang AppCGCUDA = MExprCGExt
    sem codegenCUDA (state : CodegenState) =
    | TmApp t ->
      let nthCheck =
        let perror = lam _.
          let _ = dprint (TmApp t) in
          let _ = print "\n" in
          error "codegenCUDA: TmApp: nthCheck: Invalid array access."
        in
        match t.lhs with TmApp t1 then
          match t1.lhs with TmConst c then
            match c.val with CNth _ then
              -- We are performing an array access!
              match t1.rhs with TmVar t2 then
                let idxcgr = codegenCUDA state t.rhs in
                Some ({idxcgr with code = strJoin "" [t2.ident, "[", idxcgr.code, "]"]})
              else perror ()
            else None ()
          else None ()
        else None ()
      in
      recursive let unwrap_app = lam cgr. lam argacc. lam e.
        match e with TmApp t1 then
          let rhscgr = codegenCUDA state t1.rhs in
          unwrap_app (cgr_merge "" [cgr, rhscgr]) (cons rhscgr.code argacc) t1.lhs
        else
          let lhscgr = codegenCUDA state e in
          (cgr_merge "" [cgr, lhscgr], cons lhscgr.code argacc)
      in
      -- First check if we are performing an array access
      match nthCheck with Some tcgr then
        tcgr
      else -- continue
      let ret = unwrap_app cgr_new [] (TmApp t) in
      let cgr = ret.0 in
      let func = head ret.1 in
      let args = tail ret.1 in
      let funcall = strJoin "" [func, "(", strJoin ", " args, ")"] in
      {cgr with code = funcall}
end

lang FunCGCUDA = MExprCGExt
    sem codegenCUDA (state : CodegenState) =
    | TmLam t ->
      let _ = dprint (TmLam t) in
      let _ = print "\n" in
      error "Encountered an isolated lambda in codegenCUDA"
end

lang LetCGCUDA = MExprCGExt
    sem codegenGetExprType (state : CodegenState) =
    -- Intentionally left blank

    sem codegenCUDA (state : CodegenState) =
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
      recursive let arrowtype_unfold = lam tpe. lam acc.
        match tpe with TyArrow t1 then
          arrowtype_unfold t1.to (cons t1.from acc)
        else
          cons tpe acc
      in

      let argres = chainlambdas ([], state) t.body in
      let args = argres.0 in
      let letexpr = argres.1 in
      let internalstate = argres.2 in

      let types = arrowtype_unfold (codegenGetExprType state (TmLet t)) [] in
      let rettype = head types in
      let rettypestr = type2cudastr rettype in
  
      let argtypes = reverse (tail types) in
      let argdecls = zipWith concat (map type2cudastr argtypes) args in

      let cudaret = codegenCUDA internalstate letexpr in

      let prototype = strJoin "" [
        "__device__ ", rettypestr, devname t.ident, "(", strJoin ", " argdecls, ")"
      ] in

      let devicebody = strJoin "" [
        prototype, "\n",
        "{\n",
        "\treturn ", cudaret.code, ";\n",
        "}"
      ] in
      {{{cudaret with code = devname t.ident}
                 with deviceprototypes = strset_add prototype cudaret.deviceprototypes}
                 with devicefuncs = strset_add devicebody cudaret.devicefuncs}
end

lang RecLetsCGCUDA = MExprCGExt
    sem codegenCUDA (state : CodegenState) =
    | TmRecLetsRef t ->
      -- Add ourselves as a variable to make sure that we do not get stuck
      -- in an infinite loop.
      let instate = cgs_envAdd t.ident (TmVar {ident = devname t.ident}) state in
      codegenCUDA instate (TmLet {ident = t.ident, tpe = t.tpe, body = t.body, inexpr = TmConst {val = CUnit ()}})
end

lang ConstCGCUDA = MExprCGExt
    sem codegenConstCUDA (state : CodegenState) =
    -- intentionally left blank

    sem codegenCUDA (state : CodegenState) =
    | TmConst c -> codegenConstCUDA state c.val
end

-- Generate a constant function
let genconstfun = lam typeprefix. lam name. lam argdecls. lam body.
    let prototype = strJoin "" [typeprefix, " ", name, argdecls] in
    let fullfunc = strJoin " " [prototype, body] in
    {{{cgr_new with code = name}
               with deviceprototypes = strset_add prototype strset_new}
               with devicefuncs = strset_add fullfunc strset_new}

lang IntCGCUDA = MExprCGExt
    sem codegenConstCUDA (state : CodegenState) =
    | CInt i -> {cgr_new with code = int2string i.val}
end

lang ArithIntCGCUDA = MExprCGExt
    sem codegenConstCUDA (state : CodegenState) =
    | CAddi _ -> genconstfun "__device__ int" "gpu_addi" "(int x, int y)" "{return x + y;}"
    | CSubi _ -> genconstfun "__device__ int" "gpu_subi" "(int x, int y)" "{return x - y;}"
    | CMuli _ -> genconstfun "__device__ int" "gpu_muli" "(int x, int y)" "{return x * y;}"
end

lang ArithFloatCGCUDA = MExprCGExt
    sem codegenConstCUDA (state : CodegenState) =
    | CAddf _ -> genconstfun "__device__ double" "gpu_addf" "(double x, double y)" "{return x + y;}"
    | CSubf _ -> genconstfun "__device__ double" "gpu_subf" "(double x, double y)" "{return x - y;}"
    | CMulf _ -> genconstfun "__device__ double" "gpu_mulf" "(double x, double y)" "{return x * y;}"
    | CDivf _ -> genconstfun "__device__ double" "gpu_divf" "(double x, double y)" "{return x / y;}"
end

lang BoolCGCUDA = MExprCGExt
    sem codegenConstCUDA (state : CodegenState) =
    | CBool b -> {cgr_new with code = if b.val then "true" else "false"}
    | CNot _ -> genconstfun "__device__ bool" "gpu_not" "(bool a)" "{return !a;}"
    | CAnd _ -> genconstfun "__device__ bool" "gpu_and" "(bool a, bool b)" "{return a && b;}"
    | COr _ -> genconstfun "__device__ bool" "gpu_or" "(bool a, bool b)" "{return a || b;}"

    sem codegenCUDA (state : CodegenState) =
    | TmIf t ->
      let condcgr = codegenCUDA state t.cond in
      let thncgr = codegenCUDA state t.thn in
      let elscgr = codegenCUDA state t.els in
      let code = strJoin "" ["(", condcgr.code, ") ? (", thncgr.code, ") : (", elscgr.code, ")"] in
      cgr_merge code [condcgr, thncgr, elscgr]
end

lang CmpCGCUDA = MExprCGExt
    sem codegenConstCUDA (state : CodegenState) =
    | CEqi _ -> genconstfun "__device__ bool" "gpu_eqi" "(int x, int y)" "{return x == y;}"
    | CLti _ -> genconstfun "__device__ bool" "gpu_lti" "(int x, int y)" "{return x < y;}"
end

-- acc: List of applied arguments
-- e: Expression to extract
recursive let extract_args: [Expr] -> Expr -> ([Expr], String) = use MExprCGExt in
  lam acc. lam e.
  let perror = lam _.
    let _ = dprint e in
    let _ = print "\n" in
    error "extract_args: Mapped function is not lifted! (Expected TmVar)"
  in
    match e with TmApp t1 then
      extract_args (cons t1.rhs acc) t1.lhs
    else match e with TmVar t1 then
      (acc, t1.ident)
    else
      perror ()
end

let typeOCaml2CUDA = use MExprCGExt in
  lam name. lam tpe.
  let perror = lam _.
    let _ = dprint tpe in
    let _ = print "\n" in
    error "typeOCaml2CUDA: Above type is invalid."
  in
  match tpe with TyInt () then
    strJoin "" ["Int_val(", name, ")"]
  else match tpe with TyFloat () then
    strJoin "" ["*((double *) ", name, ")"]
  else match tpe with TySeq t1 then
    name
  else perror ()

let typeCUDA2OCaml = use MExprCGExt in
  lam name. lam tpe.
  let perror = lam _.
    let _ = dprint tpe in
    let _ = print "\n" in
    error "typeCUDA2OCaml: Above type is invalid."
  in
  match tpe with TyInt () then
    strJoin "" ["Val_int(", name, ")"]
  else match tpe with TySeq t1 then
    name
  else perror ()

let assignIdxCUDA2OCaml = use MExprCGExt in
  lam lhsname. lam lhsidx. lam rhs. lam tpe.
  let perror = lam _.
    let _ = dprint tpe in
    let _ = print "\n" in
    error "assignIdxCUDA2OCaml: Above type is invalid."
  in
  match tpe with TyInt () then
    strJoin "" [lhsname, "[", lhsidx, "] = Val_int(", rhs, ")"]
  else match tpe with TyFloat () then
    strJoin "" ["((double *) ", lhsname, ")[", lhsidx, "] = ", rhs]
  else perror ()

let extractIdxCUDA2OCaml = use MExprCGExt in
  lam lhs. lam rhsname. lam rhsidx. lam tpe.
  let perror = lam _.
    let _ = dprint tpe in
    let _ = print "\n" in
    error "extractIdxCUDA2OCaml: Above type is invalid."
  in
  match tpe with TyInt () then
    strJoin "" [lhs, " = Int_val(", rhsname, "[", rhsidx, "])"]
  else match tpe with TyFloat () then
    strJoin "" [lhs, " = ((double *) ", rhsname, ")[", rhsidx, "]"]
  else perror ()

lang CUDACGCUDA = MExprCGExt
    sem codegenGetExprType (state : CodegenState) =
    -- Intentionally left blank

    -- Generate C++ host functions that interfaces with the OCaml types
    sem codegenCUDA (state : CodegenState) =
    | TmCUDAMap t ->
      let res = extract_args [] t.func in
      let args = res.0 in
      let argtypes = map (codegenGetExprType state) args in
      let argnames = mapi (lam i. lam _. concat "arg" (int2string i)) args in
      let argsconved = zipWith typeOCaml2CUDA argnames argtypes in
      let argstyped = zipWith concat (map type2cudastr argtypes) argnames in

      let arrtype = codegenGetExprType state t.array in
      let mappedfuncname = res.1 in
      let mappedfunctype = codegenGetExprType state (cgs_envLookup mappedfuncname state) in
      let mappedrettype = getRetType mappedfunctype in

      -- Generate code for the mapped function
      let cudaret = codegenCUDA state (TmVar {ident = mappedfuncname}) in

      let hostfuncname = hostname mappedfuncname in
      let globalfuncname = gblname mappedfuncname in
      let devicefuncname = cudaret.code in
      let inarr = "inarr" in
      let outarr = "outarr" in
      let cuda_inarr = concat "cuda_" inarr in
      let cuda_outarr = concat "cuda_" outarr in
      let numargs = addi (length argnames) 1 in
      if gti numargs 5 then
        let _ = print (strJoin "" ["Number of arguments to ", hostfuncname, ": ", int2string numargs]) in
        error "Number of arguments cannot exceed 5."
      else -- carry on


      -- Arguments to be applied that were generated in the global CUDA function
      let genargs = ["v"] in
      let genargs = if t.includeIndexArg then cons "i" genargs else genargs in

      -- Generate the global device body
      let globalbody = strJoin "" [
        "__global__ void ", globalfuncname, "(", strJoin ", " (concat argstyped
                                                                      (map (concat "value *") [inarr, outarr])),
                                                 ", int n)\n",
        "{\n",
        "\tint i;\n",
        "\tint start = threadIdx.x * ", int2string t.elemPerThread, ";\n",
        "\tint end = start + ", int2string t.elemPerThread, ";\n",
        "\tif (end > n)\n",
        "\t\tend = n;\n\n",
        "\tfor (i = start; i < end; ++i) {\n",
        "\t\t", type2cudastr (getSeqType arrtype), "v;\n",
        "\t\t", extractIdxCUDA2OCaml "v"
                                     inarr
                                     "i"
                                     (getSeqType arrtype),
                ";\n",
        "\t\t", assignIdxCUDA2OCaml outarr
                                    "i"
                                    (strJoin "" [devicefuncname, "(", strJoin ", " (concat argnames genargs), ")"])
                                    mappedrettype,
                ";\n",
        "\t}\n",
        "}"
      ] in

      -- Generate the prototype
      let prototype = strJoin "" [
        "value ", hostfuncname, "(",
        strJoin ", " (map (concat "value ") (concat argnames [inarr])),
        ")"
      ] in
      -- Generate the host body
      let hostbody = strJoin "" [
        prototype,
        "\n{\n",
        -- The CAML interface for the parameters
        "\tCAMLparam", int2string numargs, "(",
           strJoin ", " (concat argnames [inarr]), ");\n",
        "\tCAMLlocal1(", outarr, ");\n",
        "\tint n = Wosize_val(", inarr, ");\n\n",
        "\tvalue *", cuda_inarr, ";\n",
        "\tvalue *", cuda_outarr, ";\n",
        "\tcudaMalloc(&", cuda_inarr, ", n * sizeof(value));\n",
        "\tcudaMalloc(&", cuda_outarr, ", n * sizeof(value));\n",
        "\tcudaMemcpy(", cuda_inarr, ", Op_val(", inarr, "), n * sizeof(value), cudaMemcpyHostToDevice);\n\n",
        "\t", globalfuncname, "<<<1,(n + ", int2string (subi t.elemPerThread 1),
                              ") / ", int2string t.elemPerThread, ">>>(",
                              strJoin ", " (concat argsconved [cuda_inarr, cuda_outarr, "n"]), ");\n",
        "\t", outarr, " = caml_alloc(n, Tag_val(", inarr, "));\n",
        "\tcudaDeviceSynchronize();\n\n",
        "\tcudaMemcpy(Op_val(", outarr, "), ", cuda_outarr, ", n * sizeof(value), cudaMemcpyDeviceToHost);\n\n",
        "\tcudaFree(", cuda_inarr, ");\n",
        "\tcudaFree(", cuda_outarr, ");\n\n",
        "\tCAMLreturn(", outarr, ");\n",
        "}"
      ] in
      {{{cudaret with globalfuncs = strset_add globalbody strset_new}
                 with hostprototypes = strset_add prototype strset_new}
                 with hostfuncs = strset_add hostbody strset_new}
end

lang MExprCGCUDA = VarCGCUDA + AppCGCUDA + FunCGCUDA + LetCGCUDA +
                   RecLetsCGCUDA + ConstCGCUDA + IntCGCUDA +
                   ArithIntCGCUDA + ArithFloatCGCUDA + BoolCGCUDA +
                   CmpCGCUDA + CUDACGCUDA
