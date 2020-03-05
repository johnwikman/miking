-- CUDA Code generation

include "codegen-common.mc"
include "codegen-types.mc"

let mapi = lam f. lam seq.
  recursive let work = lam i. lam f. lam seq.
      if null seq then []
      else cons (f i (head seq)) (work (addi i 1) f (tail seq))
  in
  work 0 f seq

lang VarCGCUDA = MExprCGExt
    sem codegenCUDA (state : CodegenState) =
    | TmVar x ->
      let v = cgs_envLookup x.ident state in
      match v with TmLet t then
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

      let devicebody = strJoin "" [
        "__device__ ", rettypestr, "gpudevice_", t.ident, "(", strJoin ", " argdecls, ")\n",
        "{\n",
        "\treturn ", cudaret.code, ";\n",
        "}"
      ] in
      {cudaret with devicefuncs = strset_add devicebody cudaret.devicefuncs}
end

lang RecLetsCGCUDA = MExprCGExt
    sem codegenCUDA (state : CodegenState) =
    | TmRecLets t ->
      let _ = dprint (TmRecLets t) in
      let _ = print "\n" in
      error "TmRecLets not yet implemented in codegenCUDA"
end

lang ConstCGCUDA = MExprCGExt
    sem codegenConstCUDA (state : CodegenState) =
    -- intentionally left blank

    sem codegenCUDA (state : CodegenState) =
    | TmConst c -> codegenConstCUDA state c.val
end

lang IntCGCUDA = MExprCGExt
    sem codegenConstCUDA (state : CodegenState) =
    | CInt i -> {cgr_new with code = int2string i.val}
end

lang ArithIntCGCUDA = MExprCGExt
    sem codegenConstCUDA (state : CodegenState) =
    | CAddi _ -> {{cgr_new with code = "gpu_addi"}
                           with devicefuncs = strset_add "__device__ int gpu_addi(int x, int y) {return x + y;}" strset_new}
    | CSubi _ -> {{cgr_new with code = "gpu_subi"}
                           with devicefuncs = strset_add "__device__ int gpu_subi(int x, int y) {return x - y;}" strset_new}
    | CMuli _ -> {{cgr_new with code = "gpu_muli"}
                           with devicefuncs = strset_add "__device__ int gpu_muli(int x, int y) {return x * y;}" strset_new}
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

      let hostfuncname = concat "gpuhost_" mappedfuncname in
      let globalfuncname = concat "gpuglobal_" mappedfuncname in
      let devicefuncname = concat "gpudevice_" mappedfuncname in
      let inarr = "inarr" in
      let outarr = "outarr" in
      let cuda_inarr = concat "cuda_" inarr in
      let cuda_outarr = concat "cuda_" outarr in
      let numargs = addi (length argnames) 1 in
      if gti numargs 5 then
        let _ = print (strJoin "" ["Number of arguments to ", hostfuncname, ": ", int2string numargs]) in
        error "Number of arguments cannot exceed 5."
      else -- carry on


      -- Generate code for the mapped function
      let cudaret = codegenCUDA state (TmVar {ident = mappedfuncname}) in

      -- Generate the global device body
      let globalbody = strJoin "" [
        "__global__ void ", globalfuncname, "(", strJoin ", " (concat argstyped
                                                                      (map (concat "value *") [inarr, outarr])),
                                                 ", int n)\n",
        "{\n",
        "\tint i;\n",
        "\tint start = threadIdx.x;\n",
        "\tint end = start + ", int2string t.elemPerThread, ";\n",
        "\tif (end > n)\n",
        "\t\tend = n;\n\n",
        "\tfor (i = start; i < end; ++i) {\n",
        "\t\t", type2cudastr mappedrettype, "v = ", typeOCaml2CUDA (strJoin "" [inarr, "[i]"]) (getSeqType arrtype), ";\n",
        "\t\t", outarr, "[i] = ", typeCUDA2OCaml (strJoin "" [devicefuncname, "(", strJoin ", " (concat argnames ["v"]), ")"])
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
        "\t// The ", outarr, " must reside on the minor heap for the following cudaMemcpy to succeed.\n",
        "\t// (Possible garbage collector issues otherwise...)\n",
        "\t// (Though should never be any issues since neither the previous or new value are blocks...)\n",
        "\tCAMLassert(Is_young((value) ", outarr, "));\n",
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
                   ArithIntCGCUDA + CUDACGCUDA
