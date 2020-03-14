-- CUDA Code generation

include "common.mc"
include "types.mc"

let mapi = lam f. lam seq.
  recursive let work = lam i. lam f. lam seq.
      if null seq then []
      else cons (f i (head seq)) (work (addi i 1) f (tail seq))
  in
  work 0 f seq

let last = lam seq. nth seq (subi (length seq) 1)
let init = lam seq. slice seq 0 (subi (length seq) 1)

let hostname = lam s. concat "gpuhost_" s
let gblname = lam s. concat "gpuglobal_" s
let devname = lam s. concat "gpudevice_" s

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

let typeOCamlTag = use MExprCGExt in
  lam tpe.
  let perror = lam _.
    let _ = dprint tpe in
    let _ = print "\n" in
    error "typeOCamlTag: Above type is invalid."
  in
  match tpe with TyInt () then
    "0" -- verified on sleipner that Tag_val(int array) = 0
  else match tpe with TyFloat () then
    "Double_array_tag"
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

let accessIdxCUDA2OCaml = use MExprCGExt in
  lam name. lam idx. lam tpe.
  let perror = lam _.
    let _ = dprint tpe in
    let _ = print "\n" in
    error "accessIdxCUDA2OCaml: Above type is invalid."
  in
  match tpe with TyInt () then
    strJoin "" ["Int_val(Field(", name, ", ", idx, "))"]
  else match tpe with TyFloat () then
    strJoin "" ["Double_field(", name, ", ", idx, ")"]
  else perror ()

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
    sem codegenGetExprType (state : CodegenState) =
    -- Intentionally left blank

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
                let arrtype = codegenGetExprType state (TmVar t2) in
                let elemtype = match arrtype with TySeq t3 then t3.tpe else perror () in
                let code = typeOCaml2CUDA (strJoin "" ["(", t2.ident, "[", idxcgr.code, "])"]) elemtype in
                Some ({idxcgr with code = code})
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
    let prototype = strJoin "" ["__device__ inline ", typeprefix, " ", name, argdecls] in
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
    | CAddi _ -> genconstfun "int" "gpu_addi" "(int x, int y)" "{return x + y;}"
    | CSubi _ -> genconstfun "int" "gpu_subi" "(int x, int y)" "{return x - y;}"
    | CMuli _ -> genconstfun "int" "gpu_muli" "(int x, int y)" "{return x * y;}"
    | CDivi _ -> genconstfun "int" "gpu_divi" "(int x, int y)" "{return x / y;}"
    | CModi _ -> genconstfun "int" "gpu_modi" "(int x, int y)" "{return x % y;}"
end

lang ArithFloatCGCUDA = MExprCGExt
    sem codegenConstCUDA (state : CodegenState) =
    | CAddf _ -> genconstfun "double" "gpu_addf" "(double x, double y)" "{return x + y;}"
    | CSubf _ -> genconstfun "double" "gpu_subf" "(double x, double y)" "{return x - y;}"
    | CMulf _ -> genconstfun "double" "gpu_mulf" "(double x, double y)" "{return x * y;}"
    | CDivf _ -> genconstfun "double" "gpu_divf" "(double x, double y)" "{return x / y;}"
    | CInt2float _ -> genconstfun "double" "gpu_int2float" "(int x)" "{return (double) x;}"
end

lang BoolCGCUDA = MExprCGExt
    sem codegenConstCUDA (state : CodegenState) =
    | CBool b -> {cgr_new with code = if b.val then "true" else "false"}
    | CNot _ -> genconstfun "bool" "gpu_not" "(bool a)" "{return !a;}"
    | CAnd _ -> genconstfun "bool" "gpu_and" "(bool a, bool b)" "{return a && b;}"
    | COr _ -> genconstfun "bool" "gpu_or" "(bool a, bool b)" "{return a || b;}"

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
    | CEqi _ -> genconstfun "bool" "gpu_eqi" "(int x, int y)" "{return x == y;}"
    | CLti _ -> genconstfun "bool" "gpu_lti" "(int x, int y)" "{return x < y;}"
end

lang CUDACGCUDA = MExprCGExt
    sem codegenGetExprType (state : CodegenState) =
    -- Intentionally left blank

    -- Generate C++ host functions that interfaces with the OCaml types
    sem codegenCUDA (state : CodegenState) =
    | TmCUDAMap t ->
      let inargs = concat ["packedInts", "packedFloats"] (mapi (lam i. lam _. concat "arg" (int2string i)) t.nonPackedArgs) in
      let outarr = "outarr" in
      let pIntIdx = 0 in
      let pFloatIdx = 1 in
      let offset = 2 in
      -- Extract the packed arguments --
      recursive let packedArgs = lam acc. lam pInt. lam pFloat. lam notPacked.
        let pos = length acc in
        let hasCurPos = lam arr. if not (null arr) then eqi (head arr).pos pos else false in

        if hasCurPos pInt then
          let ipos = int2string (subi (length t.packedInts) (length pInt)) in
          let accname = accessIdxCUDA2OCaml (nth inargs pIntIdx) ipos (TyInt ()) in
          packedArgs (concat acc [(accname, TyInt ())]) (tail pInt) pFloat notPacked
        else if hasCurPos pFloat then
          let fpos = int2string (subi (length t.packedFloats) (length pFloat)) in
          let accname = accessIdxCUDA2OCaml (nth inargs pFloatIdx) fpos (TyFloat ()) in
          packedArgs (concat acc [(accname, TyFloat ())]) pInt (tail pFloat) notPacked
        else if not (null notPacked) then
          let argpos = subi (length t.nonPackedArgs) (length notPacked) in
          let argstr = nth inargs (addi offset argpos) in
          let argtpe = codegenGetExprType state (head notPacked) in
          packedArgs (concat acc [(argstr, argtpe)]) pInt pFloat (tail notPacked)
        else
          acc -- no argument to unpack!
      in
      let unpackedArgs = packedArgs [] t.packedInts t.packedFloats t.nonPackedArgs in
      let argsconved = map (lam t. match t.1 with TySeq _ then concat "cuda_" t.0 else t.0) unpackedArgs in
      let argtypes = map (lam t. t.1) unpackedArgs in

      let inarrArgs = filter (lam t. match t.1 with TySeq _ then true else false) unpackedArgs in
      let inarrs = map (lam t. t.0) inarrArgs in
      let inarrsconved = map (concat "cuda_") inarrs in

      -- Last argument determines the size of the output (either by size or as a mapped-onto array)
      let nstr =
        if t.onlyIndexArg
        then last argsconved
        else strJoin "" ["Wosize_val(", last inarrs, ")"]
      in

      -- If the last argument was the size then it has served its purpose and can be ignored
      let argsconved = if t.onlyIndexArg then init argsconved else argsconved in

      let ret = extract_args [] t.func in
      let mappedfuncname = ret.1 in
      let mappedfunctype = codegenGetExprType state (cgs_envLookup mappedfuncname state) in
      let mappedrettype = getRetType mappedfunctype in

      -- Generate code for the mapped function
      let cudaret = codegenCUDA state (TmVar {ident = mappedfuncname}) in

      let hostfuncname = hostname mappedfuncname in
      let globalfuncname = gblname mappedfuncname in
      let devicefuncname = cudaret.code in
      let cuda_outarr = concat "cuda_" outarr in
      let numargs = length inargs in
      if gti numargs 5 then
        let _ = print (strJoin "" ["Number of arguments to ", hostfuncname, ": ", int2string numargs, "\n"]) in
        error "Number of arguments cannot exceed 5."
      else -- carry on

      let cudaargs = mapi (lam i. lam _. concat "cuda_arg" (int2string i)) unpackedArgs in
      let cudaargs = if t.onlyIndexArg then init cudaargs else cudaargs in
      let cudaargstyped = zipWith concat (map type2cudastr argtypes) cudaargs in

      -- Arguments to be applied that were generated in the global CUDA function
      let genargs = [] in
      let genargs = if t.onlyIndexArg then cons "i" genargs else cons "v" genargs in
      let genargs = if t.includeIndexArg then cons "i" genargs else genargs in

      -- Generate the global device body
      let globalbody = strJoin "" [
        "__global__ void ", globalfuncname, "(", strJoin ", " (concat cudaargstyped
                                                                      [concat "value *" outarr]),
                                                 ", int n)\n",
        "{\n",
        "\tint i;\n",
        "\tint start = ((blockIdx.x * blockDim.x) + threadIdx.x) * ", int2string t.elemPerThread, ";\n",
        "\tint end = start + ", int2string t.elemPerThread, ";\n",
        "\tif (end > n)\n",
        "\t\tend = n;\n\n",
        "\tfor (i = start; i < end; ++i) {\n",
        if t.onlyIndexArg then strJoin "" [
          "\t\t", assignIdxCUDA2OCaml outarr
                                      "i"
                                      (strJoin "" [devicefuncname,
                                                   "(",
                                                   strJoin ", " (concat cudaargs genargs),
                                                   ")"])
                                      mappedrettype,
                  ";\n"
        ]
        else strJoin "" [
          "\t\t", type2cudastr (getSeqType (last argtypes)), "v;\n",
          "\t\t", extractIdxCUDA2OCaml "v"
                                       (last cudaargs)
                                       "i"
                                       (getSeqType (last argtypes)),
                  ";\n",
          "\t\t", assignIdxCUDA2OCaml outarr
                                      "i"
                                      (strJoin "" [devicefuncname,
                                                   "(",
                                                   strJoin ", " (concat (init cudaargs) genargs),
                                                   ")"])
                                      mappedrettype,
                  ";\n"
        ],
        "\t}\n",
        "}"
      ] in

      -- Generate the prototype
      let prototype = strJoin "" [
        "value ", hostfuncname, "(",
        strJoin ", " (map (concat "value ") inargs),
        ")"
      ] in
      -- Generate the host body
      let hostbody = strJoin "" [
        prototype,
        "\n{\n",
        -- The CAML interface for the parameters
        "\tCAMLparam", int2string numargs, "(",
           strJoin ", " inargs, ");\n",
        "\tCAMLlocal1(", outarr, ");\n",
        "\tint n = ", nstr, ";\n\n",
        -- Determine number of blocks and threads
        "\tint threadsPerBlock;\n",
        "\tint elemPerBlock;\n",
        "\tint blockCount;\n",
        "\tcudaDeviceGetAttribute(&threadsPerBlock, cudaDevAttrMaxThreadsPerBlock, 0);\n",
        "\telemPerBlock = threadsPerBlock * ", int2string t.elemPerThread, ";\n",
        "\tblockCount = (n + elemPerBlock - 1) / elemPerBlock;\n",
        -- Allocate device memory
        "\tvalue *", cuda_outarr, ";\n",
        strJoin "" (
          map (lam e. strJoin "" ["\tvalue *", e, ";\n"]) inarrsconved
        ),
        "\tcudaMalloc(&", cuda_outarr, ", n * sizeof(value));\n",
        strJoin "" (
          zipWith (lam eCuda. lam e. strJoin "" ["\tcudaMalloc(&", eCuda, ", Wosize_val(", e, ") * sizeof(value));\n"])
                  inarrsconved
                  inarrs
        ),
        strJoin "" (
          zipWith (lam eCuda. lam e. strJoin "" ["\tcudaMemcpy(", eCuda, ", Op_val(", e, "), Wosize_val(", e, ") * sizeof(value), cudaMemcpyHostToDevice);\n"])
                  inarrsconved
                  inarrs
        ),
        "\n",
        "\t", globalfuncname, "<<<blockCount,threadsPerBlock>>>(", strJoin ", " (concat argsconved [cuda_outarr, "n"]), ");\n",
        "\t", outarr, " = caml_alloc(n, ", typeOCamlTag mappedrettype, ");\n",
        "\tcudaDeviceSynchronize();\n\n",
        "\tcudaMemcpy(Op_val(", outarr, "), ", cuda_outarr, ", n * sizeof(value), cudaMemcpyDeviceToHost);\n\n",
        "\tcudaFree(", cuda_outarr, ");\n",
        strJoin "" (
          map (lam eCuda. strJoin "" ["\tcudaFree(", eCuda, ");\n"]) inarrsconved
        ),
        "\n",
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
