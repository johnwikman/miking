-- macros

include "../codegen/ocaml.mc"

recursive let bind_ = use MExprCGOCaml in
  lam letexpr. lam inexpr.
  match letexpr with TmLet t then
    TmLet {t with inexpr = bind_ t.inexpr inexpr}
  else match letexpr with TmRecLets t then
    TmRecLets {t with inexpr = bind_ t.inexpr inexpr}
  else
    inexpr
end

let bindall_ = lam exprs. foldl1 bind_ exprs

-- constants --
let unit_ = use MExprCGOCaml in
  TmConst {val = CUnit ()}

let int_ = use MExprCGOCaml in
  lam i.
  TmConst {val = CInt {val = i}}

let float_ = use MExprCGOCaml in
  lam i.
  TmConst {val = CFloat {val = i}}

let char_ = use MExprCGOCaml in
  lam c.
  TmConst {val = CChar {val = c}}

let var_ = use MExprCGOCaml in
  lam ident.
  TmVar {ident = ident}

let seq_ = use MExprCGOCaml in
  lam s.
  TmSeq {tms = s}

let cseq_ = use MExprCGOCaml in
  lam s.
  TmConst {val = CSeq {tms = s}}

let str_ = use MExprCGOCaml in
  lam s. cseq_ (map char_ s)

-- types --
let tyarrow_ = use MExprCGOCaml in
  lam from. lam to.
  TyArrow {from = from, to = to}

let tyarrows_ = use MExprCGOCaml in
  lam tpes.
  foldr1 (lam e. lam acc.
    TyArrow {from = e, to = acc}
  ) tpes

let tydyn_ = use MExprCGOCaml in
  TyDyn ()

let tyunit_ = use MExprCGOCaml in
  TyUnit ()

let tychar_ = use MExprCGOCaml in
  TyChar ()

let tystr_ = use MExprCGOCaml in
  TyString ()

let tyseq_ = use MExprCGOCaml in
  lam tpe.
  TySeq {tpe = tpe}

let typrod_ = use MExprCGOCaml in
  lam tpes.
  TyProd {tpes = tpes}

let tyrecord_ = use MExprCGOCaml in
  lam tpes.
  TyRecord {tpes = tpes}

let tycon_ = use MExprCGOCaml in
  lam ident.
  TyCon {ident = ident}

let tyint_ = use MExprCGOCaml in
  TyInt ()

let tyfloat_ = use MExprCGOCaml in
  TyFloat ()

let tybool_ = use MExprCGOCaml in
  TyBool ()

let tyapp_ = use MExprCGOCaml in
  lam lhs. lam rhs.
  TyApp {rhs = lhs, rhs = rhs}

-- app macros --
let app_ = use MExprCGOCaml in
  lam lhs. lam rhs.
  TmApp {lhs = lhs, rhs = rhs}

let app1f_ = use MExprCGOCaml in
  lam f. lam a1.
  app_ f a1

let app2f_ = use MExprCGOCaml in
  lam f. lam a1. lam a2.
  app_ (app1f_ f a1) a2

let app3f_ = use MExprCGOCaml in
  lam f. lam a1. lam a2. lam a3.
  app_ (app2f_ f a1 a2) a3

let app4f_ = use MExprCGOCaml in
  lam f. lam a1. lam a2. lam a3. lam a4.
  app_ (app3f_ f a1 a2 a3) a4

let app5f_ = use MExprCGOCaml in
  lam f. lam a1. lam a2. lam a3. lam a4. lam a5.
  app_ (app4f_ f a1 a2 a3 a4) a5

let app6f_ = use MExprCGOCaml in
  lam f. lam a1. lam a2. lam a3. lam a4. lam a5. lam a6.
  app_ (app5f_ f a1 a2 a3 a4 a5) a6

let app7f_ = use MExprCGOCaml in
  lam f. lam a1. lam a2. lam a3. lam a4. lam a5. lam a6. lam a7.
  app_ (app6f_ f a1 a2 a3 a4 a5 a6) a7

let app8f_ = use MExprCGOCaml in
  lam f. lam a1. lam a2. lam a3. lam a4. lam a5. lam a6. lam a7. lam a8.
  app_ (app7f_ f a1 a2 a3 a4 a5 a6 a7) a8

-- function/control-flow macros --
let let_ = use MExprCGOCaml in
  lam ident. lam tpe. lam body.
  TmLet {ident = ident, tpe = Some (tpe), body = body, inexpr = unit_}

let reclets_add = use MExprCGOCaml in
  lam ident. lam tpe. lam body. lam reclets.
  match reclets with TmRecLets t then
    TmRecLets {t with bindings = cons {ident = ident, tpe = Some (tpe), body = body} t.bindings}
  else
    error "reclets_add: Must add reclet to a TmRecLets"

let reclets_empty = use MExprCGOCaml in
  TmRecLets {bindings = [], inexpr = unit_}

let lam_ = use MExprCGOCaml in
  lam ident. lam tpe. lam body.
  TmLam {ident = ident, tpe = Some (tpe), body = body}

let if_ = use MExprCGOCaml in
  lam cond. lam thn. lam els.
  TmIf {cond = cond, thn = thn, els = els}

-- builtin functions --
let print_ = use MExprCGOCaml in
  app1f_ (TmConst {val = CPrint ()})

let error_ = use MExprCGOCaml in
  app1f_ (TmConst {val = CError ()})

let int2char_ = use MExprCGOCaml in
  app1f_ (TmConst {val = CInt2char ()})

let char2int_ = use MExprCGOCaml in
  app1f_ (TmConst {val = CChar2int ()})

let length_ = use MExprCGOCaml in
  app1f_ (TmConst {val = CLength ()})

let negi_ = use MExprCGOCaml in
  app1f_ (TmConst {val = CNegi ()})

let lti_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CLti ()})

let eqi_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CEqi ()})

let neqi_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CNeqi ()})

let gti_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CGti ()})

let geqi_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CGeqi ()})

let leqi_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CLeqi ()})

let addi_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CAddi ()})

let subi_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CSubi ()})

let muli_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CMuli ()})

let divi_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CDivi ()})

let modi_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CModi ()})

let addf_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CAddf ()})

let subf_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CSubf ()})

let mulf_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CMulf ()})

let divf_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CDivf ()})

let floorfi_ = use MExprCGOCaml in
  app1f_ (TmConst {val = CFloorfi ()})

let ceilfi_ = use MExprCGOCaml in
  app1f_ (TmConst {val = CCeilfi ()})

let roundfi_ = use MExprCGOCaml in
  app1f_ (TmConst {val = CRoundfi ()})

let int2float_ = use MExprCGOCaml in
  app1f_ (TmConst {val = CInt2float ()})

let string2float_ = use MExprCGOCaml in
  app1f_ (TmConst {val = CString2float ()})

let cons_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CCons ()})

let nth_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CNth ()})

let concat_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CConcat ()})

let slice_ = use MExprCGOCaml in
  app3f_ (TmConst {val = CSlice ()})

let makeseq_ = use MExprCGOCaml in
  app2f_ (TmConst {val = CMakeseq ()})

-- cuda macros --
let cudamapxpept_ = use MExprCGOCaml in
  lam ept. lam f. lam arr.
  TmCUDAMap {elemPerThread = ept,
             autoDetermineParallelization = false,
             autoScanElemPerThread = false,
             includeIndexArg = false,
             onlyIndexArg = false,
             onlyIndexArgSize = int_ 0,
             func = f,
             array = arr,
             packedInts = [],
             packedFloats = [],
             nonPackedArgs = []}

let cudamapixpept_ = use MExprCGOCaml in
  lam ept. lam f. lam arr.
  TmCUDAMap {elemPerThread = ept,
             autoDetermineParallelization = false,
             autoScanElemPerThread = false,
             includeIndexArg = true,
             onlyIndexArg = false,
             onlyIndexArgSize = int_ 0,
             func = f,
             array = arr,
             packedInts = [],
             packedFloats = [],
             nonPackedArgs = []}

let cudainitxpept_ = use MExprCGOCaml in
  lam ept. lam size. lam f.
  TmCUDAMap {elemPerThread = ept,
             autoDetermineParallelization = false,
             autoScanElemPerThread = false,
             includeIndexArg = false,
             onlyIndexArg = true,
             onlyIndexArgSize = size,
             func = f,
             array = seq_ [],
             packedInts = [],
             packedFloats = [],
             nonPackedArgs = []}

let cudamapautoept_ = use MExprCGOCaml in
  lam f. lam arr.
  TmCUDAMap {elemPerThread = int_ 1, -- temporary expression, will be replaced in the codegen
             autoDetermineParallelization = false,
             autoScanElemPerThread = true,
             includeIndexArg = false,
             onlyIndexArg = false,
             onlyIndexArgSize = int_ 0,
             func = f,
             array = arr,
             packedInts = [],
             packedFloats = [],
             nonPackedArgs = []}

let cudamapiautoept_ = use MExprCGOCaml in
  lam f. lam arr.
  TmCUDAMap {elemPerThread = int_ 1, -- temporary expression, will be replaced in the codegen
             autoDetermineParallelization = false,
             autoScanElemPerThread = true,
             includeIndexArg = true,
             onlyIndexArg = false,
             onlyIndexArgSize = int_ 0,
             func = f,
             array = arr,
             packedInts = [],
             packedFloats = [],
             nonPackedArgs = []}

let cudainitautoept_ = use MExprCGOCaml in
  lam size. lam f.
  TmCUDAMap {elemPerThread = int_ 1, -- temporary expression, will be replaced in the codegen
             autoDetermineParallelization = false,
             autoScanElemPerThread = true,
             includeIndexArg = false,
             onlyIndexArg = true,
             onlyIndexArgSize = size,
             func = f,
             array = seq_ [],
             packedInts = [],
             packedFloats = [],
             nonPackedArgs = []}

let cudamappredictive_ = use MExprCGOCaml in
  lam f. lam arr.
  TmCUDAMap {elemPerThread = int_ 1,
             autoDetermineParallelization = true,
             autoScanElemPerThread = false,
             includeIndexArg = false,
             onlyIndexArg = false,
             onlyIndexArgSize = int_ 0,
             func = f,
             array = arr,
             packedInts = [],
             packedFloats = [],
             nonPackedArgs = []}

let cudamapipredictive_ = use MExprCGOCaml in
  lam f. lam arr.
  TmCUDAMap {elemPerThread = int_ 1,
             autoDetermineParallelization = true,
             autoScanElemPerThread = false,
             includeIndexArg = true,
             onlyIndexArg = false,
             onlyIndexArgSize = int_ 0,
             func = f,
             array = arr,
             packedInts = [],
             packedFloats = [],
             nonPackedArgs = []}

let cudainitpredictive_ = use MExprCGOCaml in
  lam size. lam f.
  TmCUDAMap {elemPerThread = int_ 1,
             autoDetermineParallelization = true,
             autoScanElemPerThread = false,
             includeIndexArg = false,
             onlyIndexArg = true,
             onlyIndexArgSize = size,
             func = f,
             array = seq_ [],
             packedInts = [],
             packedFloats = [],
             nonPackedArgs = []}
