-- macros

include "codegen-ocaml.mc"

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
  lam f. lam a.
  app_ f a

let app2f_ = use MExprCGOCaml in
  lam f. lam a. lam b.
  app_ (app1f_ f a) b

let app3f_ = use MExprCGOCaml in
  lam f. lam a. lam b. lam c.
  app_ (app2f_ f a b) c

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
let cuda_mapintarray_ = use MExprCGOCaml in
  lam ept. lam f. lam arr.
  TmCUDAMap {elemPerThread = ept, func = f, array = arr}
