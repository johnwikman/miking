-- OCaml code generator

-- Has: 
-- let int2string = lam i. ... in
-- recursive let factorial = lam n.
--     if eqi n 0 then 1 else (muli n (factorial (subi n 1)))
-- in
-- print (int2string (factorial 5))
--
--
-- Want:
-- open Printf
-- let int2string i = ...
-- in
-- let rec factorial n = if n == 0 then 1 else n * (factorial (n - 1)) in
-- printf (int2string (factorial 5))
--
-- What needs to happen:
--  The lambdas need to appear on the left side of the equals sign

include "mexpr/ast.mc"
include "string.mc"

let spacing = lam indent. makeseq indent ' '
let newline = lam indent. concat "\n" (spacing indent)

let incr = lam indent. addi indent 4

lang VarOCamlCode = VarAst
    sem ocamlcodegen (indent : Int) =
    | TmVar x -> x.ident
end

lang AppOCamlCode = AppAst
    sem ocamlcodegen (indent : Int) =
    | TmApp t ->
      strJoin "" [ocamlcodegen indent t.lhs, " ", "(", ocamlcodegen indent t.rhs, ")"]
end

lang FunOCamlCode = FunAst
    sem ocamlcodegen (indent : Int) =
    | TmLam t -> let _ = dprint t in error "Encountered an isolated lambda"
end

lang LetOCamlCode = LetAst + FunAst
    sem ocamlcodegen (indent : Int) =
    | TmLet t ->
      -- Find all the chained lambdas
      --   acc.0: The variable names
      --   acc.1: The trailing expression
      recursive let chainlambdas = lam acc. lam expr.
        match expr with TmLam t1 then
          chainlambdas (concat acc [t1.ident]) t1.body
        else
          (acc, expr)
      in
      let argres = chainlambdas [] t.body in
      let args = argres.0 in
      let letexpr = argres.1 in
      strJoin "" ["let ", t.ident, " ", strJoin " " args, " =", newline (incr indent),
                  ocamlcodegen (incr indent) letexpr, newline indent,
                  "in", newline indent,
                  ocamlcodegen indent t.inexpr]
end

lang RecLetsOCamlCode = RecLetsAst + FunAst
    sem ocamlcodegen (indent : Int) =
    | TmRecLets t ->
      -- Initiate a list with the code generated code
      let list = ["let "] in
      recursive let generatelets = lam genacc. lam l.
        if null l then
          genacc
        else
          recursive let chainlambdas = lam acc. lam expr.
            match expr with TmLam t1 then
              chainlambdas (concat acc [t1.ident]) t1.body
            else
              (acc, expr)
          in
          let argres = chainlambdas [] (head l).body in
          let name = (head l).ident in
          let args = argres.0 in
          let letexpr = argres.1 in
          let prefix = if null genacc then "let rec " else "    and " in
          let updatedgenacc = concat genacc [
            prefix, name, " ", strJoin " " args, " =", newline (addi 4 (incr indent)),
            ocamlcodegen (addi 4 (incr indent)) letexpr, newline indent
          ]
          in
          generatelets updatedgenacc (tail l)
      in
      let list = concat (generatelets [] t.bindings) [
        "in", newline indent,
        ocamlcodegen indent t.inexpr
      ]
      in
      strJoin "" list
end

lang ConstOCamlCode = ConstAst
    sem ocamlconstgen (indent : Int) =
    -- intentionally left blank

    sem ocamlcodegen (indent : Int) =
    | TmConst c -> ocamlconstgen indent c.val
end

lang UnitOCamlCode = UnitAst
    sem ocamlconstgen (indent : Int) =
    | CUnit -> "()"
end

lang IntOCamlCode = IntAst
    sem ocamlconstgen (indent : Int) =
    | CInt i -> int2string i.val
end

lang ArithIntOCamlCode = ArithIntAst
    syn Const =
    | CModi {}
    | CDivi {}
    | CNegi {}

    sem ocamlconstgen (indent : Int) =
    | CAddi _ -> "( + )"
    | CSubi _ -> "( - )"
    | CMuli _ -> "( * )"
    | CDivi _ -> "( / )"
    | CModi _ -> "( mod )"
    | CNegi _ -> "( ~- )"
end

lang BoolOCamlCode = BoolAst
    sem ocamlconstgen (indent : Int) =
    | CBool b -> if b.val then "true" else "false"
    | CNot _ -> "not"
    | CAnd _ -> "( && )"
    | COr _ -> "( || )"

    sem ocamlcodegen (indent : Int) =
    | TmIf t ->
      strJoin "" [
        "if ", ocamlcodegen indent t.cond, " then", newline (incr indent),
        ocamlcodegen (incr indent) t.thn, newline indent,
        "else", newline (incr indent),
        ocamlcodegen (incr indent) t.els
      ]
end

lang CmpOCamlCode = CmpAst
    sem ocamlconstgen (indent : Int) =
    | CEqi _ -> "( = )"
    | CLti _ -> "( < )"
end

lang CharOCamlCode = CharAst
    syn Const =
    | CChar2int {}
    | CInt2char {}

    sem ocamlconstgen (indent : Int) =
    | CChar c -> if eqchar c.val (head "\n") then
                   [head "'", head "\\", 'n', head "'"]
                 else
                   [head "'", c.val, head "'"]
    | CChar2int _ -> "int_of_char"
    | CInt2char _ -> "char_of_int"
end

lang SeqOCamlCode = SeqAst
    syn Const =
    | CLength {}
    | CCons {}
    | CConcat {}
    | CSlice {}
    | CReverse {}

    sem ocamlconstgen (indent : Int) =
    | CNth _ -> "Array.get"
    | CLength _ -> "Array.length"
    | CCons _ -> "(fun x xs -> Array.append [|x|] xs)"
    | CConcat _ -> "Array.append"
    | CSlice _ -> "(fun xs start len -> Array.sub xs (min ((Array.length xs) - 1) start) (min ((Array.length xs) - start) len))"
    | CReverse _ -> "List.rev"
    | CSeq t -> strJoin "" ["[|", strJoin "; " (map (ocamlcodegen indent) t.tms), "|]"]

    sem ocamlcodegen (indent : Int) =
    | TmSeq t -> strJoin "" ["[|", strJoin "; " (map (ocamlcodegen indent) t.tms), "|]"]
end

lang TupleOCamlCode = TupleAst
    sem ocamlcodegen (indent : Int) =
    | TmTuple t -> strJoin "" ["(", strJoin ", " (map (ocamlcodegen indent) t.tms), ")"]
    | TmProj t ->
      recursive let mkproj = lam acc. lam idx.
        if eqi idx (length [0,1,2]) then -- TEMP: We do not know the length of the tuple here, so we cannot project out of it!
          acc
        else if eqi idx t.idx then
          concat acc ["x"]
        else
          concat acc ["_"]
      in
      let projfun = strJoin "" ["(fun t -> let (", strJoin "," (mkproj [] 0), ") = t in x)"] in
      strJoin "" [projfun, " (", ocamlcodegen indent t.tup, ")"]
end

lang MExprOCamlCode = VarOCamlCode + AppOCamlCode + FunOCamlCode + LetOCamlCode +
                      RecLetsOCamlCode + ConstOCamlCode + UnitOCamlCode + IntOCamlCode +
                      ArithIntOCamlCode + BoolOCamlCode + CmpOCamlCode + CharOCamlCode +
                      SeqOCamlCode + TupleOCamlCode
    syn Expr =
    | TmMain {body : Expr}

    syn Const =
    | CPrint {}

    sem ocamlcodegen (indent : Int) =
    | TmMain t ->
      strJoin "" ["open Printf", "\n\n",
                  "let main =", newline (incr indent),
                  ocamlcodegen (incr indent) t.body]

    sem ocamlconstgen (indent : Int) =
    | CPrint _ -> "(fun s -> printf \"%s\" (String.of_seq (Array.to_seq s)))"
end

mexpr
use MExprOCamlCode in
recursive let bind_ = lam letexpr. lam inexpr.
  match letexpr with TmLet t then
    TmLet {t with inexpr = bind_ t.inexpr inexpr}
  else match letexpr with TmRecLets t then
    TmRecLets {t with inexpr = bind_ t.inexpr inexpr}
  else
    inexpr
in

-- constants:
let unit_ = TmConst {val = CUnit ()} in
let int_ = lam i. TmConst {val = CInt {val = i}} in
let char_ = lam c. TmConst {val = CChar {val = c}} in
let var_ = lam ident. TmVar {ident = ident} in
let seq_ = lam s. TmSeq {tms = s} in
let str_ = lam s. seq_ (map char_ s) in

-- macros:
let app_ = lam lhs. lam rhs. TmApp {lhs = lhs, rhs = rhs} in
let app1f_ = lam f. lam arg. app_ f arg in
let app2f_ = lam f. lam lhs. lam rhs. app_ (app1f_ f lhs) rhs in
let app3f_ = lam f. lam a. lam b. lam c. app_ (app2f_ f a b) c in
let let_ = lam ident. lam body. TmLet {ident = ident, tpe = None (), body = body, inexpr = unit_} in
let reclets_add = lam ident. lam body. lam reclets.
  match reclets with TmRecLets t then
    TmRecLets {t with bindings = cons {ident = ident, tpe = None (), body = body} t.bindings}
  else
    error "Must add reclet to a TmRecLets"
in
let reclets_empty = TmRecLets {bindings = [], inexpr = unit_} in
let lam_ = lam ident. lam body. TmLam {ident = ident, tpe = None (), body = body} in
let if_ = lam cond. lam thn. lam els. TmIf {cond = cond, thn = thn, els = els} in

-- funcs:
let print_ = app1f_ (TmConst {val = CPrint {}}) in
let int2char_ = app1f_ (TmConst {val = CInt2char {}}) in
let char2int_ = app1f_ (TmConst {val = CChar2int {}}) in
let length_ = app1f_ (TmConst {val = CLength {}}) in
let negi_ = app1f_ (TmConst {val = CNegi {}}) in
let lti_ = app2f_ (TmConst {val = CLti {}}) in
let eqi_ = app2f_ (TmConst {val = CEqi {}}) in
let addi_ = app2f_ (TmConst {val = CAddi {}}) in
let subi_ = app2f_ (TmConst {val = CSubi {}}) in
let muli_ = app2f_ (TmConst {val = CMuli {}}) in
let divi_ = app2f_ (TmConst {val = CDivi {}}) in
let modi_ = app2f_ (TmConst {val = CModi {}}) in
let cons_ = app2f_ (TmConst {val = CCons {}}) in
let nth_ = app2f_ (TmConst {val = CNth {}}) in
let concat_ = app2f_ (TmConst {val = CConcat {}}) in
let slice_ = app3f_ (TmConst {val = CSlice {}}) in

let func_head =
  let_ "head"
       (lam_ "s"
             (nth_ (var_ "s") (int_ 0)))
in

let func_tail =
  let_ "tail"
       (lam_ "s"
             (slice_ (var_ "s")
                     (int_ 1)
                     (length_ (var_ "s"))))
in

let func_null =
  let_ "null" (
    lam_ "l" (
      eqi_ (length_ (var_ "l")) (int_ 0)
    )
  )
in

let func_map =
  reclets_add "map" (
    lam_ "f" (lam_ "seq" (
      if_ (app1f_ (var_ "null") (var_ "seq"))
          (seq_ [])
          (cons_ (app1f_ (var_ "f")
                         (app1f_ (var_ "head") (var_ "seq")))
                 (app2f_ (var_ "map")
                         (var_ "f")
                         (app1f_ (var_ "tail") (var_ "seq"))))
    ))
  ) (reclets_empty)
in

let func_int2string =
  let_ "int2string" (
    lam_ "n" (
      let recs =
        reclets_add "int2string_rechelper" (
          lam_ "n" (
            if_ (lti_ (var_ "n") (int_ 10))
                (seq_ [
                  int2char_ (addi_ (var_ "n")
                                   (char2int_ (char_ '0')))
                 ])
                (let d =
                  let_ "d" (
                    seq_ [
                      int2char_ (addi_ (modi_ (var_ "n") (int_ 10))
                                       (char2int_ (char_ '0')))
                    ]
                  )
                 in
                 bind_ d (
                  concat_ (app1f_ (var_ "int2string_rechelper")
                                  (divi_ (var_ "n") (int_ 10)))
                          (var_ "d")
                ))
          )
        ) (reclets_empty)
      in
      bind_ recs (
        if_ (lti_ (var_ "n") (int_ 0))
            (cons_ (char_ '_')
                   (app_ (var_ "int2string_rechelper")
                         (negi_ (var_ "n"))))
            (app_ (var_ "int2string_rechelper") (var_ "n"))
      )
    )
  )
in

let func_factorial =
  reclets_add "factorial" (
    lam_ "n" (
      if_ (eqi_ (var_ "n") (int_ 0))
          (int_ 1)
          (muli_ (var_ "n")
                 (app_ (var_ "factorial")
                       (subi_ (var_ "n") (int_ 1))))
    )
  ) (reclets_empty)
in

let prog = func_head in
let prog = bind_ prog func_tail in
let prog = bind_ prog func_null in
let prog = bind_ prog func_map in
let prog = bind_ prog func_int2string in
let prog = bind_ prog func_factorial in
let prog = bind_ prog (let_ "v" (int_ 10)) in
let prog = bind_ prog (let_ "res" (app_ (var_ "factorial")
                                        (var_ "v"))) in
let prog = bind_ prog (let_ "printstr" (concat_ (str_ "factorial ")
                                                (concat_ (app_ (var_ "int2string")
                                                               (var_ "v"))
                                                         (concat_ (str_ " = ")
                                                                  (concat_ (app_ (var_ "int2string")
                                                                                 (var_ "res"))
                                                                           (str_ "\n")))))) in
let prog = bind_ prog (let_ "_" (print_ (var_ "printstr"))) in

let res = ocamlcodegen 0 (TmMain {body = prog}) in

let _ = print "\n\n" in
let _ = print res in
let _ = print "\n\n" in

()
