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
    | TmVar x -> x
end

lang AppOCamlCode = AppAst
    sem ocamlcodegen (indent : Int) =
    | TmApp t ->
      let lhs = t.0 in
      let rhs = t.1 in
      strJoin "" [ocamlcodegen indent lhs, " ", "(", ocamlcodegen indent rhs, ")"]
end

lang FunOCamlCode = FunAst
    sem ocamlcodegen (indent : Int) =
    | TmLam t -> let _ = dprint t in error "Encountered an isolated lambda"
end

lang LetOCamlCode = LetAst
    sem ocamlcodegen (indent : Int) =
    | TmLet t ->
      -- Find all the chained lambdas
      --   acc.0: The variable names
      --   acc.1: The trailing expression
      recursive let chainlambdas = lam acc. lam expr.
        match expr with TmLam t1 then
          chainlambdas (concat acc [t1.0]) t1.2
        else
          (acc, expr)
      in
      let argres = chainlambdas [] t.2 in
      let name = t.0 in
      let args = argres.0 in
      let letexpr = argres.1 in
      let inexpr = t.3 in
      strJoin "" ["let ", name, " ", strJoin " " args, " =", newline (incr indent),
                  ocamlcodegen (incr indent) letexpr, newline indent,
                  "in", newline indent,
                  ocamlcodegen indent inexpr]
end

lang RecLetsOCamlCode = RecLetsAst
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
              chainlambdas (concat acc [t1.0]) t1.2
            else
              (acc, expr)
          in
          let argres = chainlambdas [] (head l).2 in
          let name = (head l).0 in
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
      let recexprs = t.0 in
      let inexpr = t.1 in
      let list = concat (generatelets [] recexprs) [
        "in", newline indent,
        ocamlcodegen indent inexpr
      ]
      in
      strJoin "" list
end

lang ConstOCamlCode = ConstAst
    sem ocamlconstgen (indent : Int) =
    -- intentionally left blank

    sem ocamlcodegen (indent : Int) =
    | TmConst c -> ocamlconstgen indent c
end

lang UnitOCamlCode = UnitAst
    sem ocamlconstgen (indent : Int) =
    | CUnit -> "()"
end

lang IntOCamlCode = IntAst
    sem ocamlconstgen (indent : Int) =
    | CInt i -> int2string i
end

lang ArithIntOCamlCode = ArithIntAst
    syn Const =
    | CModi ()
    | CDivi ()
    | CNegi ()

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
    | CBool b -> if b then "true" else "false"
    | CNot _ -> "not"
    | CAnd _ -> "( && )"
    | COr _ -> "( || )"

    sem ocamlcodegen (indent : Int) =
    | TmIf t ->
      let cond = t.0 in
      let thn = t.1 in
      let els = t.2 in
      strJoin "" [
        "if ", ocamlcodegen indent cond, " then", newline (incr indent),
        ocamlcodegen (incr indent) thn, newline indent,
        "else", newline (incr indent),
        ocamlcodegen (incr indent) els
      ]
end

lang CmpOCamlCode = CmpAst
    sem ocamlconstgen (indent : Int) =
    | CEqi _ -> "( = )"
    | CLti _ -> "( < )"
end

lang CharOCamlCode = CharAst
    syn Const =
    | CChar2int ()
    | CInt2char ()

    sem ocamlconstgen (indent : Int) =
    | CChar c -> if eqchar c (head "\n") then
                   [head "'", head "\\", 'n', head "'"]
                 else
                   [head "'", c, head "'"]
    | CChar2int _ -> "int_of_char"
    | CInt2char _ -> "char_of_int"
end

lang SeqOCamlCode = SeqAst
    syn Const =
    | CLength ()
    | CCons ()
    | CConcat ()
    | CSlice ()
    | CReverse ()

    sem ocamlconstgen (indent : Int) =
    | CNth _ -> "Array.get"
    | CLength _ -> "Array.length"
    | CCons _ -> "(fun x xs -> Array.append [|x|] xs)"
    | CConcat _ -> "Array.append"
    | CSlice _ -> "(fun xs start len -> Array.sub xs (min ((Array.length xs) - 1) start) (min ((Array.length xs) - start) len))"
    | CReverse _ -> "List.rev"
    | CSeq tms -> strJoin "" ["[|", strJoin "; " (map (ocamlcodegen indent) tms), "|]"]

    sem ocamlcodegen (indent : Int) =
    | TmSeq tms -> strJoin "" ["[|", strJoin "; " (map (ocamlcodegen indent) tms), "|]"]
end
lang TupleOCamlCode = TupleAst
    sem ocamlcodegen (indent : Int) =
    | TmTuple tms -> strJoin "" ["(", strJoin ", " (map (ocamlcodegen indent) tms), ")"]
    | TmProj t ->
      let tup = t.0 in
      let n = t.1 in
      recursive let mkproj = lam acc. lam idx.
        if eqi idx (length [0,1,2]) then -- TEMP: We do not know the length of the tuple here, so we cannot project out of it!
          acc
        else if eqi idx n then
          concat acc ["x"]
        else
          concat acc ["_"]
      in
      let projfun = strJoin "" ["(fun t -> let (", strJoin "," (mkproj [] 0), ") = t in x)"] in
      strJoin "" [projfun, " (", ocamlcodegen indent tup, ")"]
end

lang MExprOCamlCode = VarOCamlCode + AppOCamlCode + FunOCamlCode + LetOCamlCode +
                      RecLetsOCamlCode + ConstOCamlCode + UnitOCamlCode + IntOCamlCode +
                      ArithIntOCamlCode + BoolOCamlCode + CmpOCamlCode + CharOCamlCode +
                      SeqOCamlCode + TupleOCamlCode
    syn Expr =
    | TmMain Expr

    syn Const =
    | CPrint ()

    sem ocamlcodegen (indent : Int) =
    | TmMain t ->
      strJoin "" ["open Printf", "\n\n",
                  "let main =", newline (incr indent),
                  ocamlcodegen (incr indent) t]

    sem ocamlconstgen (indent : Int) =
    | CPrint _ -> "(fun s -> printf \"%s\" (String.of_seq (Array.to_seq s)))"
end

mexpr
use MExprOCamlCode in
recursive let bind_ = lam letexpr. lam inexpr.
  match letexpr with TmLet t then
    TmLet (t.0, t.1, t.2, bind_ t.3 inexpr)
  else match letexpr with TmRecLets t then
    TmRecLets (t.0, bind_ t.1 inexpr)
  else
    inexpr
in

-- macros:
let app_ = lam lhs. lam rhs. TmApp (lhs, rhs) in
let app1func_ = lam f. lam arg. app_ f arg in
let app2func_ = lam f. lam lhs. lam rhs. (app_ (app_ f lhs) rhs) in
let app3func_ = lam f. lam a. lam b. lam c. (app_ (app_ (app_ f a) b) c) in
let let_ = lam ident. lam body. TmLet (ident, None (), body, TmConst (CUnit ())) in
let reclets_ = lam bodies. TmRecLets (map (lam t. (t.0, None (), t.1)) bodies, TmConst (CUnit ())) in
let lam_ = lam ident. lam body. TmLam (ident, None (), body) in
let if_ = lam cond. lam thn. lam els. TmIf (cond, thn, els) in
let int_ = lam i. TmConst (CInt i) in
let char_ = lam c. TmConst (CChar c) in
let var_ = lam ident. TmVar (ident) in
let str_ = lam s. TmSeq (map char_ s) in
let seq_ = lam s. TmSeq s in
-- funcs:
let print_ = app1func_ (TmConst (CPrint ())) in
let int2char_ = app1func_ (TmConst (CInt2char ())) in
let char2int_ = app1func_ (TmConst (CChar2int ())) in
let length_ = app1func_ (TmConst (CLength ())) in
let negi_ = app1func_ (TmConst (CNegi ())) in
let lti_ = app2func_ (TmConst (CLti ())) in
let eqi_ = app2func_ (TmConst (CEqi ())) in
let addi_ = app2func_ (TmConst (CAddi ())) in
let subi_ = app2func_ (TmConst (CSubi ())) in
let muli_ = app2func_ (TmConst (CMuli ())) in
let divi_ = app2func_ (TmConst (CDivi ())) in
let modi_ = app2func_ (TmConst (CModi ())) in
let cons_ = app2func_ (TmConst (CCons ())) in
let nth_ = app2func_ (TmConst (CNth ())) in
let concat_ = app2func_ (TmConst (CConcat ())) in
let slice_ = app3func_ (TmConst (CSlice ())) in

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

let func_map =
  reclets_ [
    ("map",
     lam_ "f" (lam_ "seq"
                    (if_ (app_ (var_ "null")
                               (var_ "seq"))
                         (seq_ [])
                         (cons_ (app_ (var_ "f")
                                      (app_ (var_ "head")
                                            (var_ "seq")))
                                (app_ (app_ (var_ "map")
                                            (var_ "f"))
                                       (app_ (var_ "tail")
                                             (var_ "seq")))))))
  ]
in

let func_int2string =
  let_ "int2string"
       (lam_ "n"
             (bind_ (reclets_ [
                       ("int2string_rechelper",
                        lam_ "n"
                             (if_ (lti_ (var_ "n") (int_ 10))
                                  (seq_ [
                                     int2char_ (addi_ (var_ "n")
                                                      (char2int_ (char_ '0')))
                                   ])
                                   (bind_ (let_ "d" (seq_ [
                                                       int2char_ (addi_ (modi_ (var_ "n")
                                                                               (int_ 10))
                                                                        (char2int_ (char_ '0')))
                                                     ]))
                                          (concat_ (app_ (var_ "int2string_rechelper")
                                                         (divi_ (var_ "n") (int_ 10)))
                                                   (var_ "d"))))
                     )])
                     (if_ (lti_ (var_ "n") (int_ 0))
                          (cons_ (char_ '_')
                                 (app_ (var_ "int2string_rechelper")
                                       (negi_ (var_ "n"))))
                          (app_ (var_ "int2string_rechelper") (var_ "n")))))
in

let func_factorial =
  reclets_ [
    ("factorial",
     lam_ "n"
          (if_ (eqi_ (var_ "n") (int_ 0))
               (int_ 1)
               (muli_ (var_ "n")
                      (app_ (var_ "factorial")
                            (subi_ (var_ "n") (int_ 1))))
          )
     )
  ]
in

let prog = func_head in
let prog = bind_ prog func_tail in
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

let res = ocamlcodegen 0 (TmMain prog) in

let _ = print "\n\n" in
let _ = print res in
let _ = print "\n\n" in

()
