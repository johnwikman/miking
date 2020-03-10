-- io.mc (Input/Output library)

include "macros.mc"

let func_printint =
  let_ "printint" (tyarrow_ tyint_ tyunit_) (
    lam_ "i" (tyint_) (
      print_ (app_ (var_ "int2string") (var_ "i"))
    )
  )

let func_printintln =
  let_ "printintln" (tyarrow_ tyint_ tyunit_) (
    lam_ "i" (tyint_) (
      print_ (concat_ (app_ (var_ "int2string")
                            (var_ "i"))
                      (str_ "\n"))
    )
  )

-- let printintarr : String -> [Int] -> () = lam name : String. lam arr : [Int].
--   recursive let printloop = lam i : Int.
--     if eqi i (length arr) then
--       ()
--     else
--       let _ = print "    " in
--       let _ = print (int2string i) in
--       let _ = print ": "
--       let _ = print (int2string (nth arr i)) in
--       let _ = print "\n" in
--       printloop (addi i 1)
--   in
--   let _ = print "Contents of " in
--   let _ = print name in
--   let _ = print ":\n" in
--   printloop 0
-- in
let func_printintarr =
  let_ "printintarr" (tyarrows_ [tystr_, tyseq_ tyint_, tyunit_]) (
    lam_ "name" (tystr_) (
      lam_ "arr" (tyseq_ tyint_) (
        let printloop =
          reclets_add "printloop" (tyarrow_ tyint_ tyunit_) (
            lam_ "i" (tyint_) (
              if_ (eqi_ (var_ "i") (length_ (var_ "arr")))
                  (unit_)
                  (bind_ (bindall_ [
                    let_ "_" (tyunit_) (print_ (str_ "    ")),
                    let_ "_" (tyunit_) (print_ (app1f_ (var_ "int2string") (var_ "i"))),
                    let_ "_" (tyunit_) (print_ (str_ ": ")),
                    let_ "_" (tyunit_) (print_ (app1f_ (var_ "int2string") (nth_ (var_ "arr") (var_ "i")))),
                    let_ "_" (tyunit_) (print_ (str_ "\n"))
                  ]) (app1f_ (var_ "printloop") (addi_ (var_ "i") (int_ 1))))
            )
          ) (reclets_empty)
        in
        bind_ (bindall_ [
          printloop,
          let_ "_" (tyunit_) (print_ (str_ "Contents of ")),
          let_ "_" (tyunit_) (print_ (var_ "name")),
          let_ "_" (tyunit_) (print_ (str_ ":\n"))
        ]) (app1f_ (var_ "printloop") (int_ 0))
      )
    )
  )

let func_printfloatarr =
  let_ "printfloatarr" (tyarrows_ [tystr_, tyseq_ tyfloat_, tyunit_]) (
    lam_ "name" (tystr_) (
      lam_ "arr" (tyseq_ tyfloat_) (
        let printloop =
          reclets_add "printloop" (tyarrow_ tyint_ tyunit_) (
            lam_ "i" (tyint_) (
              if_ (eqi_ (var_ "i") (length_ (var_ "arr")))
                  (unit_)
                  (bind_ (bindall_ [
                    let_ "_" (tyunit_) (print_ (str_ "    ")),
                    let_ "_" (tyunit_) (print_ (app1f_ (var_ "int2string") (var_ "i"))),
                    let_ "_" (tyunit_) (print_ (str_ ": ")),
                    let_ "_" (tyunit_) (print_ (app1f_ (var_ "float2string") (nth_ (var_ "arr") (var_ "i")))),
                    let_ "_" (tyunit_) (print_ (str_ "\n"))
                  ]) (app1f_ (var_ "printloop") (addi_ (var_ "i") (int_ 1))))
            )
          ) (reclets_empty)
        in
        bind_ (bindall_ [
          printloop,
          let_ "_" (tyunit_) (print_ (str_ "Contents of ")),
          let_ "_" (tyunit_) (print_ (var_ "name")),
          let_ "_" (tyunit_) (print_ (str_ ":\n"))
        ]) (app1f_ (var_ "printloop") (int_ 0))
      )
    )
  )

let libio_ = bindall_ [
	func_printint,
	func_printintln,
	func_printintarr,
	func_printfloatarr
]
