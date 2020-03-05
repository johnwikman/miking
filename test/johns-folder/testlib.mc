-- testlib.mc

include "macros.mc"

let func_id =
  let_ "id" (tyarrows_ [tyint_, tyint_]) (
    lam_ "x" (tyint_) (
      (var_ "x")
    )
  )

let func_factorial =
  reclets_add "factorial" (tyarrow_ tyint_ tyint_) (
    lam_ "n" (tyint_) (
      if_ (eqi_ (var_ "n") (int_ 0))
          (int_ 1)
          (muli_ (var_ "n")
                 (app_ (var_ "factorial")
                       (subi_ (var_ "n") (int_ 1))))
    )
  ) (reclets_empty)

let func_saxpy_int_single =
  let_ "saxpy_int_single" (tyarrows_ [tyint_, tyint_, tyint_, tyint_]) (
    lam_ "x" (tyint_) (
      lam_ "y" (tyint_) (
        lam_ "a" (tyint_) (
          addi_ (muli_ (var_ "a")
                       (var_ "x"))
                (var_ "y")
        )
      )
    )
  )

let func_saxpy_float_single =
  let_ "saxpy_float_single" (tyarrows_ [tyfloat_, tyfloat_, tyfloat_, tyfloat_]) (
    lam_ "x" (tyfloat_) (
      lam_ "y" (tyfloat_) (
        lam_ "a" (tyfloat_) (
          addf_ (mulf_ (var_ "a")
                       (var_ "x"))
                (var_ "y")
        )
      )
    )
  )

let func_saxpy_int_mapfull =
  let_ "saxpy_int_mapfull" (tyarrows_ [tyint_, tyseq_ tyint_, tyint_, tyint_, tyint_]) (
    lam_ "a" (tyint_) (
      lam_ "y" (tyseq_ tyint_) (
        lam_ "i" (tyint_) (
          lam_ "x" (tyint_) (
            addi_ (muli_ (var_ "a")
                         (var_ "x"))
                  (nth_ (var_ "y") (var_ "i"))
          )
        )
      )
    )
  )

let func_mapcuda_saxpy_int =
  let_ "mapcuda_saxpy_int" (tyarrows_ [tyint_, tyint_, tyseq_ tyint_, tyseq_ tyint_]) (
    lam_ "x" (tyint_) (
      lam_ "y" (tyint_) (
        lam_ "arr" (tyseq_ tyint_) (
          cudamap_ 32
                   (app2f_ (var_ "saxpy_int_single")
                           (var_ "x")
                           (var_ "y"))
                   (var_ "arr")
        )
      )
    )
  )

let func_id_ignore2nd =
  let_ "id_ignore2nd" (tyarrows_ [tyint_, tyint_, tyint_]) (
    lam_ "x" (tyint_) (
      lam_ "y" (tyint_) (
        (var_ "x")
      )
    )
  )

let func_mapcuda_id_ignore2nd =
  let_ "mapcuda_id_ignore2nd" (tyarrows_ [tyseq_ tyint_, tyseq_ tyint_]) (
    lam_ "arr" (tyseq_ tyint_) (
      cudamapi_ 16
                (var_ "id_ignore2nd")
                (var_ "arr")
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

let testlib_all = [
  func_id,
  func_factorial,
  func_saxpy_int_single,
  func_saxpy_float_single,
  func_saxpy_int_mapfull,
  func_id_ignore2nd,
  func_printintln,
  func_printintarr
]

let testlibcuda_all = concat testlib_all [
  func_mapcuda_saxpy_int,
  func_mapcuda_id_ignore2nd
]

let testlib_ = bindall_ testlib_all

let testlibcuda_ = bindall_ testlibcuda_all
