-- testlib.mc

include "macros.mc"

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
          cuda_mapintarray_ 32
                            (app2f_ (var_ "saxpy_int_single")
                                    (var_ "x")
                                    (var_ "y"))
                            (var_ "arr")
        )
      )
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

let testlib_ = bindall_ [
	func_factorial,
  func_saxpy_int_single,
  func_saxpy_int_mapfull,
	func_printintln
]

let testlibcuda_ = bindall_ [
	func_factorial,
  func_saxpy_int_single,
  func_saxpy_int_mapfull,
	func_printintln,
	func_mapcuda_saxpy_int
]
