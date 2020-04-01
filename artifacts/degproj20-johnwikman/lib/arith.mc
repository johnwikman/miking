-- arith.mc (Basic arithmetic functions)

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

let func_fib =
  let_ "fib" (tyarrow_ tyint_ tyint_) (
    lam_ "n" (tyint_) (
      bindall_ [
        reclets_add "helper" (tyarrows_ [tyint_, tyint_, tyint_, tyint_]) (
          lam_ "i" (tyint_) (
            lam_ "prev" (tyint_) (
              lam_ "current" (tyint_) (
                if_ (eqi_ (var_ "i") (var_ "n"))
                    (var_ "current")
                    (app3f_ (var_ "helper")
                            (addi_ (var_ "i") (int_ 1))
                            (var_ "current")
                            (addi_ (var_ "prev") (var_ "current")))
              )
            )
          )
        ) (reclets_empty),
        app3f_ (var_ "helper")
               (int_ 0)
               (int_ 1)
               (int_ 0)
      ]
    )
  )

let libarith_ = bindall_ [
	func_id,
	func_factorial,
	func_fib
]
