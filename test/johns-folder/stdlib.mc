-- stdlib functions

include "macros.mc"

let func_head =
  let_ "head"
  	   (tyarrow_ (tyseq_ tydyn_) tydyn_)
       (lam_ "s"
       	     (tyseq_ tydyn_)
             (nth_ (var_ "s")
             	   (int_ 0)))

let func_tail =
  let_ "tail"
  	   (tyarrow_ (tyseq_ tydyn_) (tyseq_ tydyn_))
       (lam_ "s"
       	     (tyseq_ tydyn_)
             (slice_ (var_ "s")
                     (int_ 1)
                     (length_ (var_ "s"))))

let func_null =
  let_ "null"
       (tyarrow_ (tyseq_ tydyn_) tydyn_)
       (lam_ "l"
       		 (tyseq_ tydyn_)
       	     (eqi_ (length_ (var_ "l")) (int_ 0)))

let func_map =
  reclets_add "map" (tyarrows_ [tyarrow_ tydyn_ tydyn_, tyseq_ tydyn_, tyseq_ tydyn_]) (
    lam_ "f" (tyarrow_ tydyn_ tydyn_) (
      lam_ "seq" (tyseq_ tydyn_) (
        if_ (app1f_ (var_ "null") (var_ "seq"))
            (seq_ [])
            (cons_ (app1f_ (var_ "f")
                           (app1f_ (var_ "head") (var_ "seq")))
                   (app2f_ (var_ "map")
                           (var_ "f")
                           (app1f_ (var_ "tail") (var_ "seq"))))
    ))
  ) (reclets_empty)

let func_int2string =
  let_ "int2string" (tyarrow_ tyint_ tystr_) (
    lam_ "n" (tyint_) (
      let recs =
        reclets_add "int2string_rechelper" (tyarrow_ tyint_ tystr_) (
          lam_ "n" (tyint_) (
            if_ (lti_ (var_ "n") (int_ 10))
                (seq_ [
                  int2char_ (addi_ (var_ "n")
                                   (char2int_ (char_ '0')))
                 ])
                (let d =
                  let_ "d" (tyseq_ tychar_) (
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

let stdlib_ = bindall_ [
	func_head,
	func_tail,
	func_null,
	func_map,
	func_int2string
]
