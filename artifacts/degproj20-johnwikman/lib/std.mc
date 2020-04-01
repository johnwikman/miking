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
  let_ "map" (tyarrows_ [tyarrows_ [tydyn_, tydyn_], tyseq_ tydyn_, tyseq_ tydyn_]) (
    lam_ "f" (tyarrows_ [tydyn_, tydyn_]) (
      lam_ "seq" (tyseq_ tydyn_) (
        app2f_ (var_ "Array.map")
               (var_ "f")
               (var_ "seq")
      )
    )
  )
--  reclets_add "map" (tyarrows_ [tyarrow_ tydyn_ tydyn_, tyseq_ tydyn_, tyseq_ tydyn_]) (
--    lam_ "f" (tyarrow_ tydyn_ tydyn_) (
--      lam_ "seq" (tyseq_ tydyn_) (
--        if_ (app1f_ (var_ "null") (var_ "seq"))
--            (seq_ [])
--            (cons_ (app1f_ (var_ "f")
--                           (app1f_ (var_ "head") (var_ "seq")))
--                   (app2f_ (var_ "map")
--                           (var_ "f")
--                           (app1f_ (var_ "tail") (var_ "seq"))))
--    ))
--  ) (reclets_empty)

let func_mapi =
  let_ "mapi" (tyarrows_ [tyarrows_ [tyint_, tydyn_, tydyn_], tyseq_ tydyn_, tyseq_ tydyn_]) (
    lam_ "f" (tyarrows_ [tyint_, tydyn_, tydyn_]) (
      lam_ "seq" (tyseq_ tydyn_) (
        app2f_ (var_ "Array.mapi")
               (var_ "f")
               (var_ "seq")
      )
    )
  )
--  let_ "mapi"  (tyarrows_ [tyarrows_ [tyint_, tydyn_, tydyn_], tyseq_ tydyn_, tyseq_ tydyn_]) (
--    lam_ "f" (tyarrows_ [tyint_, tydyn_, tydyn_]) (
--      lam_ "seq" (tyseq_ tydyn_) (
--        let mapi_helper =
--          reclets_add "mapi_helper" (tyarrows_ [tyint_, tyseq_ tydyn_, tyseq_ tydyn_]) (
--            lam_ "i" (tyint_) (
--              lam_ "partseq" (tyseq_ tydyn_) (
--                if_ (app1f_ (var_ "null") (var_ "partseq"))
--                    (seq_ [])
--                    (cons_ (app2f_ (var_ "f")
--                                   (var_ "i")
--                                   (app1f_ (var_ "head") (var_ "partseq")))
--                           (app2f_ (var_ "mapi_helper")
--                                   (addi_ (var_ "i") (int_ 1))
--                                   (app1f_ (var_ "tail") (var_ "partseq"))))
--            ))
--          ) (reclets_empty)
--        in
--        bind_ mapi_helper (app2f_ (var_ "mapi_helper")
--                                  (int_ 0)
--                                  (var_ "seq"))
--      )
--    )
--  )

let func_seqInit =
  let_ "seqInit" (tyarrows_ [tyint_, tyarrows_ [tyint_, tydyn_], tyseq_ tydyn_]) (
    lam_ "size" (tyint_) (
      lam_ "f" (tyarrows_ [tyint_, tydyn_]) (
        app2f_ (var_ "Array.init")
               (var_ "size")
               (var_ "f")
      )
    )
  )
--  let_ "seqInit" (tyarrows_ [tyint_, tyarrows_ [tyint_, tydyn_], tyseq_ tydyn_]) (
--    lam_ "size" (tyint_) (
--      lam_ "f" (tyarrows_ [tyint_, tydyn_]) (
--        let seqInit_helper =
--          reclets_add "seqInit_helper" (tyarrows_ [tyint_, tyseq_ tydyn_, tyseq_ tydyn_]) (
--            lam_ "i" (tyint_) (
--              if_ (eqi_ (var_ "i") (var_ "size"))
--                  (seq_ [])
--                  (cons_ (app1f_ (var_ "f")
--                                 (var_ "i"))
--                         (app1f_ (var_ "seqInit_helper")
--                                 (addi_ (var_ "i") (int_ 1))))
--            )
--          ) (reclets_empty)
--        in
--        bind_ seqInit_helper (app1f_ (var_ "seqInit_helper")
--                                     (int_ 0))
--      )
--    )
--  )

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
            (cons_ (char_ '-')
                   (app_ (var_ "int2string_rechelper")
                         (negi_ (var_ "n"))))
            (app_ (var_ "int2string_rechelper") (var_ "n"))
      )
    )
  )

let func_float2string =
  let_ "float2string" (tyarrow_ tyfloat_ tystr_) (
    lam_ "f" (tyfloat_) (
      app1f_ (var_ "Array.of_seq")
             (app1f_ (var_ "String.to_seq")
                     (app1f_ (var_ "string_of_float") (var_ "f")))
    )
  )

--recursive
--  let strJoin = lam delim. lam strs.
--    if eqi (length strs) 0
--    then ""
--    else if eqi (length strs) 1
--         then head strs
--         else concat (concat (head strs) delim) (strJoin delim (tail strs))
--endmatBinitfun_v2
let func_strJoin =
  reclets_add "strJoin" (tyarrows_ [tystr_, tyseq_ tystr_, tystr_]) (
    lam_ "delim" (tystr_) (
      lam_ "strs" (tyseq_ tystr_) (
        if_ (eqi_ (length_ (var_ "strs")) (int_ 0))
            (str_ "")
            (if_ (eqi_ (length_ (var_ "strs")) (int_ 1))
                 (app1f_ (var_ "head") (var_ "strs"))
                 (concat_ (concat_ (app1f_ (var_ "head")
                                           (var_ "strs"))
                                   (var_ "delim"))
                          (app2f_ (var_ "strJoin")
                                  (var_ "delim")
                                  (app1f_ (var_ "tail")
                                          (var_ "strs")))))
      )
    )
  ) (reclets_empty)

let libstd_ = bindall_ [
	func_head,
	func_tail,
	func_null,
	func_map,
  func_mapi,
  func_seqInit,
	func_int2string,
  func_float2string,
  func_strJoin
]
