-- benchmarking utilities

include "macros.mc"

type BMParams = {warmups : Int,
                 iters : Int,
                 printStatus : Bool}

let bmparams_ = {warmups = 4, iters = 11, printStatus = false}

-- Takes an ast as input and returns an output ast that should be bound just as
-- the input AST.
let benchmark_ = lam params : BMParams. lam ast.
  if lti params.warmups 0 then
    error "number of benchmark warmups cannot be negative"
  else -- continue

  if lti params.iters 1 then
    error "number of benchmark iterations must be positive"
  else -- continue

  let deadcode = let_ "_" (tyunit_) unit_ in

  let itercountprint =
    if params.printStatus then
      bindall_ [
        let_ "_" (tyunit_) (print_ (str_ "[< Iteration ")),
        let_ "_" (tyunit_) (print_ (app1f_ (var_ "int2string")
                                           (addi_ (var_ "i") (int_ 1)))),
        let_ "_" (tyunit_) (print_ (str_ "/")),
        let_ "_" (tyunit_) (print_ (app1f_ (var_ "int2string") (var_ "n"))),
        let_ "_" (tyunit_) (print_ (str_ " >]\n"))
      ]
    else
      deadcode
  in

  let warmupprint =
    if params.printStatus then
      let_ "_" (tyunit_) (print_ (str_ "Running warmups...\n"))
    else
      deadcode
  in

  let iterprint =
    if params.printStatus then
      let_ "_" (tyunit_) (print_ (str_ "Running iterations...\n"))
    else
      deadcode
  in

  let bm_runonce =
    let_ "bm_runonce" (tyarrow_ tyunit_ tyfloat_) (
      lam_ "_" (tyunit_) (
        bindall_ [
          let_ "bm_t_start" (tyfloat_) (wallTimeSecondsf_),
          ast,
          let_ "bm_t_end" (tyfloat_) (wallTimeSecondsf_),
          subf_ (var_ "bm_t_end") (var_ "bm_t_start")
        ]
      )
    )
  in

  let bm_runmultiple =
    let_ "bm_runmultiple" (tyarrows_ [tyint_, tyseq_ tyfloat_]) (
      lam_ "n" (tyint_) (
        bindall_ [
          reclets_add "bm_iter" (tyarrows_ [tyint_, tyseq_ tyfloat_, tyseq_ tyfloat_]) (
            lam_ "i" (tyint_) (
              lam_ "acc" (tyseq_ tyfloat_) (
                if_ (geqi_ (var_ "i") (var_ "n"))
                    (var_ "acc")
                    (bindall_ [
                  itercountprint,
                  let_ "res" (tyfloat_) (app1f_ (var_ "bm_runonce") unit_),
                  let_ "newacc" (tyseq_ tyfloat_) (concat_ (var_ "acc") (seq_ [var_ "res"])),
                  app2f_ (var_ "bm_iter")
                         (addi_ (var_ "i") (int_ 1))
                         (var_ "newacc")
                ])
              )
            )
          )  (reclets_empty),
          app2f_ (var_ "bm_iter")
                 (int_ 0)
                 (seq_ [])
        ]
      )
    )
  in

  let bm_sort =
    let_ "bm_sort" (tyarrows_ [tyseq_ tyfloat_, tyseq_ tyfloat_]) (
      lam_ "arr" (tyseq_ tyfloat_) (
        bindall_ [
          let_ "n" (tyint_) (length_ (var_ "arr")),
          reclets_add "quicksort_rec" (tyarrows_ [tyfloat_, tyseq_ tyfloat_, tyseq_ tyfloat_, tyseq_ tyfloat_, tyseq_ tyfloat_]) (
            lam_ "pivot" (tyfloat_) (
              lam_ "lt_pivot" (tyseq_ tyfloat_) (
                lam_ "geq_pivot" (tyseq_ tyfloat_) (
                  lam_ "remaining" (tyseq_ tyfloat_) (
                    if_ (eqi_ (length_ (var_ "remaining")) (int_ 0))
                        (bindall_ [--then
                      let_ "seq_lt" (tyseq_ tyfloat_) (app1f_ (var_ "quicksort") (var_ "lt_pivot")),
                      let_ "seq_pivot" (tyseq_ tyfloat_) (seq_ [var_ "pivot"]),
                      let_ "seq_geq" (tyseq_ tyfloat_) (app1f_ (var_ "quicksort") (var_ "geq_pivot")),
                      concat_ (concat_ (var_ "seq_lt")
                                       (var_ "seq_pivot"))
                              (var_ "seq_geq")
                    ])
                        (bindall_ [-- else
                      let_ "e" (tyfloat_) (app1f_ (var_ "head") (var_ "remaining")),
                      let_ "t" (tyseq_ tyfloat_) (app1f_ (var_ "tail") (var_ "remaining")),
                      if_ (ltf_ (var_ "e") (var_ "pivot"))
                          (app4f_ (var_ "quicksort_rec")
                                  (var_ "pivot")
                                  (cons_ (var_ "e") (var_ "lt_pivot"))
                                  (var_ "geq_pivot")
                                  (var_ "t"))
                          (app4f_ (var_ "quicksort_rec")
                                  (var_ "pivot")
                                  (var_ "lt_pivot")
                                  (cons_ (var_ "e") (var_ "geq_pivot"))
                                  (var_ "t"))
                    ])
                  )
                )
              )
            )
          ) (reclets_add "quicksort" (tyarrows_ [tyseq_ tyfloat_, tyseq_ tyfloat_]) (
            lam_ "arr" (tyseq_ tyfloat_) (
              if_ (leqi_ (length_ (var_ "arr")) (int_ 1))
                  (var_ "arr")
                  (app4f_ (var_ "quicksort_rec")
                          (app1f_ (var_ "head") (var_ "arr"))
                          (seq_ [])
                          (seq_ [])
                          (app1f_ (var_ "tail") (var_ "arr")))
            )
          ) (reclets_empty)),
          app1f_ (var_ "quicksort") (var_ "arr")
        ]
      )
    )
  in

  let bm_median =
    let_ "bm_median" (tyarrows_ [tyseq_ tyfloat_, tyfloat_]) (
      lam_ "arr" (tyseq_ tyfloat_) (
        bindall_ [
          let_ "n" (tyint_) (length_ (var_ "arr")),
          let_ "sorted" (tyseq_ tyfloat_) (app1f_ (var_ "bm_sort") (var_ "arr")),
          if_ (eqi_ (modi_ (var_ "n") (int_ 2)) (int_ 0))
              (divf_ (addf_ (nth_ (var_ "arr")
                                  (subi_ (divi_ (var_ "n") (int_ 2))
                                         (int_ 1)))
                            (nth_ (var_ "arr")
                                  (divi_ (var_ "n") (int_ 2))))
                     (float_ 2.0))
              (nth_ (var_ "arr")
                    (divi_ (var_ "n") (int_ 2)))
        ]
      )
    )
  in

  let bm_sum =
    let_ "bm_sum" (tyarrows_ [tyseq_ tyfloat_, tyfloat_]) (
      lam_ "arr" (tyseq_ tyfloat_) (
        bindall_ [
          let_ "n" (tyint_) (length_ (var_ "arr")),
          reclets_add "work" (tyarrows_ [tyint_, tyfloat_, tyfloat_]) (
            lam_ "i" (tyint_) (
              lam_ "acc" (tyfloat_) (
                if_ (eqi_ (var_ "i") (var_ "n"))
                    (var_ "acc")
                    (app2f_ (var_ "work")
                            (addi_ (var_ "i") (int_ 1))
                            (addf_ (var_ "acc")
                                   (nth_ (var_ "arr") (var_ "i"))))
              )
            )
          ) (reclets_empty),
          app2f_ (var_ "work")
                 (int_ 0)
                 (float_ 0.0)
        ]
      )
    )
  in

  let bm_minmax = lam fname. lam op.
    let_ fname (tyarrows_ [tyseq_ tyfloat_, tyfloat_]) (
      lam_ "arr" (tyseq_ tyfloat_) (
        bindall_ [
          let_ "n" (tyint_) (length_ (var_ "arr")),
          reclets_add "work" (tyarrows_ [tyint_, tyfloat_, tyfloat_]) (
            lam_ "i" (tyint_) (
              lam_ "acc" (tyfloat_) (
                if_ (eqi_ (var_ "i") (var_ "n"))
                    (var_ "acc")
                    (bindall_ [
                  let_ "e" (tyfloat_) (nth_ (var_ "arr") (var_ "i")),
                  app2f_ (var_ "work")
                         (addi_ (var_ "i") (int_ 1))
                         (if_ (op (var_ "e") (var_ "acc"))
                              (var_ "e")
                              (var_ "acc"))
                ])
              )
            )
          ) (reclets_empty),
          app2f_ (var_ "work")
                 (int_ 1)
                 (nth_ (var_ "arr") (int_ 0))
        ]
      )
    )
  in

  let bm_max = bm_minmax "bm_max" gti_ in
  let bm_min = bm_minmax "bm_min" lti_ in

  let bm_dist =
    let_ "bm_dist" (tyarrows_ [tyfloat_, tyfloat_, tyfloat_]) (
      lam_ "a" (tyfloat_) (
        lam_ "b" (tyfloat_) (
          if_ (gtf_ (var_ "a") (var_ "b"))
              (subf_ (var_ "a") (var_ "b"))
              (subf_ (var_ "b") (var_ "a"))
        )
      )
    )
  in

  let bmres_warmup =
    let_ "bmres_warmup" (tyseq_ tyfloat_) (
      app1f_ (var_ "bm_runmultiple") (int_ params.warmups)
    )
  in

  let bmres_iters =
    let_ "bmres_iters" (tyseq_ tyfloat_) (
      app1f_ (var_ "bm_runmultiple") (int_ params.iters)
    )
  in

  let bmpresent_iters =
    let_ "_" (tyunit_) (
      bindall_ [
        let_ "median" (tyfloat_) (app1f_ (var_ "bm_median") (var_ "bmres_iters")),
        let_ "sum" (tyfloat_) (app1f_ (var_ "bm_sum") (var_ "bmres_iters")),
        let_ "avg" (tyfloat_) (divf_ (var_ "sum") (float_ (int2float params.iters))),
        let_ "max" (tyfloat_) (app1f_ (var_ "bm_max") (var_ "bmres_iters")),
        let_ "min" (tyfloat_) (app1f_ (var_ "bm_min") (var_ "bmres_iters")),
        let_ "variance" (tyfloat_) (app1f_ (var_ "bm_max")
                                           (seq_ [app2f_ (var_ "bm_dist")
                                                         (var_ "avg")
                                                         (var_ "max"),
                                                  app2f_ (var_ "bm_dist")
                                                         (var_ "avg")
                                                         (var_ "min")])),

        let_ "median" (tyfloat_) (mulf_ (var_ "median") (float_ 1000.0)),
        let_ "sum" (tyfloat_) (mulf_ (var_ "sum") (float_ 1000.0)),
        let_ "avg" (tyfloat_) (mulf_ (var_ "avg") (float_ 1000.0)),
        let_ "max" (tyfloat_) (mulf_ (var_ "max") (float_ 1000.0)),
        let_ "min" (tyfloat_) (mulf_ (var_ "min") (float_ 1000.0)),
        let_ "variance" (tyfloat_) (mulf_ (var_ "variance") (float_ 1000.0)),

        let_ "_" (tyunit_) (print_ (str_ "== ITERATION RESULTS ==\n")),
        let_ "_" (tyunit_) (print_ (str_ "No. of iterations: ")),
        let_ "_" (tyunit_) (print_ (str_ (int2string params.iters))),
        let_ "_" (tyunit_) (print_ (str_ "\n")),

        let_ "_" (tyunit_) (print_ (str_ "Median: ")),
        let_ "_" (tyunit_) (print_ (app1f_ (var_ "float2string")
                                           (var_ "median"))),
        let_ "_" (tyunit_) (print_ (str_ " ms\n")),

        let_ "_" (tyunit_) (print_ (str_ "Longest run: ")),
        let_ "_" (tyunit_) (print_ (app1f_ (var_ "float2string")
                                           (var_ "max"))),
        let_ "_" (tyunit_) (print_ (str_ " ms\n")),

        let_ "_" (tyunit_) (print_ (str_ "Shortest run: ")),
        let_ "_" (tyunit_) (print_ (app1f_ (var_ "float2string")
                                           (var_ "min"))),
        let_ "_" (tyunit_) (print_ (str_ " ms\n")),

        let_ "_" (tyunit_) (print_ (str_ "Average: ")),
        let_ "_" (tyunit_) (print_ (app1f_ (var_ "float2string")
                                           (var_ "avg"))),
        let_ "_" (tyunit_) (print_ (str_ " ms\n")),

        let_ "_" (tyunit_) (print_ (str_ "Variance: +-")),
        let_ "_" (tyunit_) (print_ (app1f_ (var_ "float2string")
                                           (var_ "variance"))),
        let_ "_" (tyunit_) (print_ (str_ " ms\n")),


        -- WARMUP INFO
        let_ "median" (tyfloat_) (app1f_ (var_ "bm_median") (var_ "bmres_warmup")),
        let_ "sum" (tyfloat_) (app1f_ (var_ "bm_sum") (var_ "bmres_warmup")),
        let_ "avg" (tyfloat_) (divf_ (var_ "sum") (float_ (int2float params.warmups))),
        let_ "max" (tyfloat_) (app1f_ (var_ "bm_max") (var_ "bmres_warmup")),
        let_ "min" (tyfloat_) (app1f_ (var_ "bm_min") (var_ "bmres_warmup")),
        let_ "variance" (tyfloat_) (app1f_ (var_ "bm_max")
                                           (seq_ [app2f_ (var_ "bm_dist")
                                                         (var_ "avg")
                                                         (var_ "max"),
                                                  app2f_ (var_ "bm_dist")
                                                         (var_ "avg")
                                                         (var_ "min")])),

        let_ "median" (tyfloat_) (mulf_ (var_ "median") (float_ 1000.0)),
        let_ "sum" (tyfloat_) (mulf_ (var_ "sum") (float_ 1000.0)),
        let_ "avg" (tyfloat_) (mulf_ (var_ "avg") (float_ 1000.0)),
        let_ "max" (tyfloat_) (mulf_ (var_ "max") (float_ 1000.0)),
        let_ "min" (tyfloat_) (mulf_ (var_ "min") (float_ 1000.0)),
        let_ "variance" (tyfloat_) (mulf_ (var_ "variance") (float_ 1000.0)),

        let_ "_" (tyunit_) (print_ (str_ "\n\n== WARMUP STATISTICS ==\n")),
        let_ "_" (tyunit_) (print_ (str_ "No. of warmup runs: ")),
        let_ "_" (tyunit_) (print_ (str_ (int2string params.warmups))),
        let_ "_" (tyunit_) (print_ (str_ "\n")),

        let_ "_" (tyunit_) (print_ (str_ "Median: ")),
        let_ "_" (tyunit_) (print_ (app1f_ (var_ "float2string")
                                           (var_ "median"))),
        let_ "_" (tyunit_) (print_ (str_ " ms\n")),

        let_ "_" (tyunit_) (print_ (str_ "Longest run: ")),
        let_ "_" (tyunit_) (print_ (app1f_ (var_ "float2string")
                                           (var_ "max"))),
        let_ "_" (tyunit_) (print_ (str_ " ms\n")),

        let_ "_" (tyunit_) (print_ (str_ "Shortest run: ")),
        let_ "_" (tyunit_) (print_ (app1f_ (var_ "float2string")
                                           (var_ "min"))),
        let_ "_" (tyunit_) (print_ (str_ " ms\n")),

        let_ "_" (tyunit_) (print_ (str_ "Average: ")),
        let_ "_" (tyunit_) (print_ (app1f_ (var_ "float2string")
                                           (var_ "avg"))),
        let_ "_" (tyunit_) (print_ (str_ " ms\n")),

        let_ "_" (tyunit_) (print_ (str_ "Variance: +-")),
        let_ "_" (tyunit_) (print_ (app1f_ (var_ "float2string")
                                           (var_ "variance"))),
        let_ "_" (tyunit_) (print_ (str_ " ms\n"))
      ]
    )
  in

  bindall_ [
    bm_runonce,
    bm_runmultiple,
    bm_sort,
    bm_median,
    bm_sum,
    bm_max,
    bm_min,
    bm_dist,
    warmupprint,
    bmres_warmup,
    iterprint,
    bmres_iters,
    bmpresent_iters
  ]
