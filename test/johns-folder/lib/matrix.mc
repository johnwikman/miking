-- matrix.mc (Matrix functionality)

include "macros.mc"

let tymatrixi_ = tyseq_ tyint_

let func_matrixMki =
  let_ "matrixMki" (tyarrows_ [tyint_, tyint_, tyint_, tymatrixi_]) (
    lam_ "rows" (tyint_) (
      lam_ "cols" (tyint_) (
        lam_ "v" (tyint_) (
          makeseq_ (muli_ (var_ "rows") (var_ "cols"))
                   (var_ "v")
        )
      )
    )
  )

let func_matrixIniti =
  let_ "matrixIniti" (tyarrows_ [tyint_, tyint_, tyarrows_ [tyint_, tyint_, tyint_], tymatrixi_]) (
    lam_ "rows" (tyint_) (
      lam_ "cols" (tyint_) (
        lam_ "f" (tyarrows_ [tyint_, tyint_, tyint_]) (
          let seqInitFun =
            let_ "seqInitFun" (tyarrows_ [tyint_, tyint_]) (
              lam_ "i" (tyint_) (
                bindall_ [
                  let_ "row" (tyint_) (divi_ (var_ "i") (var_ "cols")),
                  let_ "col" (tyint_) (modi_ (var_ "i") (var_ "cols")),
                  app2f_ (var_ "f")
                         (var_ "row")
                         (var_ "col")
                ]
              )
            )
          in
          bind_ seqInitFun (
            app2f_ (var_ "seqInit")
                   (muli_ (var_ "rows") (var_ "cols"))
                   (var_ "seqInitFun")
          )
        )
      )
    )
  )

let func_matrixGeti =
  let_ "matrixGeti" (tyarrows_ [tyint_, tyint_, tyint_, tyint_, tymatrixi_, tyint_]) (
    lam_ "row" (tyint_) (
      lam_ "col" (tyint_) (
        lam_ "m_rows" (tyint_) (
          lam_ "m_cols" (tyint_) (
            lam_ "m" (tymatrixi_) (
              nth_ (var_ "m")
                   (addi_ (muli_ (var_ "m_cols") (var_ "row"))
                          (var_ "col"))
            )
          )
        )
      )
    )
  )

let func_matrix2stri =
  let_ "matrix2stri" (tyarrows_ [tyint_, tyint_, tymatrixi_, tystr_]) (
    lam_ "m_rows" (tyint_) (
      lam_ "m_cols" (tyint_) (
        lam_ "m" (tyseq_ tyint_) (
          let printrc =
            reclets_add "printrc" (tyarrows_ [tyint_, tyint_, tystr_]) (
              lam_ "row" (tyint_) (
                lam_ "col" (tyint_) (
                  if_ (eqi_ (var_ "row") (var_ "m_rows"))
                      (str_ "")
                      (bindall_ [
                         let_ "next_col" (tyint_) (modi_ (addi_ (var_ "col") (int_ 1)) (var_ "m_cols")),
                         let_ "next_row" (tyint_) (if_ (eqi_ (var_ "next_col") (int_ 0))
                                                       (addi_ (var_ "row") (int_ 1))
                                                       (var_ "row")),
                         app2f_ (var_ "strJoin")
                                (str_ "")
                                (seq_ [app_ (var_ "int2string")
                                            (app5f_ (var_ "matrixGeti")
                                                    (var_ "row")
                                                    (var_ "col")
                                                    (var_ "m_rows")
                                                    (var_ "m_cols")
                                                    (var_ "m")),
                                       if_ (eqi_ (var_ "next_col") (int_ 0))
                                           (str_ "\n")
                                           (str_ " "),
                                       app2f_ (var_ "printrc")
                                              (var_ "next_row")
                                              (var_ "next_col")])
                       ])
                )
              )
            ) (reclets_empty)
          in
          bind_ printrc (
            app2f_ (var_ "printrc")
                   (int_ 0)
                   (int_ 0)
          )
        )
      )
    )
  )

-- let matrixMuliWorker = lam innerDim. lam a_rows. lam b_cols. lam a. lam b. lam idx.
--   let row = divi idx b_cols in
--   let col = modi idx b_cols in
--   let a_start_offset = muli innerDim row in
--   let b_start_offset = col in
--   recursive let matrixMuliWorkerReduce = lam acc. lam p. lam a_offset. lam b_offset.
--     if (eqi p innerDim)
--        (acc)
--        (matrixMuliWorkerReduce (addi acc
--                                      (muli (nth a a_offset)
--                                            (nth b b_offset))
--                                (addi p 1)
--                                (addi a_offset 1)
--                                (addi b_offset b_cols)) -- go to next row in b
--   in
--   matrixMuliWorkerReduce 0 0 a_start_offset b_start_offset
let func_matrixMuliWorkerReduce =
  reclets_add "matrixMuliWorkerReduce" (tyarrows_ [tyint_, tyint_, tymatrixi_, tymatrixi_, tyint_, tyint_, tyint_, tyint_, tyint_]) (
    lam_ "innerDim" (tyint_) (
      lam_ "b_cols" (tyint_) (
        lam_ "a" (tymatrixi_) (
          lam_ "b" (tymatrixi_) (
            lam_ "acc" (tyint_) (
              lam_ "p" (tyint_) (
                lam_ "a_offset" (tyint_) (
                  lam_ "b_offset" (tyint_) (
                    if_ (eqi_ (var_ "p") (var_ "innerDim"))
                        (var_ "acc")
                        (app8f_ (var_ "matrixMuliWorkerReduce")
                                (var_ "innerDim")
                                (var_ "b_cols")
                                (var_ "a")
                                (var_ "b")
                                (addi_ (var_ "acc")
                                       (muli_ (nth_ (var_ "a") (var_ "a_offset"))
                                              (nth_ (var_ "b") (var_ "b_offset"))))
                                (addi_ (var_ "p") (int_ 1))
                                (addi_ (var_ "a_offset") (int_ 1))
                                (addi_ (var_ "b_offset") (var_ "b_cols")))
                  )
                )
              )
            )
          )
        )
      )
    )
  ) (reclets_empty)
let func_matrixMuliWorker =
  let_ "matrixMuliWorker" (tyarrows_ [tyint_, tyint_, tyint_, tymatrixi_, tymatrixi_, tyint_, tyint_]) (
    lam_ "innerDim" (tyint_) (
      lam_ "a_rows" (tyint_) (
        lam_ "b_cols" (tyint_) (
          lam_ "a" (tymatrixi_) (
            lam_ "b" (tymatrixi_) (
              lam_ "idx" (tyint_) (
                app8f_ (var_ "matrixMuliWorkerReduce")
                       (var_ "innerDim")
                       (var_ "b_cols")
                       (var_ "a")
                       (var_ "b")
                       (int_ 0)
                       (int_ 0)
                       (muli_ (var_ "innerDim")
                              (divi_ (var_ "idx") (var_ "b_cols")))
                       (modi_ (var_ "idx") (var_ "b_cols"))
              )
            )
          )
        )
      )
    )
  )

let func_matrixMuli =
  let_ "matrixMuli" (tyarrows_ [tyint_, tyint_, tymatrixi_, tyint_, tyint_, tymatrixi_, tymatrixi_]) (
    lam_ "a_rows" (tyint_) (
      lam_ "a_cols" (tyint_) (
        lam_ "a" (tymatrixi_) (
          lam_ "b_rows" (tyint_) (
            lam_ "b_cols" (tyint_) (
              lam_ "b" (tymatrixi_) (
                if_ (neqi_ (var_ "a_cols") (var_ "b_rows"))
                    (error_ (str_ "matrixMuli: Inner dimensions differ."))
                    (app2f_ (var_ "seqInit")
                            (muli_ (var_ "a_rows") (var_ "b_cols"))
                            (app5f_ (var_ "matrixMuliWorker")
                                    (var_ "a_cols")
                                    (var_ "a_rows")
                                    (var_ "b_cols")
                                    (var_ "a")
                                    (var_ "b")))
              )
            )
          )
        )
      )
    )
  )

let func_matrixMuliCUDA =
  let_ "matrixMuliCUDA" (tyarrows_ [tyint_, tyint_, tymatrixi_, tyint_, tyint_, tymatrixi_, tymatrixi_]) (
    lam_ "a_rows" (tyint_) (
      lam_ "a_cols" (tyint_) (
        lam_ "a" (tymatrixi_) (
          lam_ "b_rows" (tyint_) (
            lam_ "b_cols" (tyint_) (
              lam_ "b" (tymatrixi_) (
                bindall_ [
                  let_ "size" (tyint_) (muli_ (var_ "a_rows") (var_ "b_cols")),
                  if_ (neqi_ (var_ "a_cols") (var_ "b_rows"))
                      (error_ (str_ "matrixMuliCUDA: Inner dimensions differ."))
                      (cudamapidx_ 32
                                   (var_ "size")
                                   (app5f_ (var_ "matrixMuliWorker")
                                           (var_ "a_cols")
                                           (var_ "a_rows")
                                           (var_ "b_cols")
                                           (var_ "a")
                                           (var_ "b")))
                ]
              )
            )
          )
        )
      )
    )
  )

let libmatrix_ = bindall_ [
  func_matrixMki,
  func_matrixGeti,
  func_matrixIniti,
  func_matrix2stri,
  func_matrixMuliWorkerReduce,
  func_matrixMuliWorker,
  func_matrixMuli,
  func_matrixMuliCUDA
]
