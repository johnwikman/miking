-- Simulated Monte Carlo: Airplane model.

include "../../codegen/ocaml.mc"
include "../../lib/std.mc"
include "../../lib/io.mc"
include "../../lib/macros.mc"
include "../../lib/benchmark-utils.mc"

-- This should define the size (precision) variable
-- Included by binding defsize_
include "_size_.mc"

-- This should provide the specificmap_ and specificinit_ methods
-- that determine how map and init should be implemented.
include "_specific_.mc"

let var_mu = let_ "mu" (tyfloat_) (float_ 0.0)
let var_sigma = let_ "sigma" (tyfloat_) (float_ 1.0)

let var_nPoints = let_ "nPoints" (tyint_) (var_ "precision")

let var_xLowerBound = let_ "xLowerBound" (tyfloat_) (float_ 0.0)
let var_xUpperBound = let_ "xUpperBound" (tyfloat_) (float_ 200.0)

let var_flightrange = let_ "flight_range" (tyint_) (int_ 100)
let var_velocity = let_ "velocity" (tyfloat_) (float_ 2.0)
let var_altitude = let_ "altitude" (tyfloat_) (float_ 70.0)
let var_heightMapSize = let_ "heightMapSize" (tyint_) (int_ 201)

let var_heightMap =
  let_ "heightMap" (tyseq_ tyfloat_) (
    floatseq_ [30.0, 32.24347788457187, 33.48510607774699, 35.461339048456786, 38.611529674483926, 41.07755980873785, 40.24140193692131, 40.84070712675728, 39.932547423151654, 38.851798453693, 38.303472680968234, 37.74197592655872, 38.91683393532277, 39.892991648557576, 40.671653263769194, 42.05351647328936, 43.376549047287014, 45.66911523306811, 45.876075454544065, 46.113992035733155, 44.71587518554206, 41.28318960973631, 40.93681799862597, 40.92580202279705, 39.64706408439017, 41.85049681102658, 40.81629817325446, 41.665841117107526, 42.469264622776564, 44.936938892659704, 47.056585502717105, 47.41800041035182, 47.50825961328893, 45.90868852328969, 42.9664945631862, 41.64295640839189, 41.04831813072419, 41.936128192982295, 43.28838525142966, 43.506598303741306, 44.117240803893594, 44.0655176331053, 43.482155334462384, 42.24752414693157, 44.18944823287295, 43.19262153866078, 41.90832683287442, 39.657449444845106, 39.52936143867039, 37.32410606124732, 36.97901403093557, 36.44260582376406, 37.592066772102925, 38.06061927763797, 37.836363021920064, 38.50742082523651, 39.2066584840748, 39.049420066271175, 38.68825807979798, 37.11290457777407, 35.76213178769407, 34.29654410046083, 33.532953293597224, 32.80378146724451, 31.628925546154527, 32.690222111954114, 33.28835303585312, 32.52929468905625, 33.52115963070205, 32.67134874442373, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 51.93766962916357, 51.62370924493974, 52.136407282207585, 50.95951727178972, 50.13582785108875, 48.50096029267523, 48.27946590693244, 48.27906685475883, 47.95598328539765, 48.72751356495592, 48.4083427377833, 47.89986751207039, 49.2903138665893, 50.143462953170605, 51.032317752824746, 53.174488850400586, 53.442196380503695, 51.58250129686619, 51.7143713244938, 49.61624241397873, 48.76687617375459, 48.80023526632021, 51.07968562486776, 51.226942485518684, 51.4000241054404, 52.1638828211723, 52.01438299079773, 53.18082473307091, 51.65678055599997, 49.69001501627759, 48.25830631760978, 46.83513205194105, 48.13477611148358, 48.903781293947034, 49.30456131776531, 48.80930965808831, 50.622037095590045, 50.5912774182791, 49.77969664157478, 51.285140842734755, 51.42600783471102, 51.72041046240234, 51.1156796683413, 50.97814441689124, 49.50530098696027, 48.0229980035732, 48.73999082458849, 48.44677116841641, 48.61729079647866, 49.16113211007241, 50.07260793269803, 50.28835196906151, 49.01938818739465, 47.992057421126724, 44.98918819928649, 44.63004035318859, 42.07970055631189, 40.29530477214739, 40.10221786594071, 39.61220958285464, 41.8384858929975, 41.49531140208828, 42.7459500168718, 43.483343686794576, 42.6948579280809, 43.10773236472075, 42.36280416150809, 42.23861707228795, 42.06291439206533, 43.36835499497256, 44.1985173032992]
  )

let var_obs =
  let_ "obs" (tyseq_ tyfloat_) (
    floatseq_ [24.914942976515498, 30.0799274458579, 30.07669380680148, 28.568224568599188, 29.4118912997254, 28.470844052464077, 23.628558781110073, 21.56412329057229, 25.261456425974064, 31.08174522152798, 26.10672755443794, 25.788962552989748, 24.904647091273286, 26.085218731986995, 27.88000880663538, 29.038048556098843, 32.28490481997697, 31.41710951149836, 33.230429218432064, 32.20916740186942, 30.401361596350434, 32.51317264900569, 34.733757171752615, 36.66994653773609, 37.502275304678996, 38.375933959956384, 36.907071066091746, 37.575779247867736, 49.27440262153642, 48.417061602664916, 48.81114074221597, 49.40368596154949, 48.06372019415349, 50.5238910238342, 49.68911382830153, 50.39904634564013, 51.955912308250696, 49.34759141761298, 50.493741777349186, 47.9693155322185, 49.88945934276753, 49.30795015221561, 50.28634368889228, 48.95474663385305, 50.68981816453479, 49.37191244035842, 52.303916697683945, 49.864050299321676, 50.09772496771485, 49.600550970737125, 48.69347314826469, 49.88183595487143, 50.66638308203191, 48.665741528597245, 50.693773152690135, 49.86629844033156, 17.192676234372676, 17.58280136399164, 20.12084671422916, 21.635851909122337, 21.256113356614044, 21.565820579689813, 20.47736716237583, 20.86848129525831, 15.907268732238393, 21.073009607571397, 21.82243951255304, 18.55395077465024, 16.981532782364773, 17.250300022212905, 21.831438141505718, 22.28202708377311, 21.880602833196477, 20.583818821624057, 20.817074910535624, 19.354635121498955, 20.92678447051283, 17.95724426382097, 16.573476698082352, 18.507609859492874, 22.4109893190528, 22.42153635968495, 20.173672053824365, 20.629464650563406, 23.177605437559567, 25.417193364593547, 27.123437825372843, 29.775059197121557, 29.691070037710745, 27.965349791469702, 24.883829683152346, 24.859253356158185, 27.710132665548404, 26.72862292823993, 29.435468548995576, 26.93998743708852, 99999.64656825125, 99999.70816263725, 100000.8579239243, 100001.46657871557]
  )


let func_g_map =
  let_ "g_map" (tyarrows_ [tyfloat_, tyseq_ tyfloat_, tyint_, tyfloat_, tyfloat_]) (
    lam_ "altitude" (tyfloat_) (
      lam_ "hgtmap" (tyseq_ tyfloat_) (
        lam_ "mapsize" (tyint_) (
          lam_ "x" (tyfloat_) (
            bindall_ [
              let_ "p" (tyint_) (floorfi_ (var_ "x")),
              let_ "n" (tyint_) (ceilfi_ (var_ "x")),
              if_ (or_ (lti_ (var_ "p") (int_ 0))
                       (gti_ (var_ "n") (subi_ (var_ "mapsize") (int_ 1))))
                  (float_ 100000.0)
                  (subf_ (var_ "altitude")
                          (addf_ (nth_ (var_ "hgtmap") (var_ "p"))
                                 (mulf_ (subf_ (nth_ (var_ "hgtmap") (var_ "n")) (nth_ (var_ "hgtmap") (var_ "p")))
                                        (subf_ (var_ "x") (int2float_ (var_ "p"))))))
            ]
          )
        )
      )
    )
  )

let func_seqMaxf =
  let_ "seqMaxf" (tyarrows_ [tyseq_ tyfloat_, tyfloat_]) (
    lam_ "s" (tyseq_ tyfloat_) (
      bindall_ [
        let_ "s_len" (tyint_) (length_ (var_ "s")),
        let_ "s_initsize" (tyint_) (divi_ (addi_ (var_ "s_len") (int_ 4095)) (int_ 4096)),
        reclets_add "maxwork" (tyarrows_ [tyseq_ tyfloat_, tyint_, tyint_, tyfloat_, tyfloat_]) (
          lam_ "s" (tyseq_ tyfloat_) (
            lam_ "i" (tyint_) (
              lam_ "end" (tyint_) (
                lam_ "candidate" (tyfloat_) (
                  if_ (eqf_ (var_ "i") (var_ "end"))
                      (var_ "candidate")
                      (app4f_ (var_ "maxwork")
                               (var_ "s")
                               (addi_ (var_ "i") (int_ 1))
                               (var_ "end")
                               (if_ (gtf_ (nth_ (var_ "s") (var_ "i")) (var_ "candidate"))
                                    (nth_ (var_ "s") (var_ "i"))
                                    (var_ "candidate")))
                )
              )
            )
          )
        ) (reclets_empty),
        let_ "maxwrap" (tyarrows_ [tyseq_ tyfloat_, tyint_, tyint_, tyfloat_]) (
          lam_ "s" (tyseq_ tyfloat_) (
            lam_ "i" (tyint_) (
              lam_ "end" (tyint_) (
                if_ (eqi_ (var_ "i") (var_ "end"))
                    (negf_ (float_ 1e300))
                    (app4f_ (var_ "maxwork")
                            (var_ "s")
                            (addi_ (var_ "i") (int_ 1))
                            (var_ "end")
                            (nth_ (var_ "s") (int_ 0)))
              )
            )
          )
        ),
        let_ "initfun" (tyarrow_ tyint_ tyfloat_) (
          lam_ "i" (tyint_) (
            bindall_ [
              let_ "start" (tyint_) (muli_ (var_ "i") (int_ 4096)),
              let_ "tmp" (tyint_) (addi_ (var_ "start") (int_ 4096)),
              let_ "end" (tyint_) (if_ (gti_ (var_ "tmp") (var_ "s_len"))
                                       (var_ "s_len")
                                       (var_ "tmp")),
              app3f_ (var_ "maxwrap")
                     (var_ "s")
                     (var_ "start")
                     (var_ "end")
            ]
          )
        ),
        let_ "partial" (tyseq_ tyfloat_) (
          specificinit_ (var_ "s_initsize") (var_ "initfun")
        ),
        app3f_ (var_ "maxwrap")
               (var_ "partial")
               (int_ 0)
               (var_ "s_initsize")
      ]
    )
  )

let func_seqSumf =
  let_ "seqSumf" (tyarrows_ [tyseq_ tyfloat_, tyfloat_]) (
    lam_ "s" (tyseq_ tyfloat_) (
      bindall_ [
        let_ "s_len" (tyint_) (length_ (var_ "s")),
        let_ "s_initsize" (tyint_) (divi_ (addi_ (var_ "s_len") (int_ 4095)) (int_ 4096)),
        reclets_add "sumwork" (tyarrows_ [tyseq_ tyfloat_, tyint_, tyint_, tyfloat_, tyfloat_]) (
          lam_ "s" (tyseq_ tyfloat_) (
            lam_ "i" (tyint_) (
              lam_ "end" (tyint_) (
                lam_ "acc" (tyfloat_) (
                  if_ (eqf_ (var_ "i") (var_ "end"))
                      (var_ "acc")
                      (app4f_ (var_ "sumwork")
                              (var_ "s")
                              (addi_ (var_ "i") (int_ 1))
                              (var_ "end")
                              (addf_ (var_ "acc") (nth_ (var_ "s") (var_ "i"))))
                )
              )
            )
          )
        ) (reclets_empty),
        let_ "initfun" (tyarrow_ tyint_ tyfloat_) (
          lam_ "i" (tyint_) (
            bindall_ [
              let_ "start" (tyint_) (muli_ (var_ "i") (int_ 4096)),
              let_ "tmp" (tyint_) (addi_ (var_ "start") (int_ 4096)),
              let_ "end" (tyint_) (if_ (gti_ (var_ "tmp") (var_ "s_len"))
                                       (var_ "s_len")
                                       (var_ "tmp")),
              app4f_ (var_ "sumwork")
                     (var_ "s")
                     (var_ "start")
                     (var_ "end")
                     (float_ 0.0)
            ]
          )
        ),
        let_ "partial" (tyseq_ tyfloat_) (
          specificinit_ (var_ "s_initsize") (var_ "initfun")
        ),
        app4f_ (var_ "sumwork")
               (var_ "partial")
               (int_ 0)
               (var_ "s_initsize")
               (float_ 0.0)
      ]
    )
  )

-- Unlike the other seq###f functions, this one does not have the ability to be
-- GPU optimized as there is no efficient way to traverse a list with an
-- accumulator in the current codegen. The temporary traverse instrinsic is
-- used in its place.
let func_seqAccsumf =
  let_ "seqAccsumf" (tyarrows_ [tyseq_ tyfloat_, tyseq_ tyfloat_]) (
    lam_ "s" (tyseq_ tyfloat_) (
      traverse_ caddf_ (float_ 0.0) (var_ "s")
    )
  )

-- Binary search to do the random selection from a set of weights
let func_binsearch =
  reclets_add "binsearch" (tyarrows_ [tyseq_ tyfloat_, tyfloat_, tyint_, tyint_, tyint_]) (
    lam_ "vec" (tyseq_ tyfloat_) (
      lam_ "p" (tyfloat_) (
        lam_ "low" (tyint_) (
          lam_ "up" (tyint_) (
            bindall_ [
              let_ "mid" (tyint_) (
                divi_ (subi_ (addi_ (var_ "low") (var_ "up"))
                             (int_ 1))
                      (int_ 2)
              ),
              if_ (eqi_ (var_ "low") (var_ "up"))
                  (var_ "low") -- bounds have come together (base case)
                  (if_ (ltf_ (var_ "p") (nth_ (var_ "vec") (var_ "mid")))
                       (app4f_ (var_ "binsearch") --then
                               (var_ "vec")
                               (var_ "p")
                               (var_ "low")
                               (var_ "mid"))
                       (app4f_ (var_ "binsearch") --else
                               (var_ "vec")
                               (var_ "p")
                               (addi_ (var_ "mid") (int_ 1))
                               (var_ "up")))
            ]
          )
        )
      )
    )
  ) (reclets_empty)

let func_pgf_print =
  let_ "pgf_print" (tyarrows_ [tyint_, tyseq_ tyfloat_, tyseq_ tyfloat_, tyunit_]) (
    lam_ "i" (tyint_) (
      lam_ "x" (tyseq_ tyfloat_) (
        lam_ "w" (tyseq_ tyfloat_) (
          bindall_ [
            let_ "pl" (tyarrows_ [tyint_, tyunit_]) (
              lam_ "j2" (tyint_) (
                bindall_ [
                  let_ "_" (tyunit_) (print_ (app1f_ (var_ "int2string") (var_ "j2"))),
                  let_ "_" (tyunit_) (print_ (str_ "\t")),
                  let_ "_" (tyunit_) (print_ (app1f_ (var_ "float2string") (nth_ (var_ "x") (var_ "j2")))),
                  let_ "_" (tyunit_) (print_ (str_ "\t80.0\t")),
                  let_ "_" (tyunit_) (print_ (app1f_ (var_ "float2string") (nth_ (var_ "w") (var_ "j2")))),
                  print_ (str_ "\n")
                ]
              )
            ),
            let_ "_" (tyunit_) (print_ (str_ "\nx[")),
            let_ "_" (tyunit_) (print_ (app1f_ (var_ "int2string") (var_ "i"))),
            let_ "_" (tyunit_) (print_ (str_ "]:\n")),
            let_ "_" (tyunit_) (
              if_ (eqi_ (length_ (var_ "x")) (int_ 0))
                  (unit_)
                  (app1f_ (var_ "pl") (int_ 0))
            ),
            reclets_add "printloop" (tyarrows_ [tyint_, tyunit_]) (
              lam_ "j" (tyint_) (
                if_ (geqi_ (var_ "j") (length_ (var_ "x")))
                    (unit_)
                    (bindall_ [
                  let_ "_" (tyunit_) (app1f_ (var_ "pl") (var_ "j")),
                  app1f_ (var_ "printloop")
                         (addi_ (var_ "j") (int_ 1))
                ])
              )
            ) (reclets_empty),
            let_ "_" (tyunit_) (app1f_ (var_ "printloop") (int_ 1)),
            print_ (str_ "\n")
          ]
        )
      )
    )
  )

let defcommon_ = bindall_ [
  defsize_,
  var_mu,
  var_sigma,
  var_nPoints,
  var_xLowerBound,
  var_xUpperBound,
  var_flightrange,
  var_velocity,
  var_altitude,
  var_heightMapSize,
  var_heightMap,
  var_obs,
  func_g_map,
  func_seqMaxf,
  func_seqSumf,
  func_seqAccsumf,
  func_binsearch,
  func_pgf_print
]

mexpr
use MExprCGOCaml in

if neqi (length argv) 4 then
  let _ = dprint argv in
  error "Must specify a target directory."
else
  -- carry on
let targetdir = get argv 3 in

let prog = libstd_ in
let prog = bind_ prog libio_ in
let prog = bind_ prog defcommon_ in

------- One run of SMC -------
let smcpart = bindall_ [
  -- These "refreshers" are needed until a type checker is in place
  -- since the lambda lifter does not carry over this type to the
  -- lifted argument.
  let_ "xLowerBound" (tyfloat_) (var_ "xLowerBound"),
  let_ "xUpperBound" (tyfloat_) (var_ "xUpperBound"),
  let_ "sigma" (tyfloat_) (var_ "sigma"),
  let_ "velocity" (tyfloat_) (var_ "velocity"),
  let_ "altitude" (tyfloat_) (var_ "altitude"),
  let_ "nPoints" (tyint_) (var_ "nPoints"),
  let_ "heightMapSize" (tyint_) (var_ "heightMapSize"),
  let_ "heightMap" (tyseq_ tyfloat_) (var_ "heightMap"),

  -- GENERATE INITIAL X
  let_ "rndinitf" (tyarrow_ tyint_ tyfloat_) (
    lam_ "_" (tyint_) (randUniformf_ (var_ "xLowerBound") (var_ "xUpperBound"))
  ),
  let_ "x" (tyseq_ tyfloat_) (
    specificinit_ (var_ "nPoints") (var_ "rndinitf")
  ),
  -- GENERATE INITIAL W
  let_ "fstobs" (tyfloat_) (nth_ (var_ "obs") (int_ 0)),
  let_ "wmapf" (tyarrow_ tyfloat_ tyfloat_) (
    lam_ "xelem" (tyfloat_) (
      logpdfNormalf_ (var_ "fstobs")
                     (app4f_ (var_ "g_map")
                             (var_ "altitude")
                             (var_ "heightMap")
                             (var_ "heightMapSize")
                             (var_ "xelem"))
                     (var_ "sigma")
    )
  ),
  let_ "w" (tyseq_ tyfloat_) (
    specificmap_ (var_ "wmapf") (var_ "x")
  ),
  -- NORMALIZE INITIAL W
  let_ "wmax" (tyfloat_) (app1f_ (var_ "seqMaxf") (var_ "w")),

  let_ "wmapf" (tyarrow_ tyfloat_ tyfloat_) (
    lam_ "welem" (tyfloat_) (
      expf_ (subf_ (var_ "welem") (var_ "wmax"))
    )
  ),
  let_ "w" (tyseq_ tyfloat_) (
    specificmap_ (var_ "wmapf") (var_ "w")
  ),
  let_ "wsum" (tyfloat_) (app1f_ (var_ "seqSumf") (var_ "w")),

  let_ "wmapf" (tyarrow_ tyfloat_ tyfloat_) (
    lam_ "welem" (tyfloat_) (
      divf_ (var_ "welem") (var_ "wsum")
    )
  ),
  let_ "w" (tyseq_ tyfloat_) (
    specificmap_ (var_ "wmapf") (var_ "w")
  ),

  -- NOW START ITERATING
  reclets_add "iterate_smc" (tyarrows_ [tyint_, tyint_, tyseq_ tyfloat_, tyseq_ tyfloat_, tyseq_ tyfloat_]) (
    lam_ "i" (tyint_) (
      lam_ "steps" (tyint_) (
        lam_ "x" (tyseq_ tyfloat_) (
          lam_ "w" (tyseq_ tyfloat_) (
            if_ (eqi_ (var_ "i") (var_ "steps"))
                (var_ "x") -- finished iterating
                (
            -- else: continue
            bindall_ [
              -- TEMP: Print X --
              --let_ "_" (tyunit_) (
              --  app3f_ (var_ "pgf_print")
              --         (subi_ (var_ "i") (int_ 1))
              --         (var_ "x")
              --         (var_ "w")
              --),
              -------------------

              -- These "refreshers" are needed until a type checker is in place
              -- since the lambda lifter does not carry over this type to the
              -- lifted argument.
              let_ "sigma" (tyfloat_) (var_ "sigma"),
              let_ "velocity" (tyfloat_) (var_ "velocity"),
              let_ "altitude" (tyfloat_) (var_ "altitude"),
              let_ "nPoints" (tyint_) (var_ "nPoints"),
              let_ "heightMapSize" (tyint_) (var_ "heightMapSize"),
              let_ "heightMap" (tyseq_ tyfloat_) (var_ "heightMap"),


              let_ "wacc" (tyseq_ tyfloat_) (
                app1f_ (var_ "seqAccsumf") (var_ "w")
              ),
              -- GENERATE THE PROPAGATED X
              let_ "propagate_x" (tyarrow_ tyint_ tyfloat_) (
                lam_ "_" (tyint_) (
                  bindall_ [
                    let_ "p" (tyfloat_) (randUniformf_ (float_ 0.0) (float_ 1.0)),
                    let_ "i" (tyint_) (
                      app4f_ (var_ "binsearch")
                             (var_ "wacc")
                             (var_ "p")
                             (int_ 0)
                             (subi_ (var_ "nPoints") (int_ 1))
                    ),
                    -- resample
                    let_ "x_new" (tyfloat_) (nth_ (var_ "x") (var_ "i")),
                    -- propagate
                    randNormalf_ (addf_ (var_ "x_new") (var_ "velocity")) (var_ "sigma")
                  ]
                )
              ),
              let_ "x_propagated" (tyseq_ tyfloat_) (
                specificinit_ (var_ "nPoints")
                              (var_ "propagate_x")
              ),

              -- UPDATE THE WEIGHTS W
              let_ "log_1onN" (tyfloat_) (
                logf_ (divf_ (float_ 1.0)
                             (int2float_ (var_ "nPoints")))
              ),
              let_ "ithobs" (tyfloat_) (nth_ (var_ "obs") (var_ "i")),
              let_ "wmapf" (tyarrow_ tyfloat_ tyfloat_) (
                lam_ "xelem" (tyfloat_) (
                  addf_ (logpdfNormalf_ (var_ "ithobs")
                                        (app4f_ (var_ "g_map")
                                                (var_ "altitude")
                                                (var_ "heightMap")
                                                (var_ "heightMapSize")
                                                (var_ "xelem"))
                                        (var_ "sigma"))
                        (var_ "log_1onN")
                )
              ),
              let_ "w" (tyseq_ tyfloat_) (
                specificmap_ (var_ "wmapf") (var_ "x_propagated")
              ),
              -- NORMALIZE THE NEW WEIGHTS W
              let_ "wmax" (tyfloat_) (app1f_ (var_ "seqMaxf") (var_ "w")),

              let_ "wmapf" (tyarrow_ tyfloat_ tyfloat_) (
                lam_ "welem" (tyfloat_) (
                  expf_ (subf_ (var_ "welem") (var_ "wmax"))
                )
              ),
              let_ "w" (tyseq_ tyfloat_) (
                specificmap_ (var_ "wmapf") (var_ "w")
              ),
              let_ "wsum" (tyfloat_) (app1f_ (var_ "seqSumf") (var_ "w")),

              let_ "wmapf" (tyarrow_ tyfloat_ tyfloat_) (
                lam_ "welem" (tyfloat_) (
                  divf_ (var_ "welem") (var_ "wsum")
                )
              ),
              let_ "w_updated" (tyseq_ tyfloat_) (
                specificmap_ (var_ "wmapf") (var_ "w")
              ),

              -- RECURSE TO THE NEXT ITERATION
              app4f_ (var_ "iterate_smc")
                     (addi_ (var_ "i") (int_ 1))
                     (var_ "steps")
                     (var_ "x_propagated")
                     (var_ "w_updated")
            ])
          )
        )
      )
    )
  ) (reclets_empty),

  let_ "x_res" (tyseq_ tyfloat_) (
    app4f_ (var_ "iterate_smc")
           (int_ 1)
           (var_ "flight_range")
           (var_ "x")
           (var_ "w")
  )
] in
---------------------------

------ Perform SMC ------
--let prog = bind prog smcpart in
-------------------------

------- Output Verification -------
--let prog = bind_ prog (let_ "_" (tyunit_) (print_ (str_ "\nx_res:\n"))) in
--let prog = bind_ prog (let_ "_" (tyunit_) (
--    app2f_ (var_ "printSeqf")
--           (var_ "nPoints")
--           (var_ "x_res")
--  )) in
-----------------------------------

------- Benchmark SMC -------
let bm =
  benchmark_ {bmparams_ with iters = 15}
             smcpart
in
let prog = bind_ prog bm in
-----------------------------

let res = codegen prog in

let _ = print (pprintCode 0 prog) in
let _ = print "\n" in

let ocamlfilename = concat targetdir "/cpucode.ml" in
let cudafilename = concat targetdir "/gpucode.cpp" in

let _ = writeFile ocamlfilename res.0 in
let _ = writeFile cudafilename res.1 in

()
