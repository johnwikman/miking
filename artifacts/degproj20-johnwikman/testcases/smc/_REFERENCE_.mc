-- randUniformf <from> <to>
-- randNormalf <mu> <sigma>
-- logpdfNormalf <x> <mu> <sigma> (compute the pdf at the point x)
-- expf <v>
-- logf <v>

let sigma = 1.0 -- std deviation 1 in the normal distribution

let n = 200

let xLowerBound = 0
let xUpperBound = 200

let map_size = 201
let altitude = 70.0

-- The observed heights at each point
let obs : [Float] = [100.0, 100.1] // KNOWN AT THE START!

-- The height map
let heightMap : [Float] = [...]

-- The map function
let g_map : Float -> Float = lam x.
  let p = floorfi x in
  let n = ceilfi x in
  if or (lti p 0) (lti (subi map_size 1) n) then
    100000.0
  else
    let ap = nth heightMap p in
    let an = nth heightMap n in
    subf altitude
         (addf ap
               (mulf (subf an ap) (subf x p)))


mexpr -- start

-- Generate n uniformly random points in the interval [xLowerBound, xUpperBound]
let x : [Float] = seqInit n (lam _. randUniformf xLowerBound xUpperBound) in

-- Generate n weights
let w : [Float] = seqMap (lam xelem. logpdfNormalf (nth obs 0) (g_map xelem) sigma) x in

-- Normalize the weights
let wmax : Float = maxf w in
let w = seqMap (lam a. expf (subf a wmax)) w in
let wsum : Float = sumf w in
let w = seqMap (lam a. divf a wsum) w in


-- NOW ITERATE

recursive let iterate_smc = lam i. lam steps. lam x. lam w.
  if eqi i steps then
    x
  else -- continue

  -- loop invariant: the w weights are normalized such that sum(w) = 1

  let wacc : [Float] = accsumf w in

  let x_propagated = seqInit n (lam _.
    let p = randUniformf 0.0 1.0 in

    recursive let binsearch = lam low. lam up.
      if eqi low up then
        low --found our position!
      else -- continue

      let mid = divi (subi (addi low up) 1) 2 in
      let w_mid = nth w mid in
      if ltf p w_mid then
        binsearch low mid
      else
        binsearch (addi mid 1) up
    in
    let i = binsearch 0 (subi n 1) in

    -- Resample
    let x_new = nth x i in

    -- Propagate
    randNormalf (addf x_new velocity) sigma
  ) in

  let log_1onN = logf (divf 1.0 (int2float n)) in

  -- Generate n weights
  let w : [Float] = seqMap (lam xelem. addf (logpdfNormalf (nth obs i) (g_map xelem) sigma) log_1onN) x_propagated in
  -- Normalize the weights
  let wmax : Float = maxf w in
  let w = seqMap (lam a. expf (subi a wmax)) w in
  let wsum : Float = sumf w in
  let w_updated = seqMap (lam a. divf a wsum) w in

  iterate_smc (addi i 1) steps x_propagated w_updated
in
-- The resulting vector x after 100 steps!
let x_res = iterate_smc 1 100 x w
