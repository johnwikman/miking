-- A collection of different profiles to use in the cost estimation


-- Cost profile information for OCaml
let costprof_ocaml_costperelem = 50
let costprof_ocaml = {{{{{{{{{{{{{{{costprof_vanilla
                                    with c_addi = 1}
                                    with c_subi = 1}
                                    with c_muli = 3}
                                    with c_divi = 5}
                                    with c_modi = 5}
                                    with c_addf = 2}
                                    with c_subf = 2}
                                    with c_mulf = 5}
                                    with c_divf = 5}
                                    with c_eqi = 2}
                                    with c_lti = 2}
                                    with c_nth = 10}
                                    with c_int2float = 5}
                                    with c_if = 5}
                                    with c_reccall = 32}

-- Cost profile information for CUDA
let costprof_cuda_fixed_latency = 40000000
let costprof_cuda_relative_latency = 10
let costprof_cuda_capacity_per_thread = 2048
let costprof_cuda_paralleldenominator = 1536
let costprof_cuda = {{{{{{{{{{{{{{{costprof_vanilla
                                   with c_addi = 12}
                                   with c_subi = 12}
                                   with c_muli = 28}
                                   with c_divi = 62}
                                   with c_modi = 62}
                                   with c_addf = 22}
                                   with c_subf = 22}
                                   with c_mulf = 28}
                                   with c_divf = 92}
                                   with c_eqi = 22}
                                   with c_lti = 22}
                                   with c_nth = 300}
                                   with c_int2float = 22}
                                   with c_if = 62}
                                   with c_reccall = 200}
