-- A collection of different profiles to use in the cost estimation


-- Cost profile information for OCaml
let costprof_ocaml_costperelem = 30
let costprof_ocaml = {{{{{{{{{{{{{{{costprof_vanilla
	                                  with c_addi = 1}
                                    with c_subi = 1}
                                    with c_muli = 3}
                                    with c_divi = 5}
                                    with c_modi = 5}
	                                  with c_addf = 1}
                                    with c_subf = 1}
                                    with c_mulf = 3}
                                    with c_divf = 5}
                                    with c_eqi = 1}
                                    with c_lti = 1}
                                    with c_nth = 2}
                                    with c_int2float = 5}
                                    with c_if = 5}
                                    with c_reccall = 16}

-- Cost profile information for CUDA
let costprof_cuda_fixed_latency = 30000000
let costprof_cuda_relative_latency = 3
let costprof_cuda_capacity_per_thread = 2048
let costprof_cuda_paralleldenominator = 1024
let costprof_cuda = {{{{{{{{{{{{{{{costprof_vanilla
	                                 with c_addi = 22}
                                   with c_subi = 22}
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
                                   with c_reccall = 500}
