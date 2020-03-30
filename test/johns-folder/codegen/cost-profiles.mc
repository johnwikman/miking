-- A collection of different profiles to use in the cost estimation


-- Cost profile information for OCaml
let costprof_ocaml_costperelem = 24
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
                                    with c_reccall = 1}

-- Cost profile information for CUDA
let costprof_cuda_fixed_latency = 10000000
let costprof_cuda_relative_latency = 3
let costprof_cuda_capacity_per_thread = 2048
let costprof_cuda_paralleldenominator = 1024
let costprof_cuda = {{{{{{{{{{{{{{{costprof_vanilla
	                                 with c_addi = 30}
                                   with c_subi = 80}
                                   with c_muli = 100}
                                   with c_divi = 500}
                                   with c_modi = 1000}
	                                 with c_addf = 400}
                                   with c_subf = 400}
                                   with c_mulf = 500}
                                   with c_divf = 1000}
                                   with c_eqi = 100}
                                   with c_lti = 100}
                                   with c_nth = 1000}
                                   with c_int2float = 200}
                                   with c_if = 600}
                                   with c_reccall = 50000}
