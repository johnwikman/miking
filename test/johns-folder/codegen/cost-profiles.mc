-- A collection of different profiles to use in the cost estimation


-- Cost profile information for OCaml
let costprof_ocaml = {{{{{{{{{{{{{{{costprof_vanilla
	                                  with c_addi = 5}
                                    with c_subi = 5}
                                    with c_muli = 7}
                                    with c_divi = 7}
                                    with c_modi = 7}
	                                  with c_addf = 5}
                                    with c_subf = 5}
                                    with c_mulf = 7}
                                    with c_divf = 7}
                                    with c_eqi = 5}
                                    with c_lti = 5}
                                    with c_nth = 5}
                                    with c_int2float = 5}
                                    with c_if = 5}
                                    with c_reccall = 10}

-- Cost profile information for CUDA
let costprof_cuda_fixed_latency = 150000000
let costprof_cuda_relative_latency = 2
let costprof_cuda_capacity_per_thread = 2048
let costprof_cuda_paralleldenominator = 1024
let costprof_cuda = {{{{{{{{{{{{{{{costprof_vanilla
	                                 with c_addi = 22}
                                   with c_subi = 22}
                                   with c_muli = 28}
                                   with c_divi = 28}
                                   with c_modi = 28}
	                                 with c_addf = 22}
                                   with c_subf = 22}
                                   with c_mulf = 28}
                                   with c_divf = 28}
                                   with c_eqi = 22}
                                   with c_lti = 22}
                                   with c_nth = 200}
                                   with c_int2float = 44}
                                   with c_if = 300}
                                   with c_reccall = 70000}
