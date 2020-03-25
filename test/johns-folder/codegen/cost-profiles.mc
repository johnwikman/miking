-- A collection of different profiles to use in the cost estimation

-- Cost profile information for CUDA
let costprof_cuda = {costprof_vanilla with c_nth = 32}
let costprof_cuda_capacity_per_thread = 1024

