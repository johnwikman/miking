#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef FLAT_FLOAT_ARRAY
#error OCaml floats are not stored in flat array, cannot GPU optimize them.
#endif
#ifdef ARCH_ALIGN_DOUBLE
#error Doubles are not same size as OCaml values, cannot GPU optimize them.
#endif

extern "C" {
	value gpuhost_fib(value arg0);
}

__device__ int gpudevice_fib(int n);
__device__ int gpudevice_fib_helper(int i, int n, int prev, int current);
__device__ inline bool gpu_eqi(int x, int y);
__device__ inline int gpu_addi(int x, int y);

__device__ int gpudevice_fib(int n)
{
	return gpudevice_fib_helper(0, n, 1, 0);
}

__device__ int gpudevice_fib_helper(int i, int n, int prev, int current)
{
	return (gpu_eqi(i, n)) ? (current) : (gpudevice_fib_helper(gpu_addi(i, 1), n, current, gpu_addi(prev, current)));
}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__global__ void gpuglobal_fib(value *outarr, int n)
{
	int i;
	int start = threadIdx.x * 16;
	int end = start + 16;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		outarr[i] = Val_int(gpudevice_fib(i));
	}
}

value gpuhost_fib(value arg0)
{
	CAMLparam1(arg0);
	CAMLlocal1(outarr);
	int n = Int_val(arg0);

	value *cuda_outarr;
	cudaMalloc(&cuda_outarr, n * sizeof(value));

	gpuglobal_fib<<<1,(n + 15) / 16>>>(cuda_outarr, n);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);

	CAMLreturn(outarr);
}