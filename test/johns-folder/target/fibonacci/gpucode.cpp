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
	value gpuhost_fun69_fib(value packedInts, value packedFloats);
}

__device__ inline int gpu_addi(int x, int y);
__device__ inline bool gpu_eqi(int x, int y);
__device__ int gpudevice_fun63_fib_helper(int arg64_i, int arg65_n, int arg66_prev, int arg67_current);
__device__ int gpudevice_fun69_fib(int arg68_n);

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ int gpudevice_fun63_fib_helper(int arg64_i, int arg65_n, int arg66_prev, int arg67_current)
{
	return (gpu_eqi(arg64_i, arg65_n)) ? (arg67_current) : (gpudevice_fun63_fib_helper(gpu_addi(arg64_i, 1), arg65_n, arg67_current, gpu_addi(arg66_prev, arg67_current)));
}

__device__ int gpudevice_fun69_fib(int arg68_n)
{
	return gpudevice_fun63_fib_helper(0, arg68_n, 1, 0);
}

__global__ void gpuglobal_fun69_fib(value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		outarr[i] = Val_int(gpudevice_fun69_fib(i));
	}
}

value gpuhost_fun69_fib(value packedInts, value packedFloats)
{
	CAMLparam2(packedInts, packedFloats);
	CAMLlocal1(outarr);

	int elemPerThread = Int_val(Field(packedInts, 0));
	int n = Int_val(Field(packedInts, 1));

	int threadsPerBlock;
	int elemPerBlock;
	int blockCount;
	cudaDeviceGetAttribute(&threadsPerBlock, cudaDevAttrMaxThreadsPerBlock, 0);
	elemPerBlock = threadsPerBlock * elemPerThread;
	blockCount = (n + elemPerBlock - 1) / elemPerBlock;
	value *cuda_outarr;
	cudaMalloc(&cuda_outarr, n * sizeof(value));

	gpuglobal_fun69_fib<<<blockCount,threadsPerBlock>>>(cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);

	CAMLreturn(outarr);
}