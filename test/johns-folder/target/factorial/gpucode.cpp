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
	value gpuhost_factidx(value packedInts, value packedFloats, value arg0);
}

__device__ int gpudevice_factidx(int i, int ignored_arg);
__device__ int gpudevice_factorial(int n);
__device__ inline bool gpu_eqi(int x, int y);
__device__ inline int gpu_subi(int x, int y);
__device__ inline int gpu_muli(int x, int y);

__device__ int gpudevice_factidx(int i, int ignored_arg)
{
	return gpudevice_factorial(i);
}

__device__ int gpudevice_factorial(int n)
{
	return (gpu_eqi(n, 0)) ? (1) : (gpu_muli(n, gpudevice_factorial(gpu_subi(n, 1))));
}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ inline int gpu_subi(int x, int y) {return x - y;}

__device__ inline int gpu_muli(int x, int y) {return x * y;}

__global__ void gpuglobal_factidx(value *cuda_arg0, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(cuda_arg0[i]);
		outarr[i] = Val_int(gpudevice_factidx(i, v));
	}
}

value gpuhost_factidx(value packedInts, value packedFloats, value arg0)
{
	CAMLparam3(packedInts, packedFloats, arg0);
	CAMLlocal1(outarr);

	int elemPerThread = Int_val(Field(packedInts, 0));
	int n = Wosize_val(arg0);

	int threadsPerBlock;
	int elemPerBlock;
	int blockCount;
	cudaDeviceGetAttribute(&threadsPerBlock, cudaDevAttrMaxThreadsPerBlock, 0);
	elemPerBlock = threadsPerBlock * elemPerThread;
	blockCount = (n + elemPerBlock - 1) / elemPerBlock;
	value *cuda_outarr;
	value *cuda_arg0;
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMalloc(&cuda_arg0, Wosize_val(arg0) * sizeof(value));
	cudaMemcpy(cuda_arg0, Op_val(arg0), Wosize_val(arg0) * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_factidx<<<blockCount,threadsPerBlock>>>(cuda_arg0, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}