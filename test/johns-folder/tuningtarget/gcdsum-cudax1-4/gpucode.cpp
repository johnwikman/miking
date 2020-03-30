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
	value gpuhost_fun70_gcdsum(value packedInts, value packedFloats, value arg0, value arg1);
}

__device__ inline int gpu_addi(int x, int y);
__device__ inline int gpu_modi(int x, int y);
__device__ inline bool gpu_eqi(int x, int y);
__device__ int gpudevice_fun58_gcd(int arg59_a, int arg60_b);
__device__ int gpudevice_fun64_recloop(value *arg69_yvec, int arg68_x, int arg67_n, int arg65_acc, int arg66_i);
__device__ int gpudevice_fun70_gcdsum(value *arg61_yvec, int arg62_n, int arg63_x);

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__device__ inline int gpu_modi(int x, int y) {return x % y;}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ int gpudevice_fun58_gcd(int arg59_a, int arg60_b)
{
	return (gpu_eqi(arg60_b, 0)) ? (arg59_a) : (gpudevice_fun58_gcd(arg60_b, gpu_modi(arg59_a, arg60_b)));
}

__device__ int gpudevice_fun64_recloop(value *arg69_yvec, int arg68_x, int arg67_n, int arg65_acc, int arg66_i)
{
	return (gpu_eqi(arg66_i, arg67_n)) ? (arg65_acc) : (gpudevice_fun64_recloop(arg69_yvec, arg68_x, arg67_n, gpu_addi(arg65_acc, gpudevice_fun58_gcd(arg68_x, Int_val((arg69_yvec[arg66_i])))), gpu_addi(arg66_i, 1)));
}

__device__ int gpudevice_fun70_gcdsum(value *arg61_yvec, int arg62_n, int arg63_x)
{
	return gpudevice_fun64_recloop(arg61_yvec, arg63_x, arg62_n, 0, 0);
}

__global__ void gpuglobal_fun70_gcdsum(value *cuda_arg0, int cuda_arg1, value *cuda_arg2, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(cuda_arg2[i]);
		outarr[i] = Val_int(gpudevice_fun70_gcdsum(cuda_arg0, cuda_arg1, v));
	}
}

value gpuhost_fun70_gcdsum(value packedInts, value packedFloats, value arg0, value arg1)
{
	CAMLparam4(packedInts, packedFloats, arg0, arg1);
	CAMLlocal1(outarr);

	int elemPerThread = Int_val(Field(packedInts, 0));
	int n = Wosize_val(arg1);

	int threadsPerBlock;
	int elemPerBlock;
	int blockCount;
	cudaDeviceGetAttribute(&threadsPerBlock, cudaDevAttrMaxThreadsPerBlock, 0);
	elemPerBlock = threadsPerBlock * elemPerThread;
	blockCount = (n + elemPerBlock - 1) / elemPerBlock;
	value *cuda_outarr;
	value *cuda_arg0;
	value *cuda_arg1;
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMalloc(&cuda_arg0, Wosize_val(arg0) * sizeof(value));
	cudaMalloc(&cuda_arg1, Wosize_val(arg1) * sizeof(value));
	cudaMemcpy(cuda_arg0, Op_val(arg0), Wosize_val(arg0) * sizeof(value), cudaMemcpyHostToDevice);
	cudaMemcpy(cuda_arg1, Op_val(arg1), Wosize_val(arg1) * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_fun70_gcdsum<<<blockCount,threadsPerBlock>>>(cuda_arg0, Int_val(Field(packedInts, 1)), cuda_arg1, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);

	CAMLreturn(outarr);
}