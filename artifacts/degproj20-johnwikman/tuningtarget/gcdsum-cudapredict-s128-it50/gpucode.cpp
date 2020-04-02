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
	value gpuhost_fun90_gcdsum(value packedInts, value packedFloats, value arg0, value arg1);
}

__device__ inline int gpu_addi(int x, int y);
__device__ inline int gpu_modi(int x, int y);
__device__ inline bool gpu_eqi(int x, int y);
__device__ int gpudevice_fun78_gcd(int arg79_a, int arg80_b);
__device__ int gpudevice_fun84_recloop(value *arg89_yvec, int arg88_x, int arg87_n, int arg85_acc, int arg86_i);
__device__ int gpudevice_fun90_gcdsum(value *arg81_yvec, int arg82_n, int arg83_x);

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__device__ inline int gpu_modi(int x, int y) {return x % y;}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ int gpudevice_fun78_gcd(int arg79_a, int arg80_b)
{
	return (gpu_eqi(arg80_b, 0)) ? (arg79_a) : (gpudevice_fun78_gcd(arg80_b, gpu_modi(arg79_a, arg80_b)));
}

__device__ int gpudevice_fun84_recloop(value *arg89_yvec, int arg88_x, int arg87_n, int arg85_acc, int arg86_i)
{
	return (gpu_eqi(arg86_i, arg87_n)) ? (arg85_acc) : (gpudevice_fun84_recloop(arg89_yvec, arg88_x, arg87_n, gpu_addi(arg85_acc, gpudevice_fun78_gcd(arg88_x, Int_val((arg89_yvec[arg86_i])))), gpu_addi(arg86_i, 1)));
}

__device__ int gpudevice_fun90_gcdsum(value *arg81_yvec, int arg82_n, int arg83_x)
{
	return gpudevice_fun84_recloop(arg81_yvec, arg83_x, arg82_n, 0, 0);
}

__global__ void gpuglobal_fun90_gcdsum(value *cuda_arg0, int cuda_arg1, value *cuda_arg2, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(cuda_arg2[i]);
		outarr[i] = Val_int(gpudevice_fun90_gcdsum(cuda_arg0, cuda_arg1, v));
	}
}

value gpuhost_fun90_gcdsum(value packedInts, value packedFloats, value arg0, value arg1)
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

	gpuglobal_fun90_gcdsum<<<blockCount,threadsPerBlock>>>(cuda_arg0, Int_val(Field(packedInts, 1)), cuda_arg1, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);

	CAMLreturn(outarr);
}