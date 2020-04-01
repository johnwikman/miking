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
	value gpuhost_fun80_gcdsum(value packedInts, value packedFloats, value arg0, value arg1);
}

__device__ inline int gpu_addi(int x, int y);
__device__ inline int gpu_modi(int x, int y);
__device__ inline bool gpu_eqi(int x, int y);
__device__ int gpudevice_fun68_gcd(int arg69_a, int arg70_b);
__device__ int gpudevice_fun74_recloop(value *arg79_yvec, int arg78_x, int arg77_n, int arg75_acc, int arg76_i);
__device__ int gpudevice_fun80_gcdsum(value *arg71_yvec, int arg72_n, int arg73_x);

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__device__ inline int gpu_modi(int x, int y) {return x % y;}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ int gpudevice_fun68_gcd(int arg69_a, int arg70_b)
{
	return (gpu_eqi(arg70_b, 0)) ? (arg69_a) : (gpudevice_fun68_gcd(arg70_b, gpu_modi(arg69_a, arg70_b)));
}

__device__ int gpudevice_fun74_recloop(value *arg79_yvec, int arg78_x, int arg77_n, int arg75_acc, int arg76_i)
{
	return (gpu_eqi(arg76_i, arg77_n)) ? (arg75_acc) : (gpudevice_fun74_recloop(arg79_yvec, arg78_x, arg77_n, gpu_addi(arg75_acc, gpudevice_fun68_gcd(arg78_x, Int_val((arg79_yvec[arg76_i])))), gpu_addi(arg76_i, 1)));
}

__device__ int gpudevice_fun80_gcdsum(value *arg71_yvec, int arg72_n, int arg73_x)
{
	return gpudevice_fun74_recloop(arg71_yvec, arg73_x, arg72_n, 0, 0);
}

__global__ void gpuglobal_fun80_gcdsum(value *cuda_arg0, int cuda_arg1, value *cuda_arg2, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(cuda_arg2[i]);
		outarr[i] = Val_int(gpudevice_fun80_gcdsum(cuda_arg0, cuda_arg1, v));
	}
}

value gpuhost_fun80_gcdsum(value packedInts, value packedFloats, value arg0, value arg1)
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

	gpuglobal_fun80_gcdsum<<<blockCount,threadsPerBlock>>>(cuda_arg0, Int_val(Field(packedInts, 1)), cuda_arg1, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);

	CAMLreturn(outarr);
}