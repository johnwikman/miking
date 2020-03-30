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
	value gpuhost_fun177_matrixMaxSumiWorker(value packedInts, value packedFloats, value arg0, value arg1);
}

__device__ int gpudevice_fun157_maxi(int arg155_x, int arg156_y);
__device__ inline bool gpu_lti(int x, int y);
__device__ inline int gpu_addi(int x, int y);
__device__ inline bool gpu_eqi(int x, int y);
__device__ int gpudevice_fun168_vecmsum(int arg176_b_cols, value *arg175_b, value *arg174_a, int arg173_innerDim, int arg169_acc, int arg170_p, int arg171_a_offset, int arg172_b_offset);
__device__ inline int gpu_muli(int x, int y);
__device__ inline int gpu_modi(int x, int y);
__device__ inline int gpu_divi(int x, int y);
__device__ int gpudevice_fun177_matrixMaxSumiWorker(int arg158_innerDim, int arg159_a_rows, int arg160_b_cols, value *arg161_a, value *arg162_b, int arg163_idx);

__device__ int gpudevice_fun157_maxi(int arg155_x, int arg156_y)
{
	return (gpu_lti(arg155_x, arg156_y)) ? (arg156_y) : (arg155_x);
}

__device__ inline bool gpu_lti(int x, int y) {return x < y;}

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ int gpudevice_fun168_vecmsum(int arg176_b_cols, value *arg175_b, value *arg174_a, int arg173_innerDim, int arg169_acc, int arg170_p, int arg171_a_offset, int arg172_b_offset)
{
	return (gpu_eqi(arg170_p, arg173_innerDim)) ? (arg169_acc) : (gpudevice_fun168_vecmsum(arg176_b_cols, arg175_b, arg174_a, arg173_innerDim, gpu_addi(arg169_acc, gpudevice_fun157_maxi(Int_val((arg174_a[arg171_a_offset])), Int_val((arg175_b[arg172_b_offset])))), gpu_addi(arg170_p, 1), gpu_addi(arg171_a_offset, 1), gpu_addi(arg172_b_offset, arg176_b_cols)));
}

__device__ inline int gpu_muli(int x, int y) {return x * y;}

__device__ inline int gpu_modi(int x, int y) {return x % y;}

__device__ inline int gpu_divi(int x, int y) {return x / y;}

__device__ int gpudevice_fun177_matrixMaxSumiWorker(int arg158_innerDim, int arg159_a_rows, int arg160_b_cols, value *arg161_a, value *arg162_b, int arg163_idx)
{
	int var164_row = gpu_divi(arg163_idx, arg160_b_cols);
	int var165_col = gpu_modi(arg163_idx, arg160_b_cols);
	int var166_a_start_offset = gpu_muli(arg158_innerDim, var164_row);
	int var167_b_start_offset = var165_col;
	return gpudevice_fun168_vecmsum(arg160_b_cols, arg162_b, arg161_a, arg158_innerDim, 0, 0, var166_a_start_offset, var167_b_start_offset);
}

__global__ void gpuglobal_fun177_matrixMaxSumiWorker(int cuda_arg0, int cuda_arg1, int cuda_arg2, value *cuda_arg3, value *cuda_arg4, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		outarr[i] = Val_int(gpudevice_fun177_matrixMaxSumiWorker(cuda_arg0, cuda_arg1, cuda_arg2, cuda_arg3, cuda_arg4, i));
	}
}

value gpuhost_fun177_matrixMaxSumiWorker(value packedInts, value packedFloats, value arg0, value arg1)
{
	CAMLparam4(packedInts, packedFloats, arg0, arg1);
	CAMLlocal1(outarr);

	int elemPerThread = Int_val(Field(packedInts, 0));
	int n = Int_val(Field(packedInts, 4));

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

	gpuglobal_fun177_matrixMaxSumiWorker<<<blockCount,threadsPerBlock>>>(Int_val(Field(packedInts, 1)), Int_val(Field(packedInts, 2)), Int_val(Field(packedInts, 3)), cuda_arg0, cuda_arg1, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);

	CAMLreturn(outarr);
}