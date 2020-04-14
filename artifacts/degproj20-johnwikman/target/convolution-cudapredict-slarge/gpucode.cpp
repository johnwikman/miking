#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <stdlib.h>
#include <curand_kernel.h>

#ifndef FLAT_FLOAT_ARRAY
#error OCaml floats are not stored in flat array, cannot GPU optimize them.
#endif
#ifdef ARCH_ALIGN_DOUBLE
#error Doubles are not same size as OCaml values, cannot GPU optimize them.
#endif

extern "C" {
	value gpuhost_fun186_convolute17Worker(value packedInts, value packedFloats, value arg0, value arg1);
}

__device__ double gpudevice_fun186_convolute17Worker(value *arg168_filter, int arg169_rows, int arg170_cols, value *arg171_mat, int arg172_idx);
__device__ inline int gpu_divi(int x, int y);
__device__ inline int gpu_modi(int x, int y);
__device__ inline int gpu_subi(int x, int y);
__device__ inline int gpu_muli(int x, int y);
__device__ double gpudevice_fun180_work(value *arg185_filter, int arg184_originalOffset, value *arg183_mat, double arg181_acc, int arg182_i);
__device__ inline bool gpu_eqi(int x, int y);
__device__ inline int gpu_addi(int x, int y);
__device__ inline double gpu_mulf(double x, double y);
__device__ inline double gpu_addf(double x, double y);

__device__ double gpudevice_fun186_convolute17Worker(value *arg168_filter, int arg169_rows, int arg170_cols, value *arg171_mat, int arg172_idx)
{
	int var173_row = gpu_divi(arg172_idx, arg170_cols);
	int var174_col = gpu_modi(arg172_idx, arg170_cols);
	int var175_originalRows = arg169_rows;
	int var176_originalCols = gpu_addi(arg170_cols, 16);
	int var177_originalRow = var173_row;
	int var178_originalCol = gpu_addi(var174_col, 8);
	int var179_originalOffset = gpu_subi(gpu_addi(gpu_muli(var177_originalRow, var176_originalCols), var178_originalCol), 8);
	return gpudevice_fun180_work(arg168_filter, var179_originalOffset, arg171_mat, 0.0, 0);
}

__device__ inline int gpu_divi(int x, int y) {return x / y;}

__device__ inline int gpu_modi(int x, int y) {return x % y;}

__device__ inline int gpu_subi(int x, int y) {return x - y;}

__device__ inline int gpu_muli(int x, int y) {return x * y;}

__device__ double gpudevice_fun180_work(value *arg185_filter, int arg184_originalOffset, value *arg183_mat, double arg181_acc, int arg182_i)
{
	return (gpu_eqi(arg182_i, 17)) ? (arg181_acc) : (gpudevice_fun180_work(arg185_filter, arg184_originalOffset, arg183_mat, gpu_addf(arg181_acc, gpu_mulf((((double *) arg183_mat)[gpu_addi(arg184_originalOffset, arg182_i)]), (((double *) arg185_filter)[arg182_i]))), gpu_addi(arg182_i, 1)));
}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__device__ inline double gpu_mulf(double x, double y) {return x * y;}

__device__ inline double gpu_addf(double x, double y) {return x + y;}

__global__ void gpuglobal_fun186_convolute17Worker(value *cuda_arg0, int cuda_arg1, int cuda_arg2, value *cuda_arg3, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		((double *) outarr)[i] = gpudevice_fun186_convolute17Worker(cuda_arg0, cuda_arg1, cuda_arg2, cuda_arg3, i);
	}
}

value gpuhost_fun186_convolute17Worker(value packedInts, value packedFloats, value arg0, value arg1)
{
	CAMLparam4(packedInts, packedFloats, arg0, arg1);
	CAMLlocal1(outarr);

	int elemPerThread = Int_val(Field(packedInts, 0));
	int n = Int_val(Field(packedInts, 3));

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

	gpuglobal_fun186_convolute17Worker<<<blockCount,threadsPerBlock>>>(cuda_arg0, Int_val(Field(packedInts, 1)), Int_val(Field(packedInts, 2)), cuda_arg1, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);

	CAMLreturn(outarr);
}