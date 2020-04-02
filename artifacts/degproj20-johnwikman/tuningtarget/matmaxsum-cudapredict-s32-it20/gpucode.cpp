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
	value gpuhost_fun187_matrixMaxSumiWorker(value packedInts, value packedFloats, value arg0, value arg1);
}

__device__ int gpudevice_fun167_maxi(int arg165_x, int arg166_y);
__device__ inline bool gpu_lti(int x, int y);
__device__ inline int gpu_addi(int x, int y);
__device__ inline bool gpu_eqi(int x, int y);
__device__ int gpudevice_fun178_vecmsum(int arg186_b_cols, value *arg185_b, value *arg184_a, int arg183_innerDim, int arg179_acc, int arg180_p, int arg181_a_offset, int arg182_b_offset);
__device__ inline int gpu_muli(int x, int y);
__device__ inline int gpu_modi(int x, int y);
__device__ inline int gpu_divi(int x, int y);
__device__ int gpudevice_fun187_matrixMaxSumiWorker(int arg168_innerDim, int arg169_a_rows, int arg170_b_cols, value *arg171_a, value *arg172_b, int arg173_idx);

__device__ int gpudevice_fun167_maxi(int arg165_x, int arg166_y)
{
	return (gpu_lti(arg165_x, arg166_y)) ? (arg166_y) : (arg165_x);
}

__device__ inline bool gpu_lti(int x, int y) {return x < y;}

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ int gpudevice_fun178_vecmsum(int arg186_b_cols, value *arg185_b, value *arg184_a, int arg183_innerDim, int arg179_acc, int arg180_p, int arg181_a_offset, int arg182_b_offset)
{
	return (gpu_eqi(arg180_p, arg183_innerDim)) ? (arg179_acc) : (gpudevice_fun178_vecmsum(arg186_b_cols, arg185_b, arg184_a, arg183_innerDim, gpu_addi(arg179_acc, gpudevice_fun167_maxi(Int_val((arg184_a[arg181_a_offset])), Int_val((arg185_b[arg182_b_offset])))), gpu_addi(arg180_p, 1), gpu_addi(arg181_a_offset, 1), gpu_addi(arg182_b_offset, arg186_b_cols)));
}

__device__ inline int gpu_muli(int x, int y) {return x * y;}

__device__ inline int gpu_modi(int x, int y) {return x % y;}

__device__ inline int gpu_divi(int x, int y) {return x / y;}

__device__ int gpudevice_fun187_matrixMaxSumiWorker(int arg168_innerDim, int arg169_a_rows, int arg170_b_cols, value *arg171_a, value *arg172_b, int arg173_idx)
{
	int var174_row = gpu_divi(arg173_idx, arg170_b_cols);
	int var175_col = gpu_modi(arg173_idx, arg170_b_cols);
	int var176_a_start_offset = gpu_muli(arg168_innerDim, var174_row);
	int var177_b_start_offset = var175_col;
	return gpudevice_fun178_vecmsum(arg170_b_cols, arg172_b, arg171_a, arg168_innerDim, 0, 0, var176_a_start_offset, var177_b_start_offset);
}

__global__ void gpuglobal_fun187_matrixMaxSumiWorker(int cuda_arg0, int cuda_arg1, int cuda_arg2, value *cuda_arg3, value *cuda_arg4, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		outarr[i] = Val_int(gpudevice_fun187_matrixMaxSumiWorker(cuda_arg0, cuda_arg1, cuda_arg2, cuda_arg3, cuda_arg4, i));
	}
}

value gpuhost_fun187_matrixMaxSumiWorker(value packedInts, value packedFloats, value arg0, value arg1)
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

	gpuglobal_fun187_matrixMaxSumiWorker<<<blockCount,threadsPerBlock>>>(Int_val(Field(packedInts, 1)), Int_val(Field(packedInts, 2)), Int_val(Field(packedInts, 3)), cuda_arg0, cuda_arg1, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);

	CAMLreturn(outarr);
}