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
	value gpuhost_fun160_matrixATAfWorker(value packedInts, value packedFloats, value arg0);
	value gpuhost_fun141_matrixMulfWorker(value packedInts, value packedFloats, value arg0, value arg1);
}

__device__ double gpudevice_fun160_matrixATAfWorker(int arg142_rows, int arg143_cols, value *arg144_a, int arg145_idx);
__device__ double gpudevice_fun152_dotprod(int arg159_outerDim, value *arg158_a, int arg157_innerDim, double arg153_acc, int arg154_p, int arg155_aT_offset, int arg156_a_offset);
__device__ inline int gpu_addi(int x, int y);
__device__ inline double gpu_mulf(double x, double y);
__device__ inline double gpu_addf(double x, double y);
__device__ inline bool gpu_eqi(int x, int y);
__device__ double gpudevice_fun132_dotprod(int arg140_b_cols, value *arg139_b, value *arg138_a, int arg137_innerDim, double arg133_acc, int arg134_p, int arg135_a_offset, int arg136_b_offset);
__device__ inline int gpu_muli(int x, int y);
__device__ inline int gpu_modi(int x, int y);
__device__ inline int gpu_divi(int x, int y);
__device__ double gpudevice_fun141_matrixMulfWorker(int arg122_innerDim, int arg123_a_rows, int arg124_b_cols, value *arg125_a, value *arg126_b, int arg127_idx);

__device__ double gpudevice_fun160_matrixATAfWorker(int arg142_rows, int arg143_cols, value *arg144_a, int arg145_idx)
{
	int var146_innerDim = arg142_rows;
	int var147_outerDim = arg143_cols;
	int var148_row = gpu_divi(arg145_idx, arg143_cols);
	int var149_col = gpu_modi(arg145_idx, arg143_cols);
	int var150_aT_start_offset = var148_row;
	int var151_a_start_offset = var149_col;
	return gpudevice_fun152_dotprod(var147_outerDim, arg144_a, var146_innerDim, 0.0, 0, var150_aT_start_offset, var151_a_start_offset);
}

__device__ double gpudevice_fun152_dotprod(int arg159_outerDim, value *arg158_a, int arg157_innerDim, double arg153_acc, int arg154_p, int arg155_aT_offset, int arg156_a_offset)
{
	return (gpu_eqi(arg154_p, arg157_innerDim)) ? (arg153_acc) : (gpudevice_fun152_dotprod(arg159_outerDim, arg158_a, arg157_innerDim, gpu_addf(arg153_acc, gpu_mulf(*((double *) (arg158_a[arg155_aT_offset])), *((double *) (arg158_a[arg156_a_offset])))), gpu_addi(arg154_p, 1), gpu_addi(arg155_aT_offset, arg159_outerDim), gpu_addi(arg156_a_offset, arg159_outerDim)));
}

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__device__ inline double gpu_mulf(double x, double y) {return x * y;}

__device__ inline double gpu_addf(double x, double y) {return x + y;}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ double gpudevice_fun132_dotprod(int arg140_b_cols, value *arg139_b, value *arg138_a, int arg137_innerDim, double arg133_acc, int arg134_p, int arg135_a_offset, int arg136_b_offset)
{
	return (gpu_eqi(arg134_p, arg137_innerDim)) ? (arg133_acc) : (gpudevice_fun132_dotprod(arg140_b_cols, arg139_b, arg138_a, arg137_innerDim, gpu_addf(arg133_acc, gpu_mulf(*((double *) (arg138_a[arg135_a_offset])), *((double *) (arg139_b[arg136_b_offset])))), gpu_addi(arg134_p, 1), gpu_addi(arg135_a_offset, 1), gpu_addi(arg136_b_offset, arg140_b_cols)));
}

__device__ inline int gpu_muli(int x, int y) {return x * y;}

__device__ inline int gpu_modi(int x, int y) {return x % y;}

__device__ inline int gpu_divi(int x, int y) {return x / y;}

__device__ double gpudevice_fun141_matrixMulfWorker(int arg122_innerDim, int arg123_a_rows, int arg124_b_cols, value *arg125_a, value *arg126_b, int arg127_idx)
{
	int var128_row = gpu_divi(arg127_idx, arg124_b_cols);
	int var129_col = gpu_modi(arg127_idx, arg124_b_cols);
	int var130_a_start_offset = gpu_muli(arg122_innerDim, var128_row);
	int var131_b_start_offset = var129_col;
	return gpudevice_fun132_dotprod(arg124_b_cols, arg126_b, arg125_a, arg122_innerDim, 0.0, 0, var130_a_start_offset, var131_b_start_offset);
}

__global__ void gpuglobal_fun160_matrixATAfWorker(int cuda_arg0, int cuda_arg1, value *cuda_arg2, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		((double *) outarr)[i] = gpudevice_fun160_matrixATAfWorker(cuda_arg0, cuda_arg1, cuda_arg2, i);
	}
}

__global__ void gpuglobal_fun141_matrixMulfWorker(int cuda_arg0, int cuda_arg1, int cuda_arg2, value *cuda_arg3, value *cuda_arg4, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		((double *) outarr)[i] = gpudevice_fun141_matrixMulfWorker(cuda_arg0, cuda_arg1, cuda_arg2, cuda_arg3, cuda_arg4, i);
	}
}

value gpuhost_fun160_matrixATAfWorker(value packedInts, value packedFloats, value arg0)
{
	CAMLparam3(packedInts, packedFloats, arg0);
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
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMalloc(&cuda_arg0, Wosize_val(arg0) * sizeof(value));
	cudaMemcpy(cuda_arg0, Op_val(arg0), Wosize_val(arg0) * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_fun160_matrixATAfWorker<<<blockCount,threadsPerBlock>>>(Int_val(Field(packedInts, 1)), Int_val(Field(packedInts, 2)), cuda_arg0, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}

value gpuhost_fun141_matrixMulfWorker(value packedInts, value packedFloats, value arg0, value arg1)
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

	gpuglobal_fun141_matrixMulfWorker<<<blockCount,threadsPerBlock>>>(Int_val(Field(packedInts, 1)), Int_val(Field(packedInts, 2)), Int_val(Field(packedInts, 3)), cuda_arg0, cuda_arg1, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);

	CAMLreturn(outarr);
}