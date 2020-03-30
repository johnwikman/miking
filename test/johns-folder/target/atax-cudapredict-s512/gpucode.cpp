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
	value gpuhost_fun150_matrixATAWorker(value packedInts, value packedFloats, value arg0);
	value gpuhost_fun131_matrixMuliWorker(value packedInts, value packedFloats, value arg0, value arg1);
}

__device__ int gpudevice_fun150_matrixATAWorker(int arg132_rows, int arg133_cols, value *arg134_a, int arg135_idx);
__device__ int gpudevice_fun142_dotprod(int arg149_outerDim, value *arg148_a, int arg147_innerDim, int arg143_acc, int arg144_p, int arg145_aT_offset, int arg146_a_offset);
__device__ inline int gpu_muli(int x, int y);
__device__ inline int gpu_addi(int x, int y);
__device__ inline bool gpu_eqi(int x, int y);
__device__ int gpudevice_fun122_dotprod(int arg130_b_cols, value *arg129_b, value *arg128_a, int arg127_innerDim, int arg123_acc, int arg124_p, int arg125_a_offset, int arg126_b_offset);
__device__ inline int gpu_modi(int x, int y);
__device__ inline int gpu_divi(int x, int y);
__device__ int gpudevice_fun131_matrixMuliWorker(int arg112_innerDim, int arg113_a_rows, int arg114_b_cols, value *arg115_a, value *arg116_b, int arg117_idx);

__device__ int gpudevice_fun150_matrixATAWorker(int arg132_rows, int arg133_cols, value *arg134_a, int arg135_idx)
{
	int var136_innerDim = arg132_rows;
	int var137_outerDim = arg133_cols;
	int var138_row = gpu_divi(arg135_idx, arg133_cols);
	int var139_col = gpu_modi(arg135_idx, arg133_cols);
	int var140_aT_start_offset = var138_row;
	int var141_a_start_offset = var139_col;
	return gpudevice_fun142_dotprod(var137_outerDim, arg134_a, var136_innerDim, 0, 0, var140_aT_start_offset, var141_a_start_offset);
}

__device__ int gpudevice_fun142_dotprod(int arg149_outerDim, value *arg148_a, int arg147_innerDim, int arg143_acc, int arg144_p, int arg145_aT_offset, int arg146_a_offset)
{
	return (gpu_eqi(arg144_p, arg147_innerDim)) ? (arg143_acc) : (gpudevice_fun142_dotprod(arg149_outerDim, arg148_a, arg147_innerDim, gpu_addi(arg143_acc, gpu_muli(Int_val((arg148_a[arg145_aT_offset])), Int_val((arg148_a[arg146_a_offset])))), gpu_addi(arg144_p, 1), gpu_addi(arg145_aT_offset, arg149_outerDim), gpu_addi(arg146_a_offset, arg149_outerDim)));
}

__device__ inline int gpu_muli(int x, int y) {return x * y;}

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ int gpudevice_fun122_dotprod(int arg130_b_cols, value *arg129_b, value *arg128_a, int arg127_innerDim, int arg123_acc, int arg124_p, int arg125_a_offset, int arg126_b_offset)
{
	return (gpu_eqi(arg124_p, arg127_innerDim)) ? (arg123_acc) : (gpudevice_fun122_dotprod(arg130_b_cols, arg129_b, arg128_a, arg127_innerDim, gpu_addi(arg123_acc, gpu_muli(Int_val((arg128_a[arg125_a_offset])), Int_val((arg129_b[arg126_b_offset])))), gpu_addi(arg124_p, 1), gpu_addi(arg125_a_offset, 1), gpu_addi(arg126_b_offset, arg130_b_cols)));
}

__device__ inline int gpu_modi(int x, int y) {return x % y;}

__device__ inline int gpu_divi(int x, int y) {return x / y;}

__device__ int gpudevice_fun131_matrixMuliWorker(int arg112_innerDim, int arg113_a_rows, int arg114_b_cols, value *arg115_a, value *arg116_b, int arg117_idx)
{
	int var118_row = gpu_divi(arg117_idx, arg114_b_cols);
	int var119_col = gpu_modi(arg117_idx, arg114_b_cols);
	int var120_a_start_offset = gpu_muli(arg112_innerDim, var118_row);
	int var121_b_start_offset = var119_col;
	return gpudevice_fun122_dotprod(arg114_b_cols, arg116_b, arg115_a, arg112_innerDim, 0, 0, var120_a_start_offset, var121_b_start_offset);
}

__global__ void gpuglobal_fun150_matrixATAWorker(int cuda_arg0, int cuda_arg1, value *cuda_arg2, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		outarr[i] = Val_int(gpudevice_fun150_matrixATAWorker(cuda_arg0, cuda_arg1, cuda_arg2, i));
	}
}

__global__ void gpuglobal_fun131_matrixMuliWorker(int cuda_arg0, int cuda_arg1, int cuda_arg2, value *cuda_arg3, value *cuda_arg4, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		outarr[i] = Val_int(gpudevice_fun131_matrixMuliWorker(cuda_arg0, cuda_arg1, cuda_arg2, cuda_arg3, cuda_arg4, i));
	}
}

value gpuhost_fun150_matrixATAWorker(value packedInts, value packedFloats, value arg0)
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

	gpuglobal_fun150_matrixATAWorker<<<blockCount,threadsPerBlock>>>(Int_val(Field(packedInts, 1)), Int_val(Field(packedInts, 2)), cuda_arg0, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}

value gpuhost_fun131_matrixMuliWorker(value packedInts, value packedFloats, value arg0, value arg1)
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

	gpuglobal_fun131_matrixMuliWorker<<<blockCount,threadsPerBlock>>>(Int_val(Field(packedInts, 1)), Int_val(Field(packedInts, 2)), Int_val(Field(packedInts, 3)), cuda_arg0, cuda_arg1, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);

	CAMLreturn(outarr);
}