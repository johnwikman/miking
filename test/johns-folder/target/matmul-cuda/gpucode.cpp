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
	value gpuhost_fun121_matrixMuliWorker(value packedInts, value packedFloats, value arg0, value arg1);
}

__device__ inline int gpu_muli(int x, int y);
__device__ inline int gpu_addi(int x, int y);
__device__ inline bool gpu_eqi(int x, int y);
__device__ int gpudevice_fun112_dotprod(int arg120_b_cols, value *arg119_b, value *arg118_a, int arg117_innerDim, int arg113_acc, int arg114_p, int arg115_a_offset, int arg116_b_offset);
__device__ inline int gpu_modi(int x, int y);
__device__ inline int gpu_divi(int x, int y);
__device__ int gpudevice_fun121_matrixMuliWorker(int arg102_innerDim, int arg103_a_rows, int arg104_b_cols, value *arg105_a, value *arg106_b, int arg107_idx);

__device__ inline int gpu_muli(int x, int y) {return x * y;}

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ int gpudevice_fun112_dotprod(int arg120_b_cols, value *arg119_b, value *arg118_a, int arg117_innerDim, int arg113_acc, int arg114_p, int arg115_a_offset, int arg116_b_offset)
{
	return (gpu_eqi(arg114_p, arg117_innerDim)) ? (arg113_acc) : (gpudevice_fun112_dotprod(arg120_b_cols, arg119_b, arg118_a, arg117_innerDim, gpu_addi(arg113_acc, gpu_muli(Int_val((arg118_a[arg115_a_offset])), Int_val((arg119_b[arg116_b_offset])))), gpu_addi(arg114_p, 1), gpu_addi(arg115_a_offset, 1), gpu_addi(arg116_b_offset, arg120_b_cols)));
}

__device__ inline int gpu_modi(int x, int y) {return x % y;}

__device__ inline int gpu_divi(int x, int y) {return x / y;}

__device__ int gpudevice_fun121_matrixMuliWorker(int arg102_innerDim, int arg103_a_rows, int arg104_b_cols, value *arg105_a, value *arg106_b, int arg107_idx)
{
	int var108_row = gpu_divi(arg107_idx, arg104_b_cols);
	int var109_col = gpu_modi(arg107_idx, arg104_b_cols);
	int var110_a_start_offset = gpu_muli(arg102_innerDim, var108_row);
	int var111_b_start_offset = var109_col;
	return gpudevice_fun112_dotprod(arg104_b_cols, arg106_b, arg105_a, arg102_innerDim, 0, 0, var110_a_start_offset, var111_b_start_offset);
}

__global__ void gpuglobal_fun121_matrixMuliWorker(int cuda_arg0, int cuda_arg1, int cuda_arg2, value *cuda_arg3, value *cuda_arg4, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		outarr[i] = Val_int(gpudevice_fun121_matrixMuliWorker(cuda_arg0, cuda_arg1, cuda_arg2, cuda_arg3, cuda_arg4, i));
	}
}

value gpuhost_fun121_matrixMuliWorker(value packedInts, value packedFloats, value arg0, value arg1)
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

	gpuglobal_fun121_matrixMuliWorker<<<blockCount,threadsPerBlock>>>(Int_val(Field(packedInts, 1)), Int_val(Field(packedInts, 2)), Int_val(Field(packedInts, 3)), cuda_arg0, cuda_arg1, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);

	CAMLreturn(outarr);
}