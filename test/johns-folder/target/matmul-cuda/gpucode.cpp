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
	value gpuhost_fun124_matrixMuliWorker(value packedInts, value packedFloats, value arg0, value arg1);
}

__device__ inline int gpu_muli(int x, int y);
__device__ inline int gpu_addi(int x, int y);
__device__ inline bool gpu_eqi(int x, int y);
__device__ int gpudevice_fun109_matrixMuliWorkerReduce(int arg110_innerDim, int arg111_b_cols, value *arg112_a, value *arg113_b, int arg114_acc, int arg115_p, int arg116_a_offset, int arg117_b_offset);
__device__ inline int gpu_modi(int x, int y);
__device__ inline int gpu_divi(int x, int y);
__device__ int gpudevice_fun124_matrixMuliWorker(int arg118_innerDim, int arg119_a_rows, int arg120_b_cols, value *arg121_a, value *arg122_b, int arg123_idx);

__device__ inline int gpu_muli(int x, int y) {return x * y;}

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ int gpudevice_fun109_matrixMuliWorkerReduce(int arg110_innerDim, int arg111_b_cols, value *arg112_a, value *arg113_b, int arg114_acc, int arg115_p, int arg116_a_offset, int arg117_b_offset)
{
	return (gpu_eqi(arg115_p, arg110_innerDim)) ? (arg114_acc) : (gpudevice_fun109_matrixMuliWorkerReduce(arg110_innerDim, arg111_b_cols, arg112_a, arg113_b, gpu_addi(arg114_acc, gpu_muli(Int_val((arg112_a[arg116_a_offset])), Int_val((arg113_b[arg117_b_offset])))), gpu_addi(arg115_p, 1), gpu_addi(arg116_a_offset, 1), gpu_addi(arg117_b_offset, arg111_b_cols)));
}

__device__ inline int gpu_modi(int x, int y) {return x % y;}

__device__ inline int gpu_divi(int x, int y) {return x / y;}

__device__ int gpudevice_fun124_matrixMuliWorker(int arg118_innerDim, int arg119_a_rows, int arg120_b_cols, value *arg121_a, value *arg122_b, int arg123_idx)
{
	return gpudevice_fun109_matrixMuliWorkerReduce(arg118_innerDim, arg120_b_cols, arg121_a, arg122_b, 0, 0, gpu_muli(arg118_innerDim, gpu_divi(arg123_idx, arg120_b_cols)), gpu_modi(arg123_idx, arg120_b_cols));
}

__global__ void gpuglobal_fun124_matrixMuliWorker(int cuda_arg0, int cuda_arg1, int cuda_arg2, value *cuda_arg3, value *cuda_arg4, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		outarr[i] = Val_int(gpudevice_fun124_matrixMuliWorker(cuda_arg0, cuda_arg1, cuda_arg2, cuda_arg3, cuda_arg4, i));
	}
}

value gpuhost_fun124_matrixMuliWorker(value packedInts, value packedFloats, value arg0, value arg1)
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

	gpuglobal_fun124_matrixMuliWorker<<<blockCount,threadsPerBlock>>>(Int_val(Field(packedInts, 1)), Int_val(Field(packedInts, 2)), Int_val(Field(packedInts, 3)), cuda_arg0, cuda_arg1, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);

	CAMLreturn(outarr);
}