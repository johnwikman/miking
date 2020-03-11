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
	value gpuhost_matrixMuliWorker(value arg0, value arg1, value arg2, value arg3);
}

__device__ inline int gpu_muli(int x, int y);
__device__ inline int gpu_addi(int x, int y);
__device__ inline bool gpu_eqi(int x, int y);
__device__ int gpudevice_matrixMuliWorkerReduce(int innerDim, int b_cols, value *a, value *b, int acc, int p, int a_offset, int b_offset);
__device__ inline int gpu_modi(int x, int y);
__device__ inline int gpu_divi(int x, int y);
__device__ int gpudevice_matrixMuliWorker(value *innerDim__a_rows__b_cols, value *a, value *b, int idx);

__device__ inline int gpu_muli(int x, int y) {return x * y;}

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ int gpudevice_matrixMuliWorkerReduce(int innerDim, int b_cols, value *a, value *b, int acc, int p, int a_offset, int b_offset)
{
	return (gpu_eqi(p, innerDim)) ? (acc) : (gpudevice_matrixMuliWorkerReduce(innerDim, b_cols, a, b, gpu_addi(acc, gpu_muli(Int_val((a[a_offset])), Int_val((b[b_offset])))), gpu_addi(p, 1), gpu_addi(a_offset, 1), gpu_addi(b_offset, b_cols)));
}

__device__ inline int gpu_modi(int x, int y) {return x % y;}

__device__ inline int gpu_divi(int x, int y) {return x / y;}

__device__ int gpudevice_matrixMuliWorker(value *innerDim__a_rows__b_cols, value *a, value *b, int idx)
{
	return gpudevice_matrixMuliWorkerReduce(Int_val((innerDim__a_rows__b_cols[0])), Int_val((innerDim__a_rows__b_cols[2])), a, b, 0, 0, gpu_muli(Int_val((innerDim__a_rows__b_cols[0])), gpu_divi(idx, Int_val((innerDim__a_rows__b_cols[2])))), gpu_modi(idx, Int_val((innerDim__a_rows__b_cols[2]))));
}

__global__ void gpuglobal_matrixMuliWorker(value *arg0, value *arg1, value *arg2, value *outarr, int n)
{
	int i;
	int start = threadIdx.x * 32;
	int end = start + 32;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		outarr[i] = Val_int(gpudevice_matrixMuliWorker(arg0, arg1, arg2, i));
	}
}

value gpuhost_matrixMuliWorker(value arg0, value arg1, value arg2, value arg3)
{
	CAMLparam4(arg0, arg1, arg2, arg3);
	CAMLlocal1(outarr);
	int n = Int_val(arg3);

	value *cuda_outarr;
	value *cuda_arg0;
	value *cuda_arg1;
	value *cuda_arg2;
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMalloc(&cuda_arg0, Wosize_val(arg0) * sizeof(value));
	cudaMalloc(&cuda_arg1, Wosize_val(arg1) * sizeof(value));
	cudaMalloc(&cuda_arg2, Wosize_val(arg2) * sizeof(value));
	cudaMemcpy(cuda_arg0, Op_val(arg0), Wosize_val(arg0) * sizeof(value), cudaMemcpyHostToDevice);
	cudaMemcpy(cuda_arg1, Op_val(arg1), Wosize_val(arg1) * sizeof(value), cudaMemcpyHostToDevice);
	cudaMemcpy(cuda_arg2, Op_val(arg2), Wosize_val(arg2) * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_matrixMuliWorker<<<1,(n + 31) / 32>>>(cuda_arg0, cuda_arg1, cuda_arg2, cuda_outarr, n);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);
	cudaFree(cuda_arg2);

	CAMLreturn(outarr);
}