#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <stdlib.h>

extern "C" {
	value gpuhost_saxpy_int(value arg0, value arg1, value inarr);
}

__device__ int gpu_addi(int x, int y) {return x + y;}

__device__ int gpu_muli(int x, int y) {return x * y;}

__device__ int gpudevice_saxpy_int(int x, int y, int a)
{
	return gpu_addi(gpu_muli(a, x), y);
}

__global__ void gpuglobal_saxpy_int(int arg0, int arg1, value *inarr, value *outarr, int n)
{
	int i;
	int start = threadIdx.x;
	int end = start + 32;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v = Int_val(inarr[i]);
		outarr[i] = Val_int(gpudevice_saxpy_int(arg0, arg1, v));
	}
}

value gpuhost_saxpy_int(value arg0, value arg1, value inarr)
{
	CAMLparam3(arg0, arg1, inarr);
	CAMLlocal1(outarr);
	int n = Wosize_val(inarr);

	value *cuda_inarr;
	value *cuda_outarr;
	cudaMalloc(&cuda_inarr, n * sizeof(value));
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMemcpy(cuda_inarr, Op_val(inarr), n * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_saxpy_int<<<1,(n + 31) / 32>>>(Int_val(arg0), Int_val(arg1), cuda_inarr, cuda_outarr, n);
	outarr = caml_alloc(n, Tag_val(inarr));
	cudaDeviceSynchronize();

	// The outarr must reside on the minor heap for the following cudaMemcpy to succeed.
	// (Possible garbage collector issues otherwise...)
	// (Though should never be any issues since neither the previous or new value are blocks...)
	CAMLassert(Is_young((value) outarr));
	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_inarr);
	cudaFree(cuda_outarr);

	CAMLreturn(outarr);
}