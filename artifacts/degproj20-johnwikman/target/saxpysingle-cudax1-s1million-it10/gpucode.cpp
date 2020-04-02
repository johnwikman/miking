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
	value gpuhost_fun81_saxpy(value packedInts, value packedFloats, value arg0);
}

__device__ double gpudevice_fun81_saxpy(double arg78_a, double arg79_y, double arg80_xelem);
__device__ inline double gpu_mulf(double x, double y);
__device__ inline double gpu_addf(double x, double y);

__device__ double gpudevice_fun81_saxpy(double arg78_a, double arg79_y, double arg80_xelem)
{
	return gpu_addf(gpu_mulf(arg78_a, arg80_xelem), arg79_y);
}

__device__ inline double gpu_mulf(double x, double y) {return x * y;}

__device__ inline double gpu_addf(double x, double y) {return x + y;}

__global__ void gpuglobal_fun81_saxpy(double cuda_arg0, double cuda_arg1, value *cuda_arg2, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		double v;
		v = ((double *) cuda_arg2)[i];
		((double *) outarr)[i] = gpudevice_fun81_saxpy(cuda_arg0, cuda_arg1, v);
	}
}

value gpuhost_fun81_saxpy(value packedInts, value packedFloats, value arg0)
{
	CAMLparam3(packedInts, packedFloats, arg0);
	CAMLlocal1(outarr);

	int elemPerThread = Int_val(Field(packedInts, 0));
	int n = Wosize_val(arg0);

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

	gpuglobal_fun81_saxpy<<<blockCount,threadsPerBlock>>>(Double_field(packedFloats, 0), Double_field(packedFloats, 1), cuda_arg0, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}