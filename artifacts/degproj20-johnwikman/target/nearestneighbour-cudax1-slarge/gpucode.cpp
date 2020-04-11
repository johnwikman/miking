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
	value gpuhost_fun85_nnDistance(value packedInts, value packedFloats, value arg0, value arg1);
}

__device__ double gpudevice_fun85_nnDistance(double arg78_x, double arg79_y, value *arg80_pointsX, value *arg81_pointsY, int arg82_i);
__device__ inline double gpu_subf(double x, double y);
__device__ inline double gpu_addf(double x, double y);
__device__ inline double gpu_mulf(double x, double y);
__device__ inline double gpu_sqrtf(double x);

__device__ double gpudevice_fun85_nnDistance(double arg78_x, double arg79_y, value *arg80_pointsX, value *arg81_pointsY, int arg82_i)
{
	double var83_dxi = gpu_subf(arg78_x, (((double *) arg80_pointsX)[arg82_i]));
	double var84_dyi = gpu_subf(arg79_y, (((double *) arg81_pointsY)[arg82_i]));
	return gpu_sqrtf(gpu_addf(gpu_mulf(var83_dxi, var83_dxi), gpu_mulf(var84_dyi, var84_dyi)));
}

__device__ inline double gpu_subf(double x, double y) {return x - y;}

__device__ inline double gpu_addf(double x, double y) {return x + y;}

__device__ inline double gpu_mulf(double x, double y) {return x * y;}

__device__ inline double gpu_sqrtf(double x) {return sqrt(x);}

__global__ void gpuglobal_fun85_nnDistance(double cuda_arg0, double cuda_arg1, value *cuda_arg2, value *cuda_arg3, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		((double *) outarr)[i] = gpudevice_fun85_nnDistance(cuda_arg0, cuda_arg1, cuda_arg2, cuda_arg3, i);
	}
}

value gpuhost_fun85_nnDistance(value packedInts, value packedFloats, value arg0, value arg1)
{
	CAMLparam4(packedInts, packedFloats, arg0, arg1);
	CAMLlocal1(outarr);

	int elemPerThread = Int_val(Field(packedInts, 0));
	int n = Int_val(Field(packedInts, 1));

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

	gpuglobal_fun85_nnDistance<<<blockCount,threadsPerBlock>>>(Double_field(packedFloats, 0), Double_field(packedFloats, 1), cuda_arg0, cuda_arg1, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);

	CAMLreturn(outarr);
}