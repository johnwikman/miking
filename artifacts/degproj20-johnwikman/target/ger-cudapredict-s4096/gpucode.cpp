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
	value gpuhost_fun173_gerWorkerf(value packedInts, value packedFloats, value arg0, value arg1, value arg2);
}

__device__ inline double gpu_addf(double x, double y);
__device__ inline double gpu_mulf(double x, double y);
__device__ inline int gpu_modi(int x, int y);
__device__ inline int gpu_divi(int x, int y);
__device__ double gpudevice_fun173_gerWorkerf(int arg163_rows, int arg164_cols, value *arg165_x, value *arg166_y, int arg167_i, double arg168_Aval);

__device__ inline double gpu_addf(double x, double y) {return x + y;}

__device__ inline double gpu_mulf(double x, double y) {return x * y;}

__device__ inline int gpu_modi(int x, int y) {return x % y;}

__device__ inline int gpu_divi(int x, int y) {return x / y;}

__device__ double gpudevice_fun173_gerWorkerf(int arg163_rows, int arg164_cols, value *arg165_x, value *arg166_y, int arg167_i, double arg168_Aval)
{
	int var169_row = gpu_divi(arg167_i, arg164_cols);
	int var170_col = gpu_modi(arg167_i, arg164_cols);
	double var171_xval = (((double *) arg165_x)[var169_row]);
	double var172_yval = (((double *) arg166_y)[var170_col]);
	return gpu_addf(gpu_mulf(var171_xval, var172_yval), arg168_Aval);
}

__global__ void gpuglobal_fun173_gerWorkerf(int cuda_arg0, int cuda_arg1, value *cuda_arg2, value *cuda_arg3, value *cuda_arg4, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		double v;
		v = ((double *) cuda_arg4)[i];
		((double *) outarr)[i] = gpudevice_fun173_gerWorkerf(cuda_arg0, cuda_arg1, cuda_arg2, cuda_arg3, i, v);
	}
}

value gpuhost_fun173_gerWorkerf(value packedInts, value packedFloats, value arg0, value arg1, value arg2)
{
	CAMLparam5(packedInts, packedFloats, arg0, arg1, arg2);
	CAMLlocal1(outarr);

	int elemPerThread = Int_val(Field(packedInts, 0));
	int n = Wosize_val(arg2);

	int threadsPerBlock;
	int elemPerBlock;
	int blockCount;
	cudaDeviceGetAttribute(&threadsPerBlock, cudaDevAttrMaxThreadsPerBlock, 0);
	elemPerBlock = threadsPerBlock * elemPerThread;
	blockCount = (n + elemPerBlock - 1) / elemPerBlock;
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

	gpuglobal_fun173_gerWorkerf<<<blockCount,threadsPerBlock>>>(Int_val(Field(packedInts, 1)), Int_val(Field(packedInts, 2)), cuda_arg0, cuda_arg1, cuda_arg2, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);
	cudaFree(cuda_arg2);

	CAMLreturn(outarr);
}