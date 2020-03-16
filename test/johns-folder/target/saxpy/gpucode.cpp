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
	value gpuhost_fun62_saxpy_int(value packedInts, value packedFloats, value arg0);
	value gpuhost_fun77_id2f_ignore2nd(value packedInts, value packedFloats, value arg0);
	value gpuhost_fun66_saxpy_float(value packedInts, value packedFloats, value arg0);
	value gpuhost_fun71_saxpy_intseq(value packedInts, value packedFloats, value arg0, value arg1);
}

__device__ int gpudevice_fun62_saxpy_int(int arg59_x, int arg60_y, int arg61_a);
__device__ double gpudevice_fun77_id2f_ignore2nd(int arg75_x, int arg76_y);
__device__ inline double gpu_int2float(int x);
__device__ double gpudevice_fun66_saxpy_float(double arg63_x, double arg64_y, double arg65_a);
__device__ inline double gpu_mulf(double x, double y);
__device__ inline double gpu_addf(double x, double y);
__device__ inline int gpu_addi(int x, int y);
__device__ inline int gpu_muli(int x, int y);
__device__ int gpudevice_fun71_saxpy_intseq(int arg67_a, value *arg68_y, int arg69_i, int arg70_x);

__device__ int gpudevice_fun62_saxpy_int(int arg59_x, int arg60_y, int arg61_a)
{
	return gpu_addi(gpu_muli(arg61_a, arg59_x), arg60_y);
}

__device__ double gpudevice_fun77_id2f_ignore2nd(int arg75_x, int arg76_y)
{
	return gpu_int2float(arg75_x);
}

__device__ inline double gpu_int2float(int x) {return (double) x;}

__device__ double gpudevice_fun66_saxpy_float(double arg63_x, double arg64_y, double arg65_a)
{
	return gpu_addf(gpu_mulf(arg65_a, arg63_x), arg64_y);
}

__device__ inline double gpu_mulf(double x, double y) {return x * y;}

__device__ inline double gpu_addf(double x, double y) {return x + y;}

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__device__ inline int gpu_muli(int x, int y) {return x * y;}

__device__ int gpudevice_fun71_saxpy_intseq(int arg67_a, value *arg68_y, int arg69_i, int arg70_x)
{
	return gpu_addi(gpu_muli(arg67_a, arg70_x), Int_val((arg68_y[arg69_i])));
}

__global__ void gpuglobal_fun62_saxpy_int(int cuda_arg0, int cuda_arg1, value *cuda_arg2, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(cuda_arg2[i]);
		outarr[i] = Val_int(gpudevice_fun62_saxpy_int(cuda_arg0, cuda_arg1, v));
	}
}

__global__ void gpuglobal_fun77_id2f_ignore2nd(value *cuda_arg0, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(cuda_arg0[i]);
		((double *) outarr)[i] = gpudevice_fun77_id2f_ignore2nd(i, v);
	}
}

__global__ void gpuglobal_fun66_saxpy_float(double cuda_arg0, double cuda_arg1, value *cuda_arg2, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		double v;
		v = ((double *) cuda_arg2)[i];
		((double *) outarr)[i] = gpudevice_fun66_saxpy_float(cuda_arg0, cuda_arg1, v);
	}
}

__global__ void gpuglobal_fun71_saxpy_intseq(int cuda_arg0, value *cuda_arg1, value *cuda_arg2, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(cuda_arg2[i]);
		outarr[i] = Val_int(gpudevice_fun71_saxpy_intseq(cuda_arg0, cuda_arg1, i, v));
	}
}

value gpuhost_fun62_saxpy_int(value packedInts, value packedFloats, value arg0)
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

	gpuglobal_fun62_saxpy_int<<<blockCount,threadsPerBlock>>>(Int_val(Field(packedInts, 1)), Int_val(Field(packedInts, 2)), cuda_arg0, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}

value gpuhost_fun77_id2f_ignore2nd(value packedInts, value packedFloats, value arg0)
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

	gpuglobal_fun77_id2f_ignore2nd<<<blockCount,threadsPerBlock>>>(cuda_arg0, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}

value gpuhost_fun66_saxpy_float(value packedInts, value packedFloats, value arg0)
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

	gpuglobal_fun66_saxpy_float<<<blockCount,threadsPerBlock>>>(Double_field(packedFloats, 0), Double_field(packedFloats, 1), cuda_arg0, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}

value gpuhost_fun71_saxpy_intseq(value packedInts, value packedFloats, value arg0, value arg1)
{
	CAMLparam4(packedInts, packedFloats, arg0, arg1);
	CAMLlocal1(outarr);

	int elemPerThread = Int_val(Field(packedInts, 0));
	int n = Wosize_val(arg1);

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

	gpuglobal_fun71_saxpy_intseq<<<blockCount,threadsPerBlock>>>(Int_val(Field(packedInts, 1)), cuda_arg0, cuda_arg1, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);

	CAMLreturn(outarr);
}