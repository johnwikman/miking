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
	value gpuhost_saxpy_int_single(value arg0, value arg1, value arg2);
	value gpuhost_saxpy_float_single(value arg0, value arg1, value arg2);
	value gpuhost_saxpy_int_mapfull(value arg0, value arg1, value arg2);
	value gpuhost_id_ignore2nd(value arg0);
	value gpuhost_id2f_ignore2nd(value arg0);
	value gpuhost_factidx(value arg0);
}

__device__ int gpudevice_saxpy_int_single(int x, int y, int a);
__device__ double gpudevice_saxpy_float_single(double x, double y, double a);
__device__ inline double gpu_mulf(double x, double y);
__device__ inline double gpu_addf(double x, double y);
__device__ int gpudevice_saxpy_int_mapfull(int a, value *y, int i, int x);
__device__ inline int gpu_addi(int x, int y);
__device__ int gpudevice_id_ignore2nd(int x, int y);
__device__ double gpudevice_id2f_ignore2nd(int x, int y);
__device__ inline double gpu_int2float(int x);
__device__ int gpudevice_factidx(int i, int ignored_arg);
__device__ int gpudevice_factorial(int n);
__device__ inline bool gpu_eqi(int x, int y);
__device__ inline int gpu_subi(int x, int y);
__device__ inline int gpu_muli(int x, int y);

__device__ int gpudevice_saxpy_int_single(int x, int y, int a)
{
	return gpu_addi(gpu_muli(a, x), y);
}

__device__ double gpudevice_saxpy_float_single(double x, double y, double a)
{
	return gpu_addf(gpu_mulf(a, x), y);
}

__device__ inline double gpu_mulf(double x, double y) {return x * y;}

__device__ inline double gpu_addf(double x, double y) {return x + y;}

__device__ int gpudevice_saxpy_int_mapfull(int a, value *y, int i, int x)
{
	return gpu_addi(gpu_muli(a, x), Int_val((y[i])));
}

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__device__ int gpudevice_id_ignore2nd(int x, int y)
{
	return x;
}

__device__ double gpudevice_id2f_ignore2nd(int x, int y)
{
	return gpu_int2float(x);
}

__device__ inline double gpu_int2float(int x) {return (double) x;}

__device__ int gpudevice_factidx(int i, int ignored_arg)
{
	return gpudevice_factorial(i);
}

__device__ int gpudevice_factorial(int n)
{
	return (gpu_eqi(n, 0)) ? (1) : (gpu_muli(n, gpudevice_factorial(gpu_subi(n, 1))));
}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ inline int gpu_subi(int x, int y) {return x - y;}

__device__ inline int gpu_muli(int x, int y) {return x * y;}

__global__ void gpuglobal_saxpy_int_single(int arg0, int arg1, value *arg2, value *outarr, int n)
{
	int i;
	int start = threadIdx.x * 32;
	int end = start + 32;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(arg2[i]);
		outarr[i] = Val_int(gpudevice_saxpy_int_single(arg0, arg1, v));
	}
}

__global__ void gpuglobal_saxpy_float_single(double arg0, double arg1, value *arg2, value *outarr, int n)
{
	int i;
	int start = threadIdx.x * 512;
	int end = start + 512;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		double v;
		v = ((double *) arg2)[i];
		((double *) outarr)[i] = gpudevice_saxpy_float_single(arg0, arg1, v);
	}
}

__global__ void gpuglobal_saxpy_int_mapfull(int arg0, value *arg1, value *arg2, value *outarr, int n)
{
	int i;
	int start = threadIdx.x * 512;
	int end = start + 512;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(arg2[i]);
		outarr[i] = Val_int(gpudevice_saxpy_int_mapfull(arg0, arg1, i, v));
	}
}

__global__ void gpuglobal_id_ignore2nd(value *arg0, value *outarr, int n)
{
	int i;
	int start = threadIdx.x * 16;
	int end = start + 16;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(arg0[i]);
		outarr[i] = Val_int(gpudevice_id_ignore2nd(i, v));
	}
}

__global__ void gpuglobal_id2f_ignore2nd(value *arg0, value *outarr, int n)
{
	int i;
	int start = threadIdx.x * 512;
	int end = start + 512;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(arg0[i]);
		((double *) outarr)[i] = gpudevice_id2f_ignore2nd(i, v);
	}
}

__global__ void gpuglobal_factidx(value *arg0, value *outarr, int n)
{
	int i;
	int start = threadIdx.x * 8;
	int end = start + 8;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(arg0[i]);
		outarr[i] = Val_int(gpudevice_factidx(i, v));
	}
}

value gpuhost_saxpy_int_single(value arg0, value arg1, value arg2)
{
	CAMLparam3(arg0, arg1, arg2);
	CAMLlocal1(outarr);
	int n = Wosize_val(arg2);

	value *cuda_outarr;
	value *cuda_arg2;
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMalloc(&cuda_arg2, Wosize_val(arg2) * sizeof(value));
	cudaMemcpy(cuda_arg2, Op_val(arg2), Wosize_val(arg2) * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_saxpy_int_single<<<1,(n + 31) / 32>>>(Int_val(arg0), Int_val(arg1), cuda_arg2, cuda_outarr, n);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg2);

	CAMLreturn(outarr);
}

value gpuhost_saxpy_float_single(value arg0, value arg1, value arg2)
{
	CAMLparam3(arg0, arg1, arg2);
	CAMLlocal1(outarr);
	int n = Wosize_val(arg2);

	value *cuda_outarr;
	value *cuda_arg2;
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMalloc(&cuda_arg2, Wosize_val(arg2) * sizeof(value));
	cudaMemcpy(cuda_arg2, Op_val(arg2), Wosize_val(arg2) * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_saxpy_float_single<<<1,(n + 511) / 512>>>(*((double *) arg0), *((double *) arg1), cuda_arg2, cuda_outarr, n);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg2);

	CAMLreturn(outarr);
}

value gpuhost_saxpy_int_mapfull(value arg0, value arg1, value arg2)
{
	CAMLparam3(arg0, arg1, arg2);
	CAMLlocal1(outarr);
	int n = Wosize_val(arg2);

	value *cuda_outarr;
	value *cuda_arg1;
	value *cuda_arg2;
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMalloc(&cuda_arg1, Wosize_val(arg1) * sizeof(value));
	cudaMalloc(&cuda_arg2, Wosize_val(arg2) * sizeof(value));
	cudaMemcpy(cuda_arg1, Op_val(arg1), Wosize_val(arg1) * sizeof(value), cudaMemcpyHostToDevice);
	cudaMemcpy(cuda_arg2, Op_val(arg2), Wosize_val(arg2) * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_saxpy_int_mapfull<<<1,(n + 511) / 512>>>(Int_val(arg0), cuda_arg1, cuda_arg2, cuda_outarr, n);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg1);
	cudaFree(cuda_arg2);

	CAMLreturn(outarr);
}

value gpuhost_id_ignore2nd(value arg0)
{
	CAMLparam1(arg0);
	CAMLlocal1(outarr);
	int n = Wosize_val(arg0);

	value *cuda_outarr;
	value *cuda_arg0;
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMalloc(&cuda_arg0, Wosize_val(arg0) * sizeof(value));
	cudaMemcpy(cuda_arg0, Op_val(arg0), Wosize_val(arg0) * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_id_ignore2nd<<<1,(n + 15) / 16>>>(cuda_arg0, cuda_outarr, n);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}

value gpuhost_id2f_ignore2nd(value arg0)
{
	CAMLparam1(arg0);
	CAMLlocal1(outarr);
	int n = Wosize_val(arg0);

	value *cuda_outarr;
	value *cuda_arg0;
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMalloc(&cuda_arg0, Wosize_val(arg0) * sizeof(value));
	cudaMemcpy(cuda_arg0, Op_val(arg0), Wosize_val(arg0) * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_id2f_ignore2nd<<<1,(n + 511) / 512>>>(cuda_arg0, cuda_outarr, n);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}

value gpuhost_factidx(value arg0)
{
	CAMLparam1(arg0);
	CAMLlocal1(outarr);
	int n = Wosize_val(arg0);

	value *cuda_outarr;
	value *cuda_arg0;
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMalloc(&cuda_arg0, Wosize_val(arg0) * sizeof(value));
	cudaMemcpy(cuda_arg0, Op_val(arg0), Wosize_val(arg0) * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_factidx<<<1,(n + 7) / 8>>>(cuda_arg0, cuda_outarr, n);
	outarr = caml_alloc(n, 0);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}