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
	value gpuhost_saxpy_int_single(value arg0, value arg1, value inarr);
	value gpuhost_saxpy_float_single(value arg0, value arg1, value inarr);
	value gpuhost_id_ignore2nd(value inarr);
	value gpuhost_factidx(value inarr);
	value gpuhost_fibidx(value inarr);
}

__device__ int gpudevice_saxpy_int_single(int x, int y, int a);
__device__ double gpudevice_saxpy_float_single(double x, double y, double a);
__device__ inline double gpu_mulf(double x, double y);
__device__ inline double gpu_addf(double x, double y);
__device__ int gpudevice_id_ignore2nd(int x, int y);
__device__ int gpudevice_factidx(int i, int ignored_arg);
__device__ int gpudevice_factorial(int n);
__device__ inline int gpu_subi(int x, int y);
__device__ inline int gpu_muli(int x, int y);
__device__ int gpudevice_fibidx(int i, int ignored_arg);
__device__ int gpudevice_fib(int n);
__device__ int gpudevice_fib_helper(int i, int n, int prev, int current);
__device__ inline bool gpu_eqi(int x, int y);
__device__ inline int gpu_addi(int x, int y);

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

__device__ int gpudevice_id_ignore2nd(int x, int y)
{
	return x;
}

__device__ int gpudevice_factidx(int i, int ignored_arg)
{
	return gpudevice_factorial(i);
}

__device__ int gpudevice_factorial(int n)
{
	return (gpu_eqi(n, 0)) ? (1) : (gpu_muli(n, gpudevice_factorial(gpu_subi(n, 1))));
}

__device__ inline int gpu_subi(int x, int y) {return x - y;}

__device__ inline int gpu_muli(int x, int y) {return x * y;}

__device__ int gpudevice_fibidx(int i, int ignored_arg)
{
	return gpudevice_fib(i);
}

__device__ int gpudevice_fib(int n)
{
	return gpudevice_fib_helper(0, n, 1, 0);
}

__device__ int gpudevice_fib_helper(int i, int n, int prev, int current)
{
	return (gpu_eqi(i, n)) ? (current) : (gpudevice_fib_helper(gpu_addi(i, 1), n, current, gpu_addi(prev, current)));
}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__global__ void gpuglobal_saxpy_int_single(int arg0, int arg1, value *inarr, value *outarr, int n)
{
	int i;
	int start = threadIdx.x * 32;
	int end = start + 32;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(inarr[i]);
		outarr[i] = Val_int(gpudevice_saxpy_int_single(arg0, arg1, v));
	}
}

__global__ void gpuglobal_saxpy_float_single(double arg0, double arg1, value *inarr, value *outarr, int n)
{
	int i;
	int start = threadIdx.x * 32;
	int end = start + 32;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		double v;
		v = ((double *) inarr)[i];
		((double *) outarr)[i] = gpudevice_saxpy_float_single(arg0, arg1, v);
	}
}

__global__ void gpuglobal_id_ignore2nd(value *inarr, value *outarr, int n)
{
	int i;
	int start = threadIdx.x * 16;
	int end = start + 16;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(inarr[i]);
		outarr[i] = Val_int(gpudevice_id_ignore2nd(i, v));
	}
}

__global__ void gpuglobal_factidx(value *inarr, value *outarr, int n)
{
	int i;
	int start = threadIdx.x * 8;
	int end = start + 8;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(inarr[i]);
		outarr[i] = Val_int(gpudevice_factidx(i, v));
	}
}

__global__ void gpuglobal_fibidx(value *inarr, value *outarr, int n)
{
	int i;
	int start = threadIdx.x * 16;
	int end = start + 16;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		int v;
		v = Int_val(inarr[i]);
		outarr[i] = Val_int(gpudevice_fibidx(i, v));
	}
}

value gpuhost_saxpy_int_single(value arg0, value arg1, value inarr)
{
	CAMLparam3(arg0, arg1, inarr);
	CAMLlocal1(outarr);
	int n = Wosize_val(inarr);

	value *cuda_inarr;
	value *cuda_outarr;
	cudaMalloc(&cuda_inarr, n * sizeof(value));
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMemcpy(cuda_inarr, Op_val(inarr), n * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_saxpy_int_single<<<1,(n + 31) / 32>>>(Int_val(arg0), Int_val(arg1), cuda_inarr, cuda_outarr, n);
	outarr = caml_alloc(n, Tag_val(inarr));
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_inarr);
	cudaFree(cuda_outarr);

	CAMLreturn(outarr);
}

value gpuhost_saxpy_float_single(value arg0, value arg1, value inarr)
{
	CAMLparam3(arg0, arg1, inarr);
	CAMLlocal1(outarr);
	int n = Wosize_val(inarr);

	value *cuda_inarr;
	value *cuda_outarr;
	cudaMalloc(&cuda_inarr, n * sizeof(value));
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMemcpy(cuda_inarr, Op_val(inarr), n * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_saxpy_float_single<<<1,(n + 31) / 32>>>(*((double *) arg0), *((double *) arg1), cuda_inarr, cuda_outarr, n);
	outarr = caml_alloc(n, Tag_val(inarr));
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_inarr);
	cudaFree(cuda_outarr);

	CAMLreturn(outarr);
}

value gpuhost_id_ignore2nd(value inarr)
{
	CAMLparam1(inarr);
	CAMLlocal1(outarr);
	int n = Wosize_val(inarr);

	value *cuda_inarr;
	value *cuda_outarr;
	cudaMalloc(&cuda_inarr, n * sizeof(value));
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMemcpy(cuda_inarr, Op_val(inarr), n * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_id_ignore2nd<<<1,(n + 15) / 16>>>(cuda_inarr, cuda_outarr, n);
	outarr = caml_alloc(n, Tag_val(inarr));
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_inarr);
	cudaFree(cuda_outarr);

	CAMLreturn(outarr);
}

value gpuhost_factidx(value inarr)
{
	CAMLparam1(inarr);
	CAMLlocal1(outarr);
	int n = Wosize_val(inarr);

	value *cuda_inarr;
	value *cuda_outarr;
	cudaMalloc(&cuda_inarr, n * sizeof(value));
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMemcpy(cuda_inarr, Op_val(inarr), n * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_factidx<<<1,(n + 7) / 8>>>(cuda_inarr, cuda_outarr, n);
	outarr = caml_alloc(n, Tag_val(inarr));
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_inarr);
	cudaFree(cuda_outarr);

	CAMLreturn(outarr);
}

value gpuhost_fibidx(value inarr)
{
	CAMLparam1(inarr);
	CAMLlocal1(outarr);
	int n = Wosize_val(inarr);

	value *cuda_inarr;
	value *cuda_outarr;
	cudaMalloc(&cuda_inarr, n * sizeof(value));
	cudaMalloc(&cuda_outarr, n * sizeof(value));
	cudaMemcpy(cuda_inarr, Op_val(inarr), n * sizeof(value), cudaMemcpyHostToDevice);

	gpuglobal_fibidx<<<1,(n + 15) / 16>>>(cuda_inarr, cuda_outarr, n);
	outarr = caml_alloc(n, Tag_val(inarr));
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_inarr);
	cudaFree(cuda_outarr);

	CAMLreturn(outarr);
}