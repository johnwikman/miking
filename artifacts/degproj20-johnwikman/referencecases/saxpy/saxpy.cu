/**
 * @file saxpy.cu
 *
 * CUDA implementation of S = a*X + Y
 *
 * Note to self: For some reason the cudaMemcpys are twice as slow as when
 * running to MCore version, the kernel time is about the same. Include this in
 * the discussion and run nvprof to look at the kernel times.
 */

#include <cstdlib>
#include <cmath>
#include <iostream>

#include <cuda_runtime.h>
#include "cublas_v2.h"

#include "../benchmark_suite.hpp"

#ifndef _SIZE_
#error The _SIZE_ constant is not defined!
#endif

void pre_saxpy(void);
void run_saxpycublas(void);
void run_saxpyexample(void);
void run_mysaxpy(void);
void post_saxpy(void);

#define PERR(a) p_cudaerr(#a, a)
#define PSTAT(a) p_cublasstat(#a, a)

static void p_cudaerr(const char *msg, cudaError_t err)
{
    if (err != cudaSuccess)
    	std::cerr << "cudaerr: " << msg << ": " << cudaGetErrorString(err) << std::endl;
}

static void p_cublasstat(const char *msg, cublasStatus_t stat)
{
#   define p_case(v) \
	       case v: \
           std::cerr << "cublasstat: " << msg << " returned " << #v << std::endl; \
           break

	switch (stat) {
	p_case(CUBLAS_STATUS_NOT_INITIALIZED);
	p_case(CUBLAS_STATUS_ALLOC_FAILED);
	p_case(CUBLAS_STATUS_INVALID_VALUE);
	p_case(CUBLAS_STATUS_ARCH_MISMATCH);
	p_case(CUBLAS_STATUS_MAPPING_ERROR);
	p_case(CUBLAS_STATUS_EXECUTION_FAILED);
	p_case(CUBLAS_STATUS_INTERNAL_ERROR);
	p_case(CUBLAS_STATUS_NOT_SUPPORTED);
	p_case(CUBLAS_STATUS_LICENSE_ERROR);
	default:
		break;
	}
#undef p_case
}

static double a_scalar;
static double *s_arr = NULL;
static double *x_arr = NULL;
static double *y_arr = NULL;

int main(void)
{
#ifdef USE_CUSTOM
	BENCHMARK(pre_saxpy, run_mysaxpy, post_saxpy);
#elif defined(USE_EXAMPLE)
	BENCHMARK(pre_saxpy, run_saxpyexample, post_saxpy);
#else
	BENCHMARK(pre_saxpy, run_saxpycublas, post_saxpy);
#endif
	return 0;
}

void pre_saxpy(void)
{
	s_arr = new double[_SIZE_];
	x_arr = new double[_SIZE_];
	y_arr = new double[_SIZE_];
	for (int i = 0; i < _SIZE_; ++i) {
		x_arr[i] = ((double) (i % 101)) / (43.8);
		y_arr[i] = ((double) (i % 103)) / (27.5);
	}
	a_scalar = 11.1;
}

void post_saxpy(void)
{
	delete[] s_arr;
	delete[] x_arr;
	delete[] y_arr;
}

void run_saxpycublas(void)
{
	double a;
	double *d_x;
	double *d_y;
	cublasHandle_t handle;

	PERR(cudaMalloc(&d_x, (_SIZE_) * sizeof(double)));
	PERR(cudaMalloc(&d_y, (_SIZE_) * sizeof(double)));
	PSTAT(cublasCreate(&handle));
	PSTAT(cublasSetVector(_SIZE_, sizeof(double), x_arr, 1, d_x, 1));
	PSTAT(cublasSetVector(_SIZE_, sizeof(double), y_arr, 1, d_y, 1));
	a = a_scalar;

	PSTAT(cublasDaxpy(handle, _SIZE_, &a, d_x, 1, d_y, 1));

	PSTAT(cublasGetVector(_SIZE_, sizeof(double), d_y, 1, s_arr, 1));
	cublasDestroy(handle);
	cudaFree(d_x);
	cudaFree(d_y);
}

// Example from: https://devblogs.nvidia.com/six-ways-saxpy/
__global__
void ex_saxpy(int n, double a, double * __restrict x, double * __restrict y)
{
	int i = blockIdx.x*blockDim.x + threadIdx.x;
	if (i < n) y[i] = a*x[i] + y[i];
}

void run_saxpyexample(void)
{
	double a;
	double *d_x;
	double *d_y;
	int threadsPerBlock;
	int numBlocks;

	PERR(cudaDeviceGetAttribute(&threadsPerBlock, cudaDevAttrMaxThreadsPerBlock, 0));
	numBlocks = ((_SIZE_) + threadsPerBlock - 1) / threadsPerBlock;

	PERR(cudaMalloc(&d_x, (_SIZE_) * sizeof(double)));
	PERR(cudaMalloc(&d_y, (_SIZE_) * sizeof(double)));
	PERR(cudaMemcpy(d_x, x_arr, (_SIZE_) * sizeof(double), cudaMemcpyHostToDevice));
	PERR(cudaMemcpy(d_y, y_arr, (_SIZE_) * sizeof(double), cudaMemcpyHostToDevice));
	a = a_scalar;

	ex_saxpy<<<numBlocks,threadsPerBlock>>>(_SIZE_, a, d_x, d_y);
	PERR(cudaDeviceSynchronize());

	PERR(cudaMemcpy(s_arr, d_y, (_SIZE_) * sizeof(double), cudaMemcpyDeviceToHost));
	cudaFree(d_x);
	cudaFree(d_y);
}



// My own implementation
__global__
void my_saxpy(int n, double a, const double *x, const double *y, double *s)
{
	int i = (blockIdx.x * blockDim.x) + threadIdx.x;

	if (i < n)
		s[i] = (a * x[i]) + y[i];
}

void run_mysaxpy(void)
{
	double a;
	double *d_x;
	double *d_y;
	double *d_s;
	int threadsPerBlock;
	int numBlocks;

	PERR(cudaDeviceGetAttribute(&threadsPerBlock, cudaDevAttrMaxThreadsPerBlock, 0));
	numBlocks = ((_SIZE_) + threadsPerBlock - 1) / threadsPerBlock;

	PERR(cudaMalloc(&d_x, (_SIZE_) * sizeof(double)));
	PERR(cudaMalloc(&d_y, (_SIZE_) * sizeof(double)));
	PERR(cudaMalloc(&d_s, (_SIZE_) * sizeof(double)));
	PERR(cudaMemcpy(d_x, x_arr, (_SIZE_) * sizeof(double), cudaMemcpyHostToDevice));
	PERR(cudaMemcpy(d_y, y_arr, (_SIZE_) * sizeof(double), cudaMemcpyHostToDevice));
	a = a_scalar;

	my_saxpy<<<numBlocks,threadsPerBlock>>>(_SIZE_, a, d_x, d_y, d_s);
	PERR(cudaDeviceSynchronize());

	PERR(cudaMemcpy(s_arr, d_s, (_SIZE_) * sizeof(double), cudaMemcpyDeviceToHost));
	cudaFree(d_x);
	cudaFree(d_y);
	cudaFree(d_s);
}
