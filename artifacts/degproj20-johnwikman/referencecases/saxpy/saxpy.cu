/**
 * @file saxpy.cu
 *
 * CUDA implementation of S = a*X + Y
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
void run_saxpy(void);
void post_saxpy(void);

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
	BENCHMARK(pre_saxpy, run_saxpy, post_saxpy);
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

void run_saxpy(void)
{
#define PERR(a) p_cudaerr(#a, a)
#define PSTAT(a) p_cublasstat(#a, a)
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
