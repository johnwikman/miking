/**
 * @file ger.cu
 *
 * CUDA (cuBLAS) implementation of S = x*y^T + A
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

void pre_ger(void);
void run_ger(void);
void post_ger(void);

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

static double *x_vec = NULL;
static double *y_vec = NULL;
static double *A_mat = NULL;
static double *S_mat = NULL;

int main(void)
{
	BENCHMARK(pre_ger, run_ger, post_ger);
	return 0;
}


void pre_ger(void)
{
	x_vec = new double[_SIZE_];
	y_vec = new double[_SIZE_];
	A_mat = new double[(_SIZE_) * (_SIZE_)];
	S_mat = new double[(_SIZE_) * (_SIZE_)];

	for (int i = 0; i < _SIZE_; ++i) {
		x_vec[i] = 1.18 - (1.0 / ((double) (i + 1)));
		y_vec[i] = 0.376 + (3.0 / ((double) (i + 1)));

		for (int j = 0; j < _SIZE_; ++j) {
			A_mat[(i * (_SIZE_)) + j] = ((double) ((i * (_SIZE_) + 1) + (j + 1))) / ((double) (_SIZE_));
		}
	}
}

void run_ger(void)
{
	double a;
	double *d_x;
	double *d_y;
	double *d_A;
	cublasHandle_t handle;

	PERR(cudaMalloc(&d_x, (_SIZE_) * sizeof(double)));
	PERR(cudaMalloc(&d_y, (_SIZE_) * sizeof(double)));
	PERR(cudaMalloc(&d_A, (_SIZE_) * (_SIZE_) * sizeof(double)));
	PSTAT(cublasCreate(&handle));
	PSTAT(cublasSetVector(_SIZE_, sizeof(double), x_vec, 1, d_x, 1));
	PSTAT(cublasSetVector(_SIZE_, sizeof(double), y_vec, 1, d_y, 1));
	PSTAT(cublasSetMatrix(_SIZE_, _SIZE_, sizeof(double), A_mat, _SIZE_, d_A, _SIZE_));
	a = 1.0;

	PSTAT(cublasDger(handle, _SIZE_, _SIZE_, &a, d_x, 1, d_y, 1, d_A, _SIZE_));

	PSTAT(cublasGetMatrix(_SIZE_, _SIZE_, sizeof(double), d_A, _SIZE_, S_mat, _SIZE_));
	cublasDestroy(handle);
	cudaFree(d_x);
	cudaFree(d_y);
	cudaFree(d_A);
}

void post_ger(void)
{
	delete[] x_vec;
	delete[] y_vec;
	delete[] A_mat;
	delete[] S_mat;
}
