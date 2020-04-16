/**
 * @file matadd.cu
 *
 * CUDA (cuBLAS) implementation of S = A + B
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

void pre_matadd(void);
void run_matadd(void);
void post_matadd(void);

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

static double *A_mat = NULL;
static double *B_mat = NULL;
static double *S_mat = NULL;

int main(void)
{
	BENCHMARK(pre_matadd, run_matadd, post_matadd);
	return 0;
}

void pre_matadd(void)
{
	A_mat = new double[(_SIZE_) * (_SIZE_)];
	B_mat = new double[(_SIZE_) * (_SIZE_)];
	S_mat = new double[(_SIZE_) * (_SIZE_)];

	for (int i = 0; i < _SIZE_; ++i) {
		for (int j = 0; j < _SIZE_; ++j) {
			A_mat[(i * (_SIZE_)) + j] = ((double) ((i * (_SIZE_) + 1) + (j + 1))) / ((double) (_SIZE_));
			B_mat[(i * (_SIZE_)) + j] = ((double) ((i + 1) - (j * (_SIZE_) + 1))) / ((double) (_SIZE_));
		}
	}
}

void run_matadd(void)
{
	double a;
	double b;
	double *d_A;
	double *d_B;
	double *d_S;
	cublasHandle_t handle;

	PERR(cudaMalloc(&d_A, (_SIZE_) * (_SIZE_) * sizeof(double)));
	PERR(cudaMalloc(&d_B, (_SIZE_) * (_SIZE_) * sizeof(double)));
	PERR(cudaMalloc(&d_S, (_SIZE_) * (_SIZE_) * sizeof(double)));
	PSTAT(cublasCreate(&handle));
	PSTAT(cublasSetMatrix(_SIZE_, _SIZE_, sizeof(double), A_mat, _SIZE_, d_A, _SIZE_));
	PSTAT(cublasSetMatrix(_SIZE_, _SIZE_, sizeof(double), B_mat, _SIZE_, d_B, _SIZE_));
	a = 1.0;
	b = 1.0;

	PSTAT(cublasDgeam(handle, CUBLAS_OP_N, CUBLAS_OP_N, _SIZE_, _SIZE_, &a, d_A, _SIZE_, &b, d_B, _SIZE_, d_S, _SIZE_));

	PSTAT(cublasGetMatrix(_SIZE_, _SIZE_, sizeof(double), d_S, _SIZE_, S_mat, _SIZE_));
	cublasDestroy(handle);
	cudaFree(d_A);
	cudaFree(d_B);
	cudaFree(d_S);
}

void post_matadd(void)
{
	delete[] A_mat;
	delete[] B_mat;
	delete[] S_mat;
}
