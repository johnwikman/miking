/**
 * @file saxpysingle.cu
 *
 * CUDA implementation of S = a*X + y
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

void pre_saxpysingle(void);
void run_saxpysinglecublas(void);
void run_mysaxpysingle(void);
void post_saxpysingle(void);

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
static double y_value;
static double *s_arr = NULL;
static double *x_arr = NULL;

int main(void)
{
	std::cout << "[<<<< BENCHMARKING CUBLAS IMPLEMENTATION >>>>]" << std::endl;
	BENCHMARK(pre_saxpysingle, run_saxpysinglecublas, post_saxpysingle);
	std::cout << "[<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>]" << std::endl;
	std::cout << std::endl << std::endl;
	std::cout << "[<<<< BENCHMARKING MY OWN IMPLEMENTATION >>>>]" << std::endl;
	BENCHMARK(pre_saxpysingle, run_mysaxpysingle, post_saxpysingle);
	std::cout << "[<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>]" << std::endl;
	return 0;
}

void pre_saxpysingle(void)
{
	s_arr = new double[_SIZE_];
	x_arr = new double[_SIZE_];
	for (int i = 0; i < _SIZE_; ++i) {
		x_arr[i] = ((double) (i % 101)) / (43.8);
	}
	a_scalar = 11.1;
	y_value = 103.5;
}

void post_saxpysingle(void)
{
	delete[] s_arr;
	delete[] x_arr;
}

void run_saxpysinglecublas(void)
{
	double a;
	double *d_x;
	double *d_y;
	cublasHandle_t handle;

	PERR(cudaMalloc(&d_x, (_SIZE_) * sizeof(double)));
	PERR(cudaMalloc(&d_y, sizeof(double)));
	PSTAT(cublasCreate(&handle));
	PSTAT(cublasSetVector(_SIZE_, sizeof(double), x_arr, 1, d_x, 1));
	PSTAT(cublasSetVector(1, sizeof(double), &y_value, 1, d_y, 1));
	a = a_scalar;

	PSTAT(cublasDaxpy(handle, _SIZE_, &a, d_y, 0, d_x, 1));

	PSTAT(cublasGetVector(_SIZE_, sizeof(double), d_x, 1, s_arr, 1));
	cublasDestroy(handle);
	cudaFree(d_x);
	cudaFree(d_y);
}



// My own implementation
__global__ void my_saxpysingle(int n, double a, double *x, double y)
{
	int i = (blockIdx.x * blockDim.x) + threadIdx.x;

	if (i < n)
		x[i] = (a * x[i]) + y;
}

void run_mysaxpysingle(void)
{
	double a;
	double y;
	double *d_x;
	int threadsPerBlock;
	int numBlocks;

	PERR(cudaDeviceGetAttribute(&threadsPerBlock, cudaDevAttrMaxThreadsPerBlock, 0));
	numBlocks = ((_SIZE_) + threadsPerBlock - 1) / threadsPerBlock;

	PERR(cudaMalloc(&d_x, (_SIZE_) * sizeof(double)));
	PERR(cudaMemcpy(d_x, x_arr, (_SIZE_) * sizeof(double), cudaMemcpyHostToDevice));
	a = a_scalar;
	y = y_value;

	my_saxpysingle<<<numBlocks,threadsPerBlock>>>(_SIZE_, a, d_x, y);
	PERR(cudaDeviceSynchronize());

	PERR(cudaMemcpy(s_arr, d_x, (_SIZE_) * sizeof(double), cudaMemcpyDeviceToHost));
	cudaFree(d_x);
}
