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
	value gpuhost_fun114_initfun(value packedInts, value packedFloats, value arg0);
	value gpuhost_fun131_initfun(value packedInts, value packedFloats, value arg0);
	value gpuhost_fun242_wmapf(value packedInts, value packedFloats, value arg0);
	value gpuhost_fun237_wmapf(value packedInts, value packedFloats, value arg0);
	value gpuhost_fun232_wmapf(value packedInts, value packedFloats, value arg0, value arg1);
	value gpuhost_fun220_propagate_x(value packedInts, value packedFloats, value arg0, value arg1);
	value gpuhost_fun169_rndinitf(value packedInts, value packedFloats);
	value gpuhost_fun178_wmapf(value packedInts, value packedFloats, value arg0, value arg1);
	value gpuhost_fun184_wmapf(value packedInts, value packedFloats, value arg0);
	value gpuhost_fun190_wmapf(value packedInts, value packedFloats, value arg0);
}

__device__ inline bool gpu_gtf(double x, double y);
__device__ double gpudevice_fun99_maxwork(value *arg100_s, int arg101_i, int arg102_end, double arg103_candidate);
__device__ inline double gpu_negf(double x);
__device__ double gpudevice_fun107_maxwrap(value *arg104_s, int arg105_i, int arg106_end);
__device__ double gpudevice_fun114_initfun(value *arg113_s, int arg111_s_len, int arg108_i);
__device__ inline bool gpu_eqf(double x, double y);
__device__ double gpudevice_fun120_sumwork(value *arg121_s, int arg122_i, int arg123_end, double arg124_acc);
__device__ inline int gpu_muli(int x, int y);
__device__ double gpudevice_fun131_initfun(value *arg130_s, int arg128_s_len, int arg125_i);
__device__ double gpudevice_fun242_wmapf(double arg241_wsum, double arg240_welem);
__device__ double gpudevice_fun237_wmapf(double arg236_wmax, double arg235_welem);
__device__ double gpudevice_fun232_wmapf(double arg231_log_1onN, double arg230_sigma, int arg229_heightMapSize, value *arg228_heightMap, double arg227_altitude, double arg226_ithobs, double arg225_xelem);
__device__ double gpudevice_fun220_propagate_x(curandState_t *randomState, double arg219_sigma, double arg218_velocity, value *arg216_x, int arg214_nPoints, value *arg213_wacc, int arg211__);
__device__ inline int gpu_addi(int x, int y);
__device__ inline bool gpu_ltf(double x, double y);
__device__ inline bool gpu_eqi(int x, int y);
__device__ inline int gpu_divi(int x, int y);
__device__ int gpudevice_fun136_binsearch(value *arg137_vec, double arg138_p, int arg139_low, int arg140_up);
__device__ inline double gpu_randNormalf(curandState_t *r, double mu, double sigma);
__device__ inline double gpu_randUniformf(curandState_t *r, double low, double up);
__device__ double gpudevice_fun169_rndinitf(curandState_t *randomState, double arg168_xUpperBound, double arg167_xLowerBound, int arg166__);
__device__ inline double gpu_logpdfnormalf(double x, double mu, double sigma);
__device__ inline double gpu_addf(double x, double y);
__device__ inline double gpu_mulf(double x, double y);
__device__ inline double gpu_int2float(int x);
__device__ inline bool gpu_or(bool a, bool b);
__device__ inline bool gpu_gti(int x, int y);
__device__ inline int gpu_subi(int x, int y);
__device__ inline bool gpu_lti(int x, int y);
__device__ inline int gpu_ceilfi(double x);
__device__ inline int gpu_floorfi(double x);
__device__ double gpudevice_fun95_g_map(double arg89_altitude, value *arg90_hgtmap, int arg91_mapsize, double arg92_x);
__device__ double gpudevice_fun178_wmapf(double arg177_sigma, int arg176_heightMapSize, value *arg175_heightMap, double arg174_altitude, double arg173_fstobs, double arg172_xelem);
__device__ double gpudevice_fun184_wmapf(double arg183_wmax, double arg182_welem);
__device__ inline double gpu_subf(double x, double y);
__device__ inline double gpu_expf(double x);
__device__ double gpudevice_fun190_wmapf(double arg189_wsum, double arg188_welem);
__device__ inline double gpu_divf(double x, double y);

__device__ inline bool gpu_gtf(double x, double y) {return x > y;}

__device__ double gpudevice_fun99_maxwork(value *arg100_s, int arg101_i, int arg102_end, double arg103_candidate)
{
	return (gpu_eqf(arg101_i, arg102_end)) ? (arg103_candidate) : (gpudevice_fun99_maxwork(arg100_s, gpu_addi(arg101_i, 1), arg102_end, (gpu_gtf((((double *) arg100_s)[arg101_i]), arg103_candidate)) ? ((((double *) arg100_s)[arg101_i])) : (arg103_candidate)));
}

__device__ inline double gpu_negf(double x) {return -x;}

__device__ double gpudevice_fun107_maxwrap(value *arg104_s, int arg105_i, int arg106_end)
{
	return (gpu_eqi(arg105_i, arg106_end)) ? (gpu_negf(1.000000e+300)) : (gpudevice_fun99_maxwork(arg104_s, gpu_addi(arg105_i, 1), arg106_end, (((double *) arg104_s)[0])));
}

__device__ double gpudevice_fun114_initfun(value *arg113_s, int arg111_s_len, int arg108_i)
{
	int var109_start = gpu_muli(arg108_i, 4096);
	int var110_tmp = gpu_addi(var109_start, 4096);
	int var112_end = (gpu_gti(var110_tmp, arg111_s_len)) ? (arg111_s_len) : (var110_tmp);
	return gpudevice_fun107_maxwrap(arg113_s, var109_start, var112_end);
}

__device__ inline bool gpu_eqf(double x, double y) {return x == y;}

__device__ double gpudevice_fun120_sumwork(value *arg121_s, int arg122_i, int arg123_end, double arg124_acc)
{
	return (gpu_eqf(arg122_i, arg123_end)) ? (arg124_acc) : (gpudevice_fun120_sumwork(arg121_s, gpu_addi(arg122_i, 1), arg123_end, gpu_addf(arg124_acc, (((double *) arg121_s)[arg122_i]))));
}

__device__ inline int gpu_muli(int x, int y) {return x * y;}

__device__ double gpudevice_fun131_initfun(value *arg130_s, int arg128_s_len, int arg125_i)
{
	int var126_start = gpu_muli(arg125_i, 4096);
	int var127_tmp = gpu_addi(var126_start, 4096);
	int var129_end = (gpu_gti(var127_tmp, arg128_s_len)) ? (arg128_s_len) : (var127_tmp);
	return gpudevice_fun120_sumwork(arg130_s, var126_start, var129_end, 0.0);
}

__device__ double gpudevice_fun242_wmapf(double arg241_wsum, double arg240_welem)
{
	return gpu_divf(arg240_welem, arg241_wsum);
}

__device__ double gpudevice_fun237_wmapf(double arg236_wmax, double arg235_welem)
{
	return gpu_expf(gpu_subf(arg235_welem, arg236_wmax));
}

__device__ double gpudevice_fun232_wmapf(double arg231_log_1onN, double arg230_sigma, int arg229_heightMapSize, value *arg228_heightMap, double arg227_altitude, double arg226_ithobs, double arg225_xelem)
{
	return gpu_addf(gpu_logpdfnormalf(arg226_ithobs, gpudevice_fun95_g_map(arg227_altitude, arg228_heightMap, arg229_heightMapSize, arg225_xelem), arg230_sigma), arg231_log_1onN);
}

__device__ double gpudevice_fun220_propagate_x(curandState_t *randomState, double arg219_sigma, double arg218_velocity, value *arg216_x, int arg214_nPoints, value *arg213_wacc, int arg211__)
{
	double var212_p = gpu_randUniformf(randomState, 0.0, 1.0e-0);
	int var215_i = gpudevice_fun136_binsearch(arg213_wacc, var212_p, 0, gpu_subi(arg214_nPoints, 1));
	double var217_x_new = (((double *) arg216_x)[var215_i]);
	return gpu_randNormalf(randomState, gpu_addf(var217_x_new, arg218_velocity), arg219_sigma);
}

__device__ inline int gpu_addi(int x, int y) {return x + y;}

__device__ inline bool gpu_ltf(double x, double y) {return x < y;}

__device__ inline bool gpu_eqi(int x, int y) {return x == y;}

__device__ inline int gpu_divi(int x, int y) {return x / y;}

__device__ int gpudevice_fun136_binsearch(value *arg137_vec, double arg138_p, int arg139_low, int arg140_up)
{
	int var141_mid = gpu_divi(gpu_subi(gpu_addi(arg139_low, arg140_up), 1), 2);
	return (gpu_eqi(arg139_low, arg140_up)) ? (arg139_low) : ((gpu_ltf(arg138_p, (((double *) arg137_vec)[var141_mid]))) ? (gpudevice_fun136_binsearch(arg137_vec, arg138_p, arg139_low, var141_mid)) : (gpudevice_fun136_binsearch(arg137_vec, arg138_p, gpu_addi(var141_mid, 1), arg140_up)));
}

__device__ inline double gpu_randNormalf(curandState_t *r, double mu, double sigma) {return mu + ((double) curand_normal(r) * sigma);}

__device__ inline double gpu_randUniformf(curandState_t *r, double low, double up) {return low + ((double) curand_uniform(r) * abs(up - low));}

__device__ double gpudevice_fun169_rndinitf(curandState_t *randomState, double arg168_xUpperBound, double arg167_xLowerBound, int arg166__)
{
	return gpu_randUniformf(randomState, arg167_xLowerBound, arg168_xUpperBound);
}

__device__ inline double gpu_logpdfnormalf(double x, double mu, double sigma) {double t = x - mu; return (-0.5 * t * t / (sigma * sigma)) - log(sigma * sqrt(2.0 * 3.14159265359));}

__device__ inline double gpu_addf(double x, double y) {return x + y;}

__device__ inline double gpu_mulf(double x, double y) {return x * y;}

__device__ inline double gpu_int2float(int x) {return (double) x;}

__device__ inline bool gpu_or(bool a, bool b) {return a || b;}

__device__ inline bool gpu_gti(int x, int y) {return x > y;}

__device__ inline int gpu_subi(int x, int y) {return x - y;}

__device__ inline bool gpu_lti(int x, int y) {return x < y;}

__device__ inline int gpu_ceilfi(double x) {return (int) ceil(x);}

__device__ inline int gpu_floorfi(double x) {return (int) floor(x);}

__device__ double gpudevice_fun95_g_map(double arg89_altitude, value *arg90_hgtmap, int arg91_mapsize, double arg92_x)
{
	int var93_p = gpu_floorfi(arg92_x);
	int var94_n = gpu_ceilfi(arg92_x);
	return (gpu_or(gpu_lti(var93_p, 0), gpu_gti(var94_n, gpu_subi(arg91_mapsize, 1)))) ? (1.0e+5) : (gpu_subf(arg89_altitude, gpu_addf((((double *) arg90_hgtmap)[var93_p]), gpu_mulf(gpu_subf((((double *) arg90_hgtmap)[var94_n]), (((double *) arg90_hgtmap)[var93_p])), gpu_subf(arg92_x, gpu_int2float(var93_p))))));
}

__device__ double gpudevice_fun178_wmapf(double arg177_sigma, int arg176_heightMapSize, value *arg175_heightMap, double arg174_altitude, double arg173_fstobs, double arg172_xelem)
{
	return gpu_logpdfnormalf(arg173_fstobs, gpudevice_fun95_g_map(arg174_altitude, arg175_heightMap, arg176_heightMapSize, arg172_xelem), arg177_sigma);
}

__device__ double gpudevice_fun184_wmapf(double arg183_wmax, double arg182_welem)
{
	return gpu_expf(gpu_subf(arg182_welem, arg183_wmax));
}

__device__ inline double gpu_subf(double x, double y) {return x - y;}

__device__ inline double gpu_expf(double x) {return exp(x);}

__device__ double gpudevice_fun190_wmapf(double arg189_wsum, double arg188_welem)
{
	return gpu_divf(arg188_welem, arg189_wsum);
}

__device__ inline double gpu_divf(double x, double y) {return x / y;}

__global__ void gpuglobal_fun114_initfun(value *cuda_arg0, int cuda_arg1, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		((double *) outarr)[i] = gpudevice_fun114_initfun(cuda_arg0, cuda_arg1, i);
	}
}

__global__ void gpuglobal_fun131_initfun(value *cuda_arg0, int cuda_arg1, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		((double *) outarr)[i] = gpudevice_fun131_initfun(cuda_arg0, cuda_arg1, i);
	}
}

__global__ void gpuglobal_fun242_wmapf(double cuda_arg0, value *cuda_arg1, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		double v;
		v = ((double *) cuda_arg1)[i];
		((double *) outarr)[i] = gpudevice_fun242_wmapf(cuda_arg0, v);
	}
}

__global__ void gpuglobal_fun237_wmapf(double cuda_arg0, value *cuda_arg1, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		double v;
		v = ((double *) cuda_arg1)[i];
		((double *) outarr)[i] = gpudevice_fun237_wmapf(cuda_arg0, v);
	}
}

__global__ void gpuglobal_fun232_wmapf(double cuda_arg0, double cuda_arg1, int cuda_arg2, value *cuda_arg3, double cuda_arg4, double cuda_arg5, value *cuda_arg6, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		double v;
		v = ((double *) cuda_arg6)[i];
		((double *) outarr)[i] = gpudevice_fun232_wmapf(cuda_arg0, cuda_arg1, cuda_arg2, cuda_arg3, cuda_arg4, cuda_arg5, v);
	}
}

__global__ void gpuglobal_fun220_propagate_x(double cuda_arg0, double cuda_arg1, value *cuda_arg2, int cuda_arg3, value *cuda_arg4, value *outarr, int n, int elemPerThread, unsigned long long seed)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	curandState_t r;
	curand_init(seed + (unsigned long long) start, 0, 0, &r);
	for (i = start; i < end; ++i) {
		((double *) outarr)[i] = gpudevice_fun220_propagate_x(&r, cuda_arg0, cuda_arg1, cuda_arg2, cuda_arg3, cuda_arg4, i);
	}
}

__global__ void gpuglobal_fun169_rndinitf(double cuda_arg0, double cuda_arg1, value *outarr, int n, int elemPerThread, unsigned long long seed)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	curandState_t r;
	curand_init(seed + (unsigned long long) start, 0, 0, &r);
	for (i = start; i < end; ++i) {
		((double *) outarr)[i] = gpudevice_fun169_rndinitf(&r, cuda_arg0, cuda_arg1, i);
	}
}

__global__ void gpuglobal_fun178_wmapf(double cuda_arg0, int cuda_arg1, value *cuda_arg2, double cuda_arg3, double cuda_arg4, value *cuda_arg5, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		double v;
		v = ((double *) cuda_arg5)[i];
		((double *) outarr)[i] = gpudevice_fun178_wmapf(cuda_arg0, cuda_arg1, cuda_arg2, cuda_arg3, cuda_arg4, v);
	}
}

__global__ void gpuglobal_fun184_wmapf(double cuda_arg0, value *cuda_arg1, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		double v;
		v = ((double *) cuda_arg1)[i];
		((double *) outarr)[i] = gpudevice_fun184_wmapf(cuda_arg0, v);
	}
}

__global__ void gpuglobal_fun190_wmapf(double cuda_arg0, value *cuda_arg1, value *outarr, int n, int elemPerThread)
{
	int i;
	int start = ((blockIdx.x * blockDim.x) + threadIdx.x) * elemPerThread;
	int end = start + elemPerThread;
	if (end > n)
		end = n;

	for (i = start; i < end; ++i) {
		double v;
		v = ((double *) cuda_arg1)[i];
		((double *) outarr)[i] = gpudevice_fun190_wmapf(cuda_arg0, v);
	}
}

value gpuhost_fun114_initfun(value packedInts, value packedFloats, value arg0)
{
	CAMLparam3(packedInts, packedFloats, arg0);
	CAMLlocal1(outarr);

	int elemPerThread = Int_val(Field(packedInts, 0));
	int n = Int_val(Field(packedInts, 2));

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

	gpuglobal_fun114_initfun<<<blockCount,threadsPerBlock>>>(cuda_arg0, Int_val(Field(packedInts, 1)), cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}

value gpuhost_fun131_initfun(value packedInts, value packedFloats, value arg0)
{
	CAMLparam3(packedInts, packedFloats, arg0);
	CAMLlocal1(outarr);

	int elemPerThread = Int_val(Field(packedInts, 0));
	int n = Int_val(Field(packedInts, 2));

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

	gpuglobal_fun131_initfun<<<blockCount,threadsPerBlock>>>(cuda_arg0, Int_val(Field(packedInts, 1)), cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}

value gpuhost_fun242_wmapf(value packedInts, value packedFloats, value arg0)
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

	gpuglobal_fun242_wmapf<<<blockCount,threadsPerBlock>>>(Double_field(packedFloats, 0), cuda_arg0, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}

value gpuhost_fun237_wmapf(value packedInts, value packedFloats, value arg0)
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

	gpuglobal_fun237_wmapf<<<blockCount,threadsPerBlock>>>(Double_field(packedFloats, 0), cuda_arg0, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}

value gpuhost_fun232_wmapf(value packedInts, value packedFloats, value arg0, value arg1)
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

	gpuglobal_fun232_wmapf<<<blockCount,threadsPerBlock>>>(Double_field(packedFloats, 0), Double_field(packedFloats, 1), Int_val(Field(packedInts, 1)), cuda_arg0, Double_field(packedFloats, 2), Double_field(packedFloats, 3), cuda_arg1, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);

	CAMLreturn(outarr);
}

value gpuhost_fun220_propagate_x(value packedInts, value packedFloats, value arg0, value arg1)
{
	CAMLparam4(packedInts, packedFloats, arg0, arg1);
	CAMLlocal1(outarr);

	int elemPerThread = Int_val(Field(packedInts, 0));
	int n = Int_val(Field(packedInts, 2));

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

	gpuglobal_fun220_propagate_x<<<blockCount,threadsPerBlock>>>(Double_field(packedFloats, 0), Double_field(packedFloats, 1), cuda_arg0, Int_val(Field(packedInts, 1)), cuda_arg1, cuda_outarr, n, elemPerThread, (unsigned long long) clock());
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);

	CAMLreturn(outarr);
}

value gpuhost_fun169_rndinitf(value packedInts, value packedFloats)
{
	CAMLparam2(packedInts, packedFloats);
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
	cudaMalloc(&cuda_outarr, n * sizeof(value));

	gpuglobal_fun169_rndinitf<<<blockCount,threadsPerBlock>>>(Double_field(packedFloats, 0), Double_field(packedFloats, 1), cuda_outarr, n, elemPerThread, (unsigned long long) clock());
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);

	CAMLreturn(outarr);
}

value gpuhost_fun178_wmapf(value packedInts, value packedFloats, value arg0, value arg1)
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

	gpuglobal_fun178_wmapf<<<blockCount,threadsPerBlock>>>(Double_field(packedFloats, 0), Int_val(Field(packedInts, 1)), cuda_arg0, Double_field(packedFloats, 1), Double_field(packedFloats, 2), cuda_arg1, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);
	cudaFree(cuda_arg1);

	CAMLreturn(outarr);
}

value gpuhost_fun184_wmapf(value packedInts, value packedFloats, value arg0)
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

	gpuglobal_fun184_wmapf<<<blockCount,threadsPerBlock>>>(Double_field(packedFloats, 0), cuda_arg0, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}

value gpuhost_fun190_wmapf(value packedInts, value packedFloats, value arg0)
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

	gpuglobal_fun190_wmapf<<<blockCount,threadsPerBlock>>>(Double_field(packedFloats, 0), cuda_arg0, cuda_outarr, n, elemPerThread);
	outarr = caml_alloc(n, Double_array_tag);
	cudaDeviceSynchronize();

	cudaMemcpy(Op_val(outarr), cuda_outarr, n * sizeof(value), cudaMemcpyDeviceToHost);

	cudaFree(cuda_outarr);
	cudaFree(cuda_arg0);

	CAMLreturn(outarr);
}