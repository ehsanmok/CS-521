/* hw3
 @author Ehsan MohyedinKermani
*/
#include <stdio.h>
#include <math.h>
#include <cuda.h>
#include <curand_kernel.h>
#include "time_it.h"
#define LOGISTIC 1
#define NORM 2
#define RND 3

static void HandleError( cudaError_t err,
                         const char *file,
                         int line ) {
    if (err != cudaSuccess) {
        printf( "%s in %s at line %d\n", cudaGetErrorString( err ),
                file, line );
        exit( EXIT_FAILURE );
    }
}
#define HANDLE_ERROR( err ) (HandleError( err, __FILE__, __LINE__ ))

// struct for passing arguments through time_it_run to our kernel functions in Q1, Q2
struct kernel_arg {
  float *x, *result;
  uint n, m;
  int nblks, tpb;
};

/* Question 1*/

__global__ void logistic(float *x, uint n, uint m) {
	uint i = blockIdx.x * blockDim.x + threadIdx.x;
	if(i < n) {
		float val = x[i];
		for(int iter = 0; iter < m; iter+=64)
			// unrolling for loop 64 times
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			val = 3.9f * val * (1.0f - val);
			
	}
}

void do_logistic(void *void_args) {
	struct kernel_arg *argk = (struct kernel_arg *)(void_args);
	logistic<<<argk->nblks, argk->tpb>>>(argk->x, argk->n, argk->m);
	HANDLE_ERROR(cudaDeviceSynchronize());
	
	logistic<<<argk->nblks, argk->tpb>>>(argk->x, argk->n, argk->m);
	HANDLE_ERROR(cudaDeviceSynchronize());
	
	logistic<<<argk->nblks, argk->tpb>>>(argk->x, argk->n, argk->m);
	HANDLE_ERROR(cudaDeviceSynchronize());
	
	logistic<<<argk->nblks, argk->tpb>>>(argk->x, argk->n, argk->m);
	HANDLE_ERROR(cudaDeviceSynchronize());
}

void time_logistic(uint n, int m, int tpb, int ntrials) {
	
	int nblks = n / tpb;
	uint size = n * sizeof(float);
	float *x;
	float *dev_x;
	x = (float *)malloc(size);
	// initialize x
	x[0] = 0.123f;
	for(int i = 1; i < n; i++)
		x[i] = 3.9f * x[i - 1] * (1.0f - x[i - 1]);
	
	HANDLE_ERROR(cudaMalloc((void**)(&dev_x), size));
	HANDLE_ERROR(cudaMemcpy(dev_x, x, size, cudaMemcpyHostToDevice));
	
	struct kernel_arg argk;
  	struct time_it_raw *tr = time_it_create(ntrials);
  	struct time_it_stats stats;
	// initialize argk
  	argk.n = n;
  	argk.x = dev_x;
  	argk.m = m;
  	argk.nblks = nblks;
  	argk.tpb = tpb;
  	
  	// run the kernel and report timing info
  	time_it_run(tr, do_logistic, (void *)(&argk));
  	time_it_get_stats(tr, &stats);
  	HANDLE_ERROR(cudaMemcpy(x, dev_x, size, cudaMemcpyDeviceToHost));
  	printf("Time logistic: mean(T) = %10.3e, std(T) = %10.3e\n", stats.mean, stats.std);
  	printf("Number of GFLOPS is %10.3e\n", (3*m*n/stats.mean)/1e9);
  	free(x);
	HANDLE_ERROR(cudaFree(dev_x));
	time_it_free(tr);
}

/* Question 2 */

#define NBLKS 2048
__shared__ float sdata[NBLKS];

__device__ void reduce_sum_dev(float *g_idata, float *g_odata, uint n) {

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    uint tid = threadIdx.x;
    uint i = blockIdx.x * blockDim.x * 2 + threadIdx.x;
    uint gridSize = blockDim.x * 2 * gridDim.x;

    float mySum = 0;

    // we reduce multiple elements per thread.  The number is determined by the
    // number of active thread blocks (via gridDim).  More blocks will result
    // in a larger gridSize and therefore fewer elements per thread
    while (i < n)
    {
        mySum += g_idata[i];
        // ensure we don't read out of bounds
        if (i + blockDim.x < n)
            mySum += g_idata[i+blockDim.x];
        i += gridSize;
    }

    // each thread puts its local sum into shared memory
    sdata[tid] = mySum;
    __syncthreads();


    // do reduction in shared mem
    if (blockDim.x >= 512) { if (tid < 256) { sdata[tid] = mySum = mySum + sdata[tid + 256]; } __syncthreads(); }
    if (blockDim.x >= 256) { if (tid < 128) { sdata[tid] = mySum = mySum + sdata[tid + 128]; } __syncthreads(); }
    if (blockDim.x >= 128) { if (tid <  64) { sdata[tid] = mySum = mySum + sdata[tid +  64]; } __syncthreads(); }

    if (tid < 32)
    {
        // now that we are using warp-synchronous programming (below)
        // we need to declare our shared memory volatile so that the compiler
        // doesn't reorder stores to it and induce incorrect behavior.
        volatile float* smem = sdata;
        if (blockDim.x >=  64) { smem[tid] = mySum = mySum + smem[tid + 32]; }
        if (blockDim.x >=  32) { smem[tid] = mySum = mySum + smem[tid + 16]; }
        if (blockDim.x >=  16) { smem[tid] = mySum = mySum + smem[tid +  8]; }
        if (blockDim.x >=   8) { smem[tid] = mySum = mySum + smem[tid +  4]; }
        if (blockDim.x >=   4) { smem[tid] = mySum = mySum + smem[tid +  2]; }
        if (blockDim.x >=   2) { smem[tid] = mySum = mySum + smem[tid +  1]; }
    }

    // write result for this block to global mem
    if (tid == 0)
        g_odata[blockIdx.x] = sdata[0];
}


__global__ void norm(float *x, float *result, uint n) {
	uint i = blockDim.x * blockIdx.x + threadIdx.x;
	if(i < n)
		x[i] = x[i] * x[i];
	reduce_sum_dev(x, result, n);
}

void do_norm(void *void_args) {
	struct kernel_arg *argk = (struct kernel_arg *)(void_args);
	norm<<<(argk->nblks + argk->tpb - 1)/argk->tpb, argk->tpb>>>(argk->x, argk->result, argk->n);
	HANDLE_ERROR(cudaDeviceSynchronize());
}

void mem_norm(uint n, uint nblks, uint tpb, int ntrials) {
	
	uint size = n * sizeof(float);
	float *x, *result;
	float *dev_x, *dev_result;
	x = (float *)malloc(size);
	result = (float *)malloc(size);
	
	x[0] = 0.123f;
	for(int i = 1; i < n; i++)
		x[i] = x[0];
	
	HANDLE_ERROR(cudaMalloc((void**)(&dev_x), size));
	HANDLE_ERROR(cudaMalloc((void**)(&dev_result), size));
	HANDLE_ERROR(cudaMemcpy(dev_x, x, size, cudaMemcpyHostToDevice));
	HANDLE_ERROR(cudaMemcpy(dev_result, result, size, cudaMemcpyHostToDevice));
	struct kernel_arg argk;
  	struct time_it_raw *tr = time_it_create(ntrials);
  	struct time_it_stats stats;
	// initialize argk
  	argk.n = n;
  	argk.x = dev_x;
  	argk.result = dev_result;
  	argk.nblks = nblks;
  	argk.tpb = tpb;
  	
  	// run the kernel and report timing info
  	time_it_run(tr, do_norm, (void *)(&argk));
  	time_it_get_stats(tr, &stats);
  	HANDLE_ERROR(cudaMemcpy(x, dev_x, size, cudaMemcpyDeviceToHost));
  	HANDLE_ERROR(cudaMemcpy(result, dev_result, size, cudaMemcpyDeviceToHost));
  	printf("Time norm: mean(T) = %10.3e, std(T) = %10.3e\n", stats.mean, stats.std);
  	printf("Memory-bandwidth is %10.3e, GB/second\n", (n*sizeof(float)/stats.mean)/1e9);
  	free(x);
  	free(result);
	HANDLE_ERROR(cudaFree(dev_x));
	HANDLE_ERROR(cudaFree(dev_result));
	time_it_free(tr);
}

/* Question 3*/

// struct for passing arguments to time_it_run in Q3
struct kernel_rarg {
	uint nblks, tpb, m, seed;
	uint *x;
	curandState *randState;
};

// initialization
__global__ void setup_kernel(uint seed, curandState *state) {
  	
  	uint myId = blockDim.x * blockIdx.x + threadIdx.x;
  	seed = blockIdx.x;
  	curand_init(seed, myId, 0, &state[myId]);
}
// random number generator
__global__ void rndm(uint *x, uint m, curandState *randState) {
	
	uint myId = blockDim.x * blockIdx.x + threadIdx.x;
  	curandState *myRandState = &(randState[myId]);
  	for(int j = 0; j < m; j++) {
    	x[myId] = (curand_uniform(myRandState) <= 0.5);
  	}
}

void randNumGenerator(void *void_args) {
	struct kernel_rarg *argk = (struct kernel_rarg *)(void_args);
	rndm<<<argk->nblks, argk->tpb>>>(argk->x, argk->m, argk->randState);
	HANDLE_ERROR(cudaDeviceSynchronize());
}

void time_randNumGenerator(uint nblks, uint tpb, uint m, int ntrials) {
	
	uint n = nblks * tpb * m; 
	uint size = n * sizeof(uint);
	uint *x;
	uint *dev_x;
	x = (uint *)malloc(size);
	curandState *devState;
	
	HANDLE_ERROR(cudaMalloc((void**)(&devState), n * sizeof(curandState)));
	HANDLE_ERROR(cudaMalloc((void**)(&dev_x), size));
	HANDLE_ERROR(cudaMemcpy(dev_x, x, size, cudaMemcpyHostToDevice));
	struct kernel_rarg argk;
  	struct time_it_raw *tr = time_it_create(ntrials);
  	struct time_it_stats stats;
	// initialize argk
  	argk.x = dev_x;
  	argk.nblks = nblks;
  	argk.tpb = tpb;
  	argk.m = m;
  	argk.randState = devState;
  	
  	// run the kernel and report timing info
  	time_it_run(tr, randNumGenerator, (void *)(&argk));
  	time_it_get_stats(tr, &stats);
  	HANDLE_ERROR(cudaMemcpy(x, dev_x, size, cudaMemcpyDeviceToHost));
  	printf("Time randNumGenerator: mean(T) = %10.3e, std(T) = %10.3e\n", stats.mean, stats.std);
  	printf("Random number generation speed is %10.3e, random numbers per second\n", (n*sizeof(float)/stats.mean));
  	free(x);
	HANDLE_ERROR(cudaFree(dev_x));
	time_it_free(tr);
}

int main(int argc, char **argv) {
	
	uint what = atoi(argv[1]);
	cudaDeviceProp prop;
	int ndev;
  	HANDLE_ERROR(cudaGetDeviceCount(&ndev));
  	if(ndev < 1) {
    	fprintf(stderr, "No CUDA device found!\n");
    	exit(-1);
  	}
  	HANDLE_ERROR(cudaGetDeviceProperties(&prop, 0));
  	
  	int sharedMemPerBlock = prop.sharedMemPerBlock;
  	int regsPerBlock = prop.regsPerBlock;
  	printf("GPU is a %s supporing CUDA level %d.%d\n", prop.name, prop.major, prop.minor);
  	printf("It has %d SMs and a warp size of %d\n", prop.multiProcessorCount, prop.warpSize);
  	printf("sharedMemPerBlock = %d, regsPerBlock = %d\n", sharedMemPerBlock, regsPerBlock);
  	printf("clock rate = %d\n", prop.clockRate);
	
	switch(what) {
		case LOGISTIC:
			time_logistic(61440, 6400, 256, 10);
			break;
		case NORM:
			mem_norm(pow(2,26), 6144*2, 256, 10);
			break;
		case RND:
			time_randNumGenerator(1024, 512, 4, 10);
			break;
		default:
      		fprintf(stderr, "ERROR: unknown test case -- %d\n", what);
      		exit(-1);
	}
	exit(0);
}


