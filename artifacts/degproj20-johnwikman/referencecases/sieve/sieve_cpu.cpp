/**
 * Sieve test case reference implementation.
 */

#include <cstdlib>
#include <cmath>
#include <iostream>

#include "../benchmark_suite.hpp"

#ifndef _SIZE_
#error The _SIZE_ constant is not defined!
#endif

void prepare_sieve(void);
void calculate_sieve(void);

static bool *sieve_arr = NULL;

int main(int argc, char *argv[])
{
	BENCHMARK(prepare_sieve, calculate_sieve);

	// TEMP: print primes
	if (sieve_arr != NULL) {
		//std::cout << "Primes: " << std::endl;
		//for (size_t i = 0; i < _SIZE_; ++i) {
		//	if (sieve_arr[i])
		//		std::cout << i << " ";
		//}
		//std::cout << std::endl;
		delete[] sieve_arr;
	}
	return 0;
}

void prepare_sieve(void)
{
	if (sieve_arr != NULL)
		delete[] sieve_arr;

	sieve_arr = new bool[_SIZE_];
	sieve_arr[0] = false;
	sieve_arr[1] = false;
	for (size_t i = 2; i < _SIZE_; ++i)
		sieve_arr[i] = true;
}

void calculate_sieve(void)
{
	size_t limit = (size_t) std::ceil(std::sqrt(_SIZE_));

	for (size_t i = 0; i <= limit; ++i) {
		if (!sieve_arr[i])
			continue;

		// i is a prime number
		for (size_t j = i + i; j < _SIZE_; j += i)
			sieve_arr[j] = false;
	}
}
