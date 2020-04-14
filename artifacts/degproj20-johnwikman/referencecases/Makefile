.PHONY: all sieve clean

SIEVESIZES=100000 500000 1000000 10000000 50000000

sieve:
	make $(foreach size,$(SIEVESIZES),$(size).sievecpu)

%.sievecpu:
	g++ --std=c++11 -D_SIZE_=$* -o sievecpu-s$*.out sieve/sieve_cpu.cpp

clean:
	rm -f *.out

