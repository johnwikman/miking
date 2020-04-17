.PHONY: all clean runall \
        saxpy_custom saxpy_example saxpy \
        saxpysingle_custom saxpysingle \
        vecadd_custom vecadd \
        vecscalar_custom vecscalar \
        ger matadd

NVCC=nvcc
NVCCFLAGS=-arch=sm_50 -lcublas -O3
SHELL=bash

SAXPYSIZES=64 256 1024 4096 16384 65536 262144 500000 1000000 10000000 50000000

GERSIZES=32 64 128 256 512 1024 2048 4096 8192

MATADDSIZES=32 64 128 256 512 1024 2048 4096 8192

VECADDSIZES=64 256 1024 4096 16384 65536 262144 500000 1000000 10000000 50000000

VECSCALARSIZES=64 256 1024 4096 16384 65536 262144 500000 1000000 10000000 50000000

all: saxpy_custom saxpy_example saxpy \
     saxpysingle_custom saxpysingle \
     vecadd_custom vecadd \
     vecscalar_custom vecscalar \
     ger matadd

clean:
	rm -f *.out
	rm -f *.o

runall:
	mkdir -p results
	make $(foreach prog,$(shell ls *.out),$(shell basename $(prog) .out).run)
%.run:
	nvprof ./$*.out > results/$*.toml 2> results/$*.nvprof


saxpy_custom:
	make $(foreach size,$(SAXPYSIZES),$(size).$@)
%.saxpy_custom:
	$(NVCC) $(NVCCFLAGS) -D_SIZE_=$* -o saxpy_custom-s$*.out saxpy/saxpy.cu -DUSE_CUSTOM

saxpy_example:
	make $(foreach size,$(SAXPYSIZES),$(size).$@)
%.saxpy_example:
	$(NVCC) $(NVCCFLAGS) -D_SIZE_=$* -o saxpy_example-s$*.out saxpy/saxpy.cu -DUSE_EXAMPLE

saxpy:
	make $(foreach size,$(SAXPYSIZES),$(size).$@)
%.saxpy:
	$(NVCC) $(NVCCFLAGS) -D_SIZE_=$* -o saxpy-s$*.out saxpy/saxpy.cu


saxpysingle:
	make $(foreach size,$(SAXPYSIZES),$(size).$@)
%.saxpysingle:
	$(NVCC) $(NVCCFLAGS) -D_SIZE_=$* -o saxpysingle-s$*.out saxpysingle/saxpysingle.cu

saxpysingle_custom:
	make $(foreach size,$(SAXPYSIZES),$(size).$@)
%.saxpysingle_custom:
	$(NVCC) $(NVCCFLAGS) -D_SIZE_=$* -o saxpysingle_custom-s$*.out saxpysingle/saxpysingle.cu -DUSE_CUSTOM


ger:
	make $(foreach size,$(GERSIZES),$(size).$@)
%.ger:
	$(NVCC) $(NVCCFLAGS) -D_SIZE_=$* -o ger-s$*.out ger/ger.cu


matadd:
	make $(foreach size,$(MATADDSIZES),$(size).$@)
%.matadd:
	$(NVCC) $(NVCCFLAGS) -D_SIZE_=$* -o matadd-s$*.out matadd/matadd.cu


vecadd:
	make $(foreach size,$(VECADDSIZES),$(size).$@)
%.vecadd:
	$(NVCC) $(NVCCFLAGS) -D_SIZE_=$* -o vecadd-s$*.out vecadd/vecadd.cu

vecadd_custom:
	make $(foreach size,$(VECADDSIZES),$(size).$@)
%.vecadd_custom:
	$(NVCC) $(NVCCFLAGS) -D_SIZE_=$* -o vecadd_custom-s$*.out vecadd/vecadd.cu -DUSE_CUSTOM


vecscalar:
	make $(foreach size,$(VECSCALARSIZES),$(size).$@)
%.vecscalar:
	$(NVCC) $(NVCCFLAGS) -D_SIZE_=$* -o vecscalar-s$*.out vecscalar/vecscalar.cu

vecscalar_custom:
	make $(foreach size,$(VECSCALARSIZES),$(size).$@)
%.vecscalar_custom:
	$(NVCC) $(NVCCFLAGS) -D_SIZE_=$* -o vecscalar_custom-s$*.out vecscalar/vecscalar.cu -DUSE_CUSTOM
