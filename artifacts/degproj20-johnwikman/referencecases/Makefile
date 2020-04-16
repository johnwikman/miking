.PHONY: all clean runall \
        saxpy_custom saxpy_example saxpy \
        saxpysingle_custom saxpysingle \
        ger matadd

NVCC=nvcc
NVCCFLAGS=-arch=sm_50 -lcublas -O3
SHELL=bash

SAXPYSIZES=2000 500000 1000000 10000000 50000000

GERSIZES=4096 8192

MATADDSIZES=32 4096 8192


all: saxpy_custom saxpy_example saxpy saxpysingle saxpysingle_custom ger matadd

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
