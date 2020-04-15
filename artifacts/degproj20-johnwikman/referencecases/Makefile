.PHONY: all saxpy clean

SAXPYSIZES=2000 500000 1000000 10000000 50000000

saxpy:
	make $(foreach size,$(SAXPYSIZES),$(size).saxpy)

%.saxpy:
	nvcc -arch=sm_50 --std=c++11 -D_SIZE_=$* -lcublas -o saxpy-s$*.out saxpy/saxpy.cu

clean:
	rm -f *.out
	rm -f *.o
