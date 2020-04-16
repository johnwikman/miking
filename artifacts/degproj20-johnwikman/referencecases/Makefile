.PHONY: all saxpy saxpysingle ger clean

SAXPYSIZES=2000 500000 1000000 10000000 50000000

GERSIZES=4096 8192

saxpy:
	make $(foreach size,$(SAXPYSIZES),$(size).saxpy)

%.saxpy:
	nvcc -arch=sm_50 -D_SIZE_=$* -lcublas -o saxpy-s$*.out saxpy/saxpy.cu -O3


saxpysingle:
	make $(foreach size,$(SAXPYSIZES),$(size).saxpysingle)

%.saxpysingle:
	nvcc -arch=sm_50 -D_SIZE_=$* -lcublas -o saxpysingle-s$*.out saxpysingle/saxpysingle.cu -O3


ger:
	make $(foreach size,$(GERSIZES),$(size).ger)

%.ger:
	nvcc -arch=sm_50 -D_SIZE_=$* -lcublas -o ger-s$*.out ger/ger.cu -O3


clean:
	rm -f *.out
	rm -f *.o
