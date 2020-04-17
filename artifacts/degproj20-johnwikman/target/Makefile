.PHONY: all benchmark clean

BINDIR=bin

all:
	rm -rf $(BINDIR)
	@sh -c 'for dir in $$(ls -d */); do make $${dir%%/}.build; done'

%.buildall:
	@sh -c 'for dir in $$(ls -d $**/); do make $${dir%%/}.build; done'

%.build:
	mkdir -p $(BINDIR)
	nvcc -x cu -arch=sm_50 -I`ocamlc -where` -dc $*/gpucode.cpp -o $*/gpuhost.o
	nvcc -arch=sm_50 -dlink $*/gpuhost.o -o $*/gpudevice.o

	ocamlopt $*/gpudevice.o $*/gpuhost.o \
	         unix.cmxa $*/cpucode.ml     \
	         -cclib -lcudart             \
	         -cclib -lstdc++             \
	         -O3                         \
	         -o $(BINDIR)/$*.out

benchmark:
	mkdir -p results
	make $(foreach p, $(shell ls bin/*.out), $(shell basename $(p) .out).benchmark)
%.benchmark:
	nvprof ./bin/$*.out > results/$*.toml 2> results/$*.nvprof

clean:
	rm -rf $(BINDIR)
	rm -f */*.o */*.cmi */*.cmx
