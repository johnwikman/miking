.PHONY: all clean

BINDIR=bin

all:
	rm -rf $(BINDIR)
	@sh -c 'for dir in $$(ls -d */); do make $${dir%%/}.build; done'

%.build:
	mkdir -p $(BINDIR)
	nvcc -x cu -arch=sm_50 -I`ocamlc -where` -dc $*/gpucode.cpp -o $*/gpuhost.o
	nvcc -arch=sm_50 -dlink $*/gpuhost.o -o $*/gpudevice.o

	ocamlopt $*/gpudevice.o $*/gpuhost.o $*/cpucode.ml \
	         -cclib -lcudart                           \
	         -cclib -lstdc++                           \
	         -O3                                       \
	         -o $(BINDIR)/$*.out

clean:
	rm -rf $(BINDIR)
	rm -f */*.o */*.cmi */*.cmx
