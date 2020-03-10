.PHONY: all clean

all:
	@sh -c 'for dir in $$(ls -d */); do make $${dir%%/}.build; done'

%.build:
	nvcc -x cu -arch=sm_50 -I`ocamlc -where` -dc $*/gpucode.cpp -o $*/gpuhost.o
	nvcc -arch=sm_50 -dlink $*/gpuhost.o -o $*/gpudevice.o

	ocamlopt $*/gpudevice.o $*/gpuhost.o $*/cpucode.ml \
	         -cclib -lcudart                           \
	         -cclib -lstdc++                           \
	         -o $*.out

clean:
	rm -f *.out */*.o */*.cmi */*.cmx
