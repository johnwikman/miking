.PHONY: all run clean

BINDIR=bin
SHELL=bash

all:
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

%.out: FORCE
	$(eval PROGBASE := $(shell basename $@ .out))
	{ time ./$@; } &> $*.txt
FORCE:

run:
	$(eval OUTFILES := $(shell ls bin/*.out))
	make $(OUTFILES)
	mkdir -p raw-results
	mv bin/*.txt raw-results/

run-predict:
	$(eval OUTFILES := $(shell ls bin/*predict*.out))
	make $(OUTFILES)
	mkdir -p raw-results
	mv bin/*.txt raw-results/

clean:
	rm -rf $(BINDIR)
	rm -f */*.o */*.cmi */*.cmx
