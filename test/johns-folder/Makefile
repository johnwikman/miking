.PHONY: build upload


SOURCE=ocamlcode.mc

build:
	miking run $(SOURCE)

upload:
	scp codegen/cpucode.ml sleipner:~/Code/gen/cpucode.ml
	scp codegen/gpucode.cpp sleipner:~/Code/gen/gpucode.cpp
	scp codegen/Makefile sleipner:~/Code/gen/Makefile
