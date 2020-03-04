.PHONY: build upload


SOURCE=main.mc

build:
	miking run $(SOURCE)

upload:
	scp target/cpucode.ml sleipner:~/Code/gen/cpucode.ml
	scp target/gpucode.cpp sleipner:~/Code/gen/gpucode.cpp
	scp target/Makefile sleipner:~/Code/gen/Makefile
