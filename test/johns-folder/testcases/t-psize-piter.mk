# Makefile for a test case that has a size parameter and an iteration parameter

.PHONY: all cptarget clean

all:
	make $(shell ls [^_]*.mc)
	make cptarget

cptarget:
	mkdir -p ../../target
	cp -r target/* ../../target/

%.iter: FORCE
	$(eval PWDBASE := $(shell basename `pwd`))
	$(eval PROG := $(shell dirname $(shell dirname $*)))
	$(eval SIZE := $(shell basename $(shell dirname $*)))
	$(eval ITER := $(shell basename $*))
	cp $(PROG).mc _specific_.mc
	cp sizes/$(SIZE).mc _size_.mc
	cp iterations/$(ITER).mc _iter_.mc
	mkdir -p target/$(PWDBASE)-$(PROG)-s$(SIZE)-it$(ITER)
	miking run _main_.mc target/$(PWDBASE)-$(PROG)-s$(SIZE)-it$(ITER)
	rm _specific_.mc
	rm _size_.mc
	rm _iter_.mc
%.size: FORCE
	$(eval ITERS := $(shell ls iterations/*.mc))
	make $(foreach iter,$(ITERS),$*/$(shell basename $(iter) .mc).iter)
%.mc: FORCE
	$(eval SIZES := $(shell ls sizes/*.mc))
	make $(foreach size,$(SIZES),$*/$(shell basename $(size) .mc).size)
FORCE:

clean:
	rm -rf target/
	rm -f _size_.mc _specific_.mc
