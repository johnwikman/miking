# Makefile for a test case that only has size parameter

.PHONY: all cptarget clean

all:
	make $(shell ls [^_]*.mc)
	make cptarget

cptarget:
	mkdir -p ../../target
	cp -r target/* ../../target/

%.size: FORCE
	$(eval PWDBASE := $(shell basename `pwd`))
	$(eval PROG := $(shell dirname $*))
	$(eval SIZE := $(shell basename $*))
	cp $(PROG).mc _specific_.mc
	cp sizes/$(SIZE).mc _size_.mc
	mkdir -p target/$(PWDBASE)-$(PROG)-s$(SIZE)
	miking run _main_.mc target/$(PWDBASE)-$(PROG)-s$(SIZE)
	rm _specific_.mc
	rm _size_.mc
%.mc: FORCE
	$(eval SIZES := $(shell ls sizes/*.mc))
	make $(foreach size,$(SIZES),$*/$(shell basename $(size) .mc).size)
FORCE:

clean:
	rm -rf target/
	rm -f _size_.mc _specific_.mc
