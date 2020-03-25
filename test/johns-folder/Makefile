.PHONY: all upload clean

all:
	$(eval TESTCASES := $(shell ls -d testcases/*/))
	make $(foreach testcase,$(TESTCASES),$(testcase).dir)

%.dir: FORCE
	make -C $*
FORCE:

upload:
	ssh sleipner rm -rf /home/wikman/Code/gen
	scp -r target/ sleipner:/home/wikman/Code/gen/

clean:
	$(eval TARGETDIR := $(shell ls -d target/*/))
	rm -rf $(TARGETDIR)
