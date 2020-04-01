.PHONY: tests tuning tuning-upload upload clean

tests:
	$(eval TESTCASES := $(shell ls -d testcases/*/))
	make $(foreach testcase,$(TESTCASES),$(testcase).dir)

tuning:
	$(eval TUNINGCASES := $(shell ls -d tuningcases/*/))
	make $(foreach tuningcase,$(TUNINGCASES),$(tuningcase).dir)

%.dir: FORCE
	make -C $*
FORCE:

upload:
	ssh sleipner rm -rf /home/wikman/Code/gen
	scp -r target/ sleipner:/home/wikman/Code/gen/

upload-tuning:
	ssh sleipner rm -rf /home/wikman/Code/gen
	scp -r tuningtarget/ sleipner:/home/wikman/Code/gen/

clean:
	$(eval TARGETS := $(shell ls -d target/*/))
	$(eval TUNINGTARGETS := $(shell ls -d tuningtarget/*/))
	$(eval INTERNALTESTTARGETS := $(foreach tc,$(shell ls -d testcases/*/),$(tc)/target))
	$(eval INTERNALTUNINGTARGETS := $(foreach tc,$(shell ls -d tuningcases/*/),$(tc)/target))
	rm -rf $(TARGETS)
	rm -rf $(TUNINGTARGETS)
	rm -rf $(INTERNALTESTTARGETS)
	rm -rf $(INTERNALTUNINGTARGETS)
