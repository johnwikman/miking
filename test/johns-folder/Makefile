.PHONY: upload clean

all:
	make $(shell ls *.mc)

%.mc: FORCE
	mkdir -p target/$*
	miking run $@ target/$*
FORCE:

upload:
	ssh sleipner rm -rf /home/wikman/Code/gen
	scp -r target/ sleipner:/home/wikman/Code/gen/

clean:
	$(eval TARGETDIR := $(shell ls -d target/*/))
	rm -rf $(TARGETDIR)
