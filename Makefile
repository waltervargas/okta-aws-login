TARGET=target
EXE=$(TARGET)/okta-aws-login
DIST_EXE=$(EXE)-$(shell uname -s)-$(shell uname -m)
DIST_EXE_SIG=$(DIST_EXE).gpg.sig

build:
	stack build okta-aws-login

build-prof:
	stack build --profile --ghc-options="-rtsopts" okta-aws-login

install:
	stack install okta-aws-login

bindist:
	mkdir -p $(TARGET)
	stack --local-bin-path $(TARGET) install okta-aws-login
	upx --best $(EXE)
	mv $(EXE) $(DIST_EXE)
	gpg --output $(DIST_EXE_SIG) --detach-sign $(DIST_EXE)

clean:
	stack clean
	rm -rf target

tags:
	hasktags-generate .

sources:
	stack-unpack-dependencies


.PHONY: build build-prof clean tags sources

