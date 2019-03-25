TARGET=target
EXE=$(TARGET)/okta-aws-login
DIST_EXE=$(EXE)-$(shell uname -s)-$(shell uname -m)
DIST_EXE_SIG=$(DIST_EXE).sig

default: test lint

build:
	stack build okta-aws-login

build-prof:
	stack build --profile --ghc-options="-rtsopts" okta-aws-login

test:
	stack test okta-aws-login

lint:
	hlint src test

install:
	stack install okta-aws-login

bindist:
	mkdir -p $(TARGET)
	stack --local-bin-path $(TARGET) install $(STACK_OPTS) okta-aws-login
	upx --best $(EXE)
	mv $(EXE) $(DIST_EXE)
	gpg --output $(DIST_EXE_SIG) --detach-sign $(DIST_EXE)

clean:
	stack clean
	rm -rf target

hoogle:
	stack hoogle --server

.PHONY: \
	build \
	build-prof \
	clean \
	default \
	hoogle \
	install \
	lint \
	test \
