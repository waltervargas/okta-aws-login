build:
	stack build okta-aws-login

build-prof:
	stack build --profile --ghc-options="-rtsopts" okta-aws-login 

install:
	stack install okta-aws-login

clean:
	stack clean

tags:
	hasktags-generate .

sources:
	stack-unpack-dependencies

.PHONY: build build-prof clean tags sources

