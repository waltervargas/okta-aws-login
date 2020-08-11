default: hpack test lint

hpack:
	hpack

build:
	cabal new-build

test:
	cabal new-test

lint:
	hlint src app test

clean:
	cabal clean

.PHONY: \
	build \
	clean \
	default \
	hoogle \
	lint \
	test \
