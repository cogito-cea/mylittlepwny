.PHONY: build
build:
	stack build

.PHONY: run
run: build
	stack exec aes-test

.PHONY: test
test:
	stack test

.PHONY: stylish-haskell
stylish-haskell:
	find . src tests -name '*hs' -exec stylish-haskell {} -i \;

# install binaries locally, in $HOME/.local/bin
.PHONY: install
install: build
	stack install
