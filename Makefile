.PHONY: run
run:
	stack exec aes-test

.PHONY: build
build:
	stack build

.PHONY: test
test:
	stack test

.PHONY: stylish-haskell
stylish-haskell:
	find . src tests -name '*hs' -exec stylish-haskell {} -i \;

