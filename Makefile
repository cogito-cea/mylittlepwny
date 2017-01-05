.PHONY: build
build:
	stack build

.PHONY: run
run: build
	stack exec aes-test -- --output random.html

.PHONY: test
test:
	stack test

.PHONY: stylish-haskell
stylish-haskell:
	find . src tests -name '*hs' -exec stylish-haskell {} -i \;
