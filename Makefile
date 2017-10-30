all: build
test: build
	stack test
build: setup
	stack build
setup:
	stack setup
clean:
	stack clean
.PHONY: all setup build test
