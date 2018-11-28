all: build

GITCOMMIT:=$(shell git log -1 --pretty=format:"%h")

test: build
	stack test
build: setup
	stack build
setup:
	stack setup
clean:
	stack clean

release:
	stack --docker build
	stack image container
	docker save -o mylittlepwny-3034931.tar.gz mylittlepwny:latest
.PHONY: all setup build test release

README.pdf: README.org
	emacs -l .emacs.init --batch $< --eval "(org-beamer-export-to-pdf)"
