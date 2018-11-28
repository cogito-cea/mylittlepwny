all: build

GITCOMMIT:=$(shell git log -1 --pretty=format:"%h")
GITVERSION:=$(shell git describe --tags)


test: build
	stack test
build: setup
	stack build
setup:
	stack setup
clean:
	stack clean

docker-test:
	stack --docker test

release: docker-test README.pdf
	stack --docker build
	stack image container
	docker save -o mylittlepwny-$(GITVERSION).tar.gz mylittlepwny:latest
.PHONY: all setup build test release

README.pdf: README.org
	emacs -l .emacs.init --batch $< --eval "(org-beamer-export-to-pdf)"
