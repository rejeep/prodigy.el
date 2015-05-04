EMACS ?= emacs
CASK ?= cask

all: test

test:
	${MAKE} unit
	${MAKE} ecukes

unit:
	${CASK} exec ert-runner

ecukes:
	${CASK} exec ecukes

.PHONY:	all test unit ecukes
