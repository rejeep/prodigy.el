EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc
	${MAKE} unit
	${MAKE} ecukes
	${MAKE} compile
	${MAKE} unit
	${MAKE} ecukes
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner

ecukes:
	${CASK} exec ecukes --reporter magnars

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile prodigy.el

clean-elc:
	rm -f prodigy.elc

.PHONY:	all test unit ecukes compile
