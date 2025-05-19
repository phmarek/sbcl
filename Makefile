

TESTFILE	?= defstruct.impure.lisp

tests:
	cd tests/ && bash run-tests.sh $(TESTFILE)

.PHONY: tests

