SBCL_TOP ?= $(realpath ../..)
SYSTEM=sb-udef-inttype
include ../asdf-module.mk


SHELL := /usr/bin/bash
dumpfile  := -
# The 1-level (re)allocations need a fair bit of RAM
sbcl := ../../run-sbcl.sh --dynamic-space-size 3000
time := /usr/bin/time --format '     Elapsed:   %E   User: %U   System: %S   max RAM:%M'
print_size := [ "x$(dumpfile)" = "x-" ] || ( stat --printf="     Size: %12s" $(dumpfile) ; $(dumpfile) --no-userinit --no-sysinit --eval '(sizes)' --eval '(sb-ext:quit)' )

ifdef V
	Q := 
else
	Q := @
endif


comparison:
	$(Q)$(MAKE) c1 c4 c3 c-note c2
.PHONY: c-note
c-note:
	@echo "" 
	@echo "The values above are the interesting ones"
	@echo "" 

c1:
	$(Q)echo --- DEFSTRUCT:
	$(Q)$(time) $(sbcl) --script s-l-a-d-comparison.lisp 1 $(dumpfile)
	$(Q)$(print_size)
c2:
	$(Q)echo --- DEF-COLUMN-STRUCT:
	$(Q)$(time) $(sbcl) --script s-l-a-d-comparison.lisp 2 $(dumpfile)
	$(Q)$(print_size)
c3:
	$(Q)echo --- DEF-COLUMN-STRUCT with large initial-size:
	$(Q)$(time) $(sbcl) --script s-l-a-d-comparison.lisp 3 $(dumpfile)
	$(Q)$(print_size)
c4:
	$(Q)echo --- DEF-COLUMN-STRUCT with 2-level allocation:
	$(Q)$(time) $(sbcl) --script s-l-a-d-comparison.lisp 4 $(dumpfile)
	$(Q)$(print_size)

recompile:
	( cd ../.. && bash make-target-contrib.sh )

test:
	cd $(SBCL_TOP)/tests && bash run-tests.sh sb-udef.impure.lisp
	$(MAKE) comparison
