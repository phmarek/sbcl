SBCL_TOP ?= $(realpath ../..)
SYSTEM=sb-udef-inttype
include ../asdf-module.mk


SHELL := /usr/bin/bash
dumpfile  := -
sbcl := ../../run-sbcl.sh --dynamic-space-size 2000
time := /usr/bin/time --format '  Elapsed:   %E   User: %U   System: %S'
print_size := [ "x$(dumpfile)" = "x-" ] || ( stat --printf="  Size: %12s" $(dumpfile) ; $(dumpfile) )

ifdef V
	Q := 
else
	Q := @
endif


comparison:
	$(Q)$(MAKE) c1 c2 c3 c4
c1:
	$(Q)echo DEFSTRUCT:
	$(Q)$(time) $(sbcl) --script s-l-a-d-comparison.lisp 1 $(dumpfile)
	$(Q)$(print_size)
c2:
	$(Q)echo DEF-COLUMN-STRUCT:
	$(Q)$(time) $(sbcl) --script s-l-a-d-comparison.lisp 2 $(dumpfile)
	$(Q)$(print_size)
c3:
	$(Q)echo DEF-COLUMN-STRUCT with large initial-size:
	$(Q)$(time) $(sbcl) --script s-l-a-d-comparison.lisp 3 $(dumpfile)
	$(Q)$(print_size)
c4:
	$(Q)echo DEF-COLUMN-STRUCT with large initial-size and batched:
	$(Q)$(time) $(sbcl) --script s-l-a-d-comparison.lisp 4 $(dumpfile)
	$(Q)$(print_size)

recompile:
	( cd ../.. && bash make-target-contrib.sh )

test:
	$(Q)$(time) $(sbcl) --load ../../tests/test-util.lisp --script test-bitfield.lisp
	$(Q)$(time) $(sbcl) --load ../../tests/test-util.lisp --script example-column-structure.lisp
	$(Q)$(time) $(sbcl) --load ../../tests/test-util.lisp --script example-buffer.lisp
	$(MAKE) comparison
