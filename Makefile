SHELL = /bin/sh
EMACS = emacs

EXCLUDES := $(wildcard *-pkg.el *-autoloads.el)
TESTS := $(wildcard *-tests.el)
SRCS := $(filter-out $(EXCLUDES) $(TESTS),$(wildcard *.el))
OBJS := $(SRCS:.el=.elc)

.SUFFIXES:
.PHONY: all clean check

all: $(OBJS)

clean:
	-rm -f $(OBJS)

check: all
	$(EMACS) -batch -Q -L . -l ert $(addprefix -l ,$(TESTS)) \
		-f ert-run-tests-batch-and-exit

%.elc: %.el
	$(EMACS) -batch -Q -L . -f batch-byte-compile $<
