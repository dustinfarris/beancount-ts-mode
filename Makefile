EMACS ?= emacs
BATCH := $(EMACS) -Q --batch
SRC := beancount-ts-mode.el beancount-ts-refile.el
TEST_FILES := $(wildcard test/*-test.el)

.PHONY: test lint clean grammar

## Run the ERT suite
test: clean
	$(BATCH) -L . $(foreach f,$(TEST_FILES),-l $(f)) -f ert-run-tests-batch-and-exit

## Byte-compile with warnings as errors
lint:
	$(BATCH) -L . --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRC) $(TEST_FILES)

## Build the tree-sitter grammar the mode pins (needs git and a C compiler)
grammar:
	$(BATCH) -L . -l beancount-ts-mode.el \
	  --eval '(treesit-install-language-grammar (quote beancount))'

clean:
	rm -f *.elc test/*.elc
