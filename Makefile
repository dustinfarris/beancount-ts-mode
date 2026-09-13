EMACS ?= emacs
BATCH := $(EMACS) -Q --batch
SRC := beancount-ts-mode.el beancount-ts-refile.el
TEST_FILES := $(wildcard test/*-test.el)
TEST_HELPER := test/beancount-ts-test-helper.el

.PHONY: test lint clean grammar

## Run the ERT suite
test: clean
	$(BATCH) -L . -L test $(foreach f,$(TEST_FILES),-l $(f)) -f ert-run-tests-batch-and-exit

## Byte-compile with warnings as errors
lint:
	$(BATCH) -L . -L test --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRC) $(TEST_HELPER) $(TEST_FILES)

## Build the tree-sitter grammar the mode pins (needs git and a C compiler).
## `treesit-install-language-grammar' reports a failed build as a warning
## and returns normally, so check that the grammar actually loads.
grammar:
	$(BATCH) -L . -l beancount-ts-mode.el \
	  --eval '(treesit-install-language-grammar (quote beancount))' \
	  --eval '(unless (treesit-ready-p (quote beancount)) (kill-emacs 1))'

clean:
	rm -f *.elc test/*.elc
