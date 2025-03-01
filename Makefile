.PHONY: test lint clean

EMACS ?= emacs
BATCH = $(EMACS) -batch -Q -L .

test:
	$(BATCH) -l test/unison-sync-mode-tests.el -f ert-run-tests-batch-and-exit

lint:
	$(BATCH) -l package-lint -f package-lint-batch-and-exit unison-sync-mode.el

clean:
	rm -f *.elc test/*.elc