.PHONY: test lint clean

EMACS ?= emacs
BATCH = $(EMACS) -batch -Q -L .

test:
	$(BATCH) -l test/unison-sync-mode-tests.el -f ert-run-tests-batch-and-exit

lint:
	$(EMACS) -batch --eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'package-lint) (package-refresh-contents) (package-install 'package-lint))" \
		-l package-lint \
		--eval "(setq package-lint-main-file \"unison-sync-mode.el\")" \
		-f package-lint-batch-and-exit unison-sync-mode.el

clean:
	rm -f *.elc test/*.elc