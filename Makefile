.PHONY: check

EMACS ?= emacs
# magit-section comes from an Emacs config's packages; INIT names that config.
INIT ?= $(HOME)/.config/emacs
BATCH = $(EMACS) -Q --batch --init-directory $(INIT) -f package-initialize -L .

check:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile diff-review.el
	rm -f diff-review.elc
	$(BATCH) --eval '(checkdoc-file "diff-review.el")'
	$(BATCH) -l test/diff-review-test.el -f ert-run-tests-batch-and-exit
