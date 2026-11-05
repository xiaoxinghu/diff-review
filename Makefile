.PHONY: check deps

EMACS ?= emacs
# magit-section, the one dependency, lives in .deps/ as package-user-dir.
DEPS = --eval '(setq package-user-dir (expand-file-name ".deps"))'
BATCH = $(EMACS) -Q --batch $(DEPS) -f package-initialize -L .

deps: .deps

.deps:
	$(EMACS) -Q --batch $(DEPS) -l package \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	  -f package-initialize -f package-refresh-contents \
	  --eval "(package-install 'magit-section)" \
	  || { rm -rf .deps; exit 1; }

check: .deps
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile diff-review.el
	rm -f diff-review.elc
	$(BATCH) --eval '(checkdoc-file "diff-review.el")'
	$(BATCH) -l test/diff-review-test.el -f ert-run-tests-batch-and-exit
