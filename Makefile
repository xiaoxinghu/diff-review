.PHONY: check deps

EMACS ?= emacs
# The dependencies live in .deps/ as package-user-dir: magit-section, and
# magit for diff-review-magit.el.
DEPS = --eval '(setq package-user-dir (expand-file-name ".deps"))'
MAGIT = $(EMACS) -Q --batch $(DEPS) -f package-initialize -L .
# The package without magit on the load path: diff-review.el must not need it.
BATCH = $(EMACS) -Q --batch $(DEPS) --eval "(setq package-load-list '((magit nil) all))" \
  -f package-initialize -L .

deps: .deps/ok

# A file, not the folder, so a .deps from before magit was a dependency is refetched.
.deps/ok:
	$(EMACS) -Q --batch $(DEPS) -l package \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	  -f package-initialize -f package-refresh-contents \
	  --eval "(package-install 'magit-section)" \
	  --eval "(package-install 'magit)" \
	  && touch $@ || { rm -rf .deps; exit 1; }

check: .deps/ok
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile diff-review.el
	$(MAGIT) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile diff-review-magit.el
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile diff-review-github.el
	rm -f diff-review.elc diff-review-magit.elc diff-review-github.elc
	$(BATCH) --eval '(checkdoc-file "diff-review.el")' --eval '(checkdoc-file "diff-review-magit.el")' \
	  --eval '(checkdoc-file "diff-review-github.el")'
	$(BATCH) -l test/diff-review-test.el -f ert-run-tests-batch-and-exit
	$(BATCH) -l test/diff-review-github-test.el --eval '(ert-run-tests-batch-and-exit "^diff-review-github-")'
	$(MAGIT) -l test/diff-review-magit-test.el --eval '(ert-run-tests-batch-and-exit "^diff-review-magit-")'
