;;; diff-review-magit-test.el --- Tests for diff-review-magit  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with magit on the load path, the selector "^diff-review-magit-":
;; the repository helpers come from diff-review-test.el, whose own tests
;; run in an Emacs without magit.

;;; Code:

(require 'ert)
(load (expand-file-name "diff-review-test.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'diff-review-magit)

(ert-deftest diff-review-magit-test-core-loads-no-magit ()
  "With magit on the load path, requiring diff-review loads none of it."
  (should (locate-library "magit"))
  (should (equal (with-output-to-string
                   (with-current-buffer standard-output
                     (call-process (expand-file-name invocation-name invocation-directory)
                                   nil t nil "-Q" "--batch"
                                   "--eval" (format "(setq load-path '%S)" load-path)
                                   "--eval" "(progn (require 'diff-review) (princ (featurep 'magit)))")))
                 "nil")))

(ert-deftest diff-review-magit-test-worktrees-section ()
  "Magit-status on main lists the other worktrees' branches with their reviews;
RET opens one.  A detached worktree and one whose branch is gone have no row."
  (diff-review-test--with-repo
    (let ((feature (diff-review-test--worktree repo "feature" "-b" "feature"))
          (other (diff-review-test--worktree repo "other" "-b" "feat/x"))
          (magit-status-sections-hook (append magit-status-sections-hook
                                              (list #'diff-review-magit-insert-worktrees)))
          status)
      (diff-review-test--worktree repo "gone" "-b" "gone")
      (diff-review-test--worktree repo "detached" "--detach")
      (diff-review-test--git "update-ref" "-d" "refs/heads/gone")
      (diff-review-test--commit-in feature "a.txt" "a\nB\nc\nd\nE\n" "feature one")
      (diff-review-test--commit-in feature "c.txt" "c\n" "feature two")
      (diff-review-test--commit-in other "a.txt" "A\n" "other one")
      ;; feature has a review: one of its two files viewed, a thread open, approved.
      (delete-other-windows)
      (diff-review-branch "feature")
      (diff-review--set-viewed (diff-review--file 0) t)
      (diff-review--add-comment nil nil nil "Looks fine.")
      (diff-review-approve)
      (kill-buffer)
      (unwind-protect
          (progn
            (magit-status-setup-buffer repo)
            (setq status (current-buffer))
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "^Worktree branches (2)" text))
              (should (string-match-p "^feature +2 commits  1/2 viewed  1 open  approved$" text))
              (should (string-match-p "^feat/x +1 commit$" text))
              (should-not (string-match-p "^gone" text)))
            ;; From a worktree, main is a row and the worktree is not.
            (let ((default-directory feature))
              (with-temp-buffer
                (magit-section-mode)
                (let ((inhibit-read-only t))
                  (magit-insert-section (status) (diff-review-magit-insert-worktrees)))
                (should (string-match-p "^main +0 commits$" (buffer-string)))
                (should-not (string-match-p "^feature" (buffer-string)))))
            ;; RET on a row without a review creates and opens it.
            (goto-char (point-min))
            (re-search-forward "^feat/x")
            (call-interactively (key-binding (kbd "RET")))
            (should (derived-mode-p 'diff-review-mode))
            (should (equal diff-review--head "feat/x"))
            (should (file-exists-p (diff-review--branch-file repo "feat/x"))))
        (when (buffer-live-p status) (kill-buffer status))))))

(provide 'diff-review-magit-test)
;;; diff-review-magit-test.el ends here
