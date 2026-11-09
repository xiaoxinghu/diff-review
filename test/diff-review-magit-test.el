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

(defun diff-review-magit-test--paths ()
  "The paths of this review's files."
  (mapcar (lambda (f) (plist-get f :path)) diff-review--files))

(defun diff-review-magit-test--select (lines)
  "In a magit log, select its first LINES commits and review them."
  (goto-char (point-min))
  (push-mark (point) t t)
  (dotimes (_ (1- lines)) (magit-section-forward))
  (diff-review-magit-commits))

(ert-deftest diff-review-magit-test-commits ()
  "Selections in a log review exactly their commits, each in a range file
under the common dir, the same from main's clone and the branch's worktree;
without a region, the commit at point."
  (diff-review-test--with-repo
    (let ((feature (diff-review-test--worktree repo "feature" "-b" "feature"))
          (transient-mark-mode t)
          log three)
      ;; One file a commit, so a review's files say which commits it covers.
      (dotimes (i 5)
        (diff-review-test--commit-in feature (format "f%d.txt" (1+ i)) "x\n"
                                     (format "feature %d" (1+ i))))
      (delete-other-windows)
      (unwind-protect
          (progn
            (let ((default-directory feature))
              (setq log (magit-log-setup-buffer (list "feature") nil nil)))
            (set-buffer log)
            ;; The top three, then the top five: two reviews.
            (diff-review-magit-test--select 3)
            (should (derived-mode-p 'diff-review-mode))
            (should (equal (diff-review-magit-test--paths) '("f3.txt" "f4.txt" "f5.txt")))
            (should (diff-review-test--same-file-p diff-review--root repo))
            (setq three buffer-file-name)
            (should (diff-review-test--same-file-p
                     (file-name-directory three)
                     (expand-file-name "review/" (diff-review--common-dir repo))))
            (should-not (region-active-p))
            (diff-review--set-viewed (diff-review--file 0) t)
            (kill-buffer)
            (with-current-buffer log
              (diff-review-magit-test--select 5)
              (should (equal (diff-review-magit-test--paths)
                             '("f1.txt" "f2.txt" "f3.txt" "f4.txt" "f5.txt")))
              (should-not (equal buffer-file-name three))
              (should-not (file-exists-p (diff-review--branch-file repo "feature")))
              (kill-buffer))
            ;; The first selection again, from main's clone: the same file, its mark kept.
            (let ((default-directory repo))
              (set-buffer (magit-log-setup-buffer (list "feature") nil nil))
              (diff-review-magit-test--select 3)
              (should (equal buffer-file-name three))
              (should (diff-review--viewed-p (diff-review--file 0)))
              (kill-buffer))
            ;; No region: the commit at point.
            (with-current-buffer log
              (deactivate-mark)
              (goto-char (point-min))
              (magit-section-forward)
              (diff-review-magit-commits)
              (should (equal (diff-review-magit-test--paths) '("f4.txt")))
              (kill-buffer)))
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (derived-mode-p 'magit-mode) (kill-buffer buffer))))))))

(provide 'diff-review-magit-test)
;;; diff-review-magit-test.el ends here
