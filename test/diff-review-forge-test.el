;;; diff-review-forge-test.el --- Tests for diff-review-forge  -*- lexical-binding: t; -*-

;;; Commentary:

;; What the backends' tests share: a repository whose bare origin has a
;; pull request at a ref, and the tests of the forge layer itself.  Each
;; backend's tests load this file; the repository helpers come from
;; diff-review-test.el.

;;; Code:

(require 'ert)
(load (expand-file-name "diff-review-test.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'diff-review-forge)

(defun diff-review-forge-test--json (value)
  "VALUE, alists and vectors, as a forge's JSON reads back."
  (json-parse-string (json-serialize value) :null-object nil :false-object nil))

(defmacro diff-review-forge-test--with-pr (ref &rest body)
  "Run BODY with a pull request at REF on a bare origin, `head' its tip.
The pull request changes line 2 of long.txt, twenty lines on main, on
the branch checked out in the worktree `feature'.  Questions are answered yes."
  (declare (indent 1))
  `(diff-review-test--with-repo
     (let* ((origin (concat (directory-file-name repo) "-origin"))
            (feature (diff-review-test--worktree repo "feature" "-b" "feature"))
            (long (mapconcat (lambda (i) (format "l%d\n" i)) (number-sequence 1 20) ""))
            (diff-review-sidebar nil)
            head)
       (diff-review-test--commit-in repo "long.txt" long "long")
       (let ((default-directory feature))
         (diff-review-test--git "merge" "-q" "main"))
       (diff-review-test--commit-in feature "long.txt" (string-replace "l2\n" "L2\n" long) "pr")
       (setq head (diff-review--git-at feature "rev-parse" "HEAD"))
       (diff-review-test--git "init" "-q" "--bare" origin)
       (diff-review-test--git "remote" "add" "origin" origin)
       (diff-review-test--git "push" "-q" "origin" "main" (concat "feature:" ,ref))
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
         (delete-other-windows)
         ,@body))))

(defun diff-review-forge-test--by-node (node)
  "The review's thread whose forge id is NODE."
  (seq-find (lambda (c) (equal (diff-review-forge--node c) node)) diff-review--comments))

(ert-deftest diff-review-forge-test-url-of-a-remote ()
  "Remote URLs name a host and a path, nested groups too; a local path names none."
  (dolist (url '("git@github.com:o/r.git" "https://github.com/o/r" "https://github.com/o/r.git/"
                 "ssh://git@github.com:22/o/r.git" "https://me:t0k@github.com/o/r.git"))
    (should (equal (diff-review-forge--url url) '("github.com" . "o/r"))))
  (should (equal (diff-review-forge--url "git@gitlab.corp:team/sub/app.git")
                 '("gitlab.corp" . "team/sub/app")))
  (should-not (diff-review-forge--url "/tmp/x/origin"))
  (should-not (diff-review-forge--url "file:///tmp/x/origin.git")))

(ert-deftest diff-review-forge-test-read-a-number ()
  "A pull request is picked from the list or typed, with or without its sign;
with a prefix argument its number is read alone, and the list not asked for."
  (let ((candidates (lambda () '(("#3 Fix it  (fix, alice)" . 3) ("#14711 Other" . 14711)))))
    (dolist (case '(("#3 Fix it  (fix, alice)" . 3) ("4711" . 4711) ("!42" . 42) (" #7 " . 7)))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (car case))))
        (should (equal (diff-review-forge-read "PR: " candidates) (cdr case)))))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "fix")))
      (should-error (diff-review-forge-read "PR: " candidates) :type 'user-error))
    (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 4711)))
      (let ((current-prefix-arg '(4)))
        (should (equal (diff-review-forge-read "PR: " (lambda () (error "Listed"))) 4711))))))

(ert-deftest diff-review-forge-test-not-a-pull-request ()
  "Pull and submit refuse a review of no pull request, and of a backend
that is not on the load path."
  (diff-review-test--with-repo
    (diff-review-test--open repo)
    (should-error (diff-review-forge-pull) :type 'user-error)
    (should-error (diff-review-forge-submit) :type 'user-error)
    (diff-review-forge-mark diff-review--data "backend" "nope" "number" 1)
    (should (equal (cadr (should-error (diff-review-forge-pull) :type 'user-error))
                   "No backend nope: diff-review-nope is not on the load path"))))

(provide 'diff-review-forge-test)
;;; diff-review-forge-test.el ends here
