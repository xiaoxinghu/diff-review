;;; diff-review-gitlab-test.el --- Tests for diff-review-gitlab  -*- lexical-binding: t; -*-

;;; Commentary:

;; GitLab is a fake of its API, in Lisp, that serves a merge request and
;; its discussions and records what is sent; the merge request is
;; diff-review-forge-test.el's, its head at refs/merge-requests/7/head.

;;; Code:

(require 'ert)
(require 'diff-review-forge-test
         (expand-file-name "diff-review-forge-test.el"
                           (file-name-directory (or load-file-name buffer-file-name))))
(require 'diff-review-gitlab)

(defvar diff-review-gitlab-test--discussions nil "The fake merge request's discussions, as Lisp.")
(defvar diff-review-gitlab-test--calls nil "The fake API's calls, newest first: (METHOD PATH BODY).")
(defvar diff-review-gitlab-test--approvers nil "Who approved the fake merge request.")

(defun diff-review-gitlab-test--api (method path &optional body _pages)
  "The fake GitLab API: answer METHOD on PATH, recording them with BODY."
  (push (list method path (and body (diff-review-forge-test--json body)))
        diff-review-gitlab-test--calls)
  (diff-review-forge-test--json
   (pcase (list method path)
     ('("GET" "merge_requests/7") '((iid . 7) (target_branch . "main")))
     ('("GET" "/user") '((username . "me")))
     ('("GET" "merge_requests/7/approvals")
      `((approved_by . ,(vconcat (mapcar (lambda (by) `((user . ((username . ,by)))))
                                         diff-review-gitlab-test--approvers)))))
     ('("GET" "merge_requests/7/discussions?per_page=100")
      (vconcat diff-review-gitlab-test--discussions))
     ('("GET" "merge_requests/7/versions")
      (let ((base (diff-review--git "rev-parse" "origin/main")))
        (vector `((head_commit_sha . ,(diff-review--git "rev-parse" "merge-requests/7"))
                  (base_commit_sha . ,base) (start_commit_sha . ,base)))))
     ('("POST" "merge_requests/7/discussions")
      `((id . "DN") (notes . [((id . ,(+ 900 (length diff-review-gitlab-test--calls))))])))
     (`("POST" ,(pred (string-suffix-p "/notes"))) '((id . 990)))
     (_ '((ok . t))))))

(defun diff-review-gitlab-test--note (id author body &rest more)
  "A note ID by AUTHOR saying BODY, with MORE fields."
  `((id . ,id) (author . ((username . ,author))) (body . ,body)
    (created_at . "2026-11-01T10:00:00Z") ,@more))

(defmacro diff-review-gitlab-test--with-mr (&rest body)
  "Run BODY with merge request 7 on a bare origin, `head' its tip, GitLab faked.
Its discussions: D1 on line 2 at the head, with a reply; D2 resolved, on
the line 2 the base had; D3 on no line; D4 only system notes."
  (declare (indent 0))
  `(diff-review-forge-test--with-pr "refs/merge-requests/7/head"
     (let* ((base (diff-review--git-at repo "rev-parse" "main"))
            (at (lambda (side line)
                  `((position . ((base_sha . ,base) (start_sha . ,base) (head_sha . ,head)
                                 (old_path . "long.txt") (new_path . "long.txt")
                                 (position_type . "text") (,side . ,line))))))
            (diff-review-gitlab-test--calls nil)
            (diff-review-gitlab-test--approvers '("alice"))
            (diff-review-gitlab-test--discussions
             `(((id . "D1")
                (notes . [,(apply #'diff-review-gitlab-test--note 101 "alice" "Why shout?"
                                  '(resolvable . t) '(resolved . :false)
                                  (funcall at 'new_line 2))
                          ,(diff-review-gitlab-test--note 102 "bob" "Because.")
                          ,(diff-review-gitlab-test--note 100 "ci" "added 1 commit"
                                                          '(system . t))]))
               ((id . "D2")
                (notes . [,(apply #'diff-review-gitlab-test--note 103 "alice" "Why drop l2?"
                                  '(resolvable . t) '(resolved . t)
                                  (funcall at 'old_line 2))]))
               ((id . "D3") (individual_note . t)
                (notes . [,(diff-review-gitlab-test--note 104 "carol" "Ping.")]))
               ((id . "D4")
                (notes . [,(diff-review-gitlab-test--note 105 "ci" "changed the title"
                                                          '(system . t))])))))
       (cl-letf (((symbol-function 'diff-review-gitlab--api) #'diff-review-gitlab-test--api))
         ,@body))))

(defun diff-review-gitlab-test--sent (method path)
  "What the fake API was last sent by METHOD at PATH."
  (nth 2 (seq-find (lambda (call) (equal (list (car call) (cadr call)) (list method path)))
                   diff-review-gitlab-test--calls)))

(ert-deftest diff-review-gitlab-test-open-a-merge-request ()
  "A merge request opens as a review of its fetched head against its target,
with its discussions on their lines, each side's, and on the review; its
system notes left out."
  (diff-review-gitlab-test--with-mr
    (diff-review-gitlab-mr 7)
    (should (derived-mode-p 'diff-review-mode))
    (should (diff-review-test--same-file-p
             buffer-file-name
             (expand-file-name "review/gitlab/7.json" (diff-review--common-dir repo))))
    (should (equal diff-review--head "merge-requests/7"))
    (should (equal diff-review--tip head))
    (should (equal (gethash "base" diff-review--data) "origin/main"))
    (should (equal (diff-review-forge--get diff-review--data "forge" "backend") "gitlab"))
    (should (equal (mapcar #'diff-review-forge--node diff-review--comments) '(101 103 104)))
    (let ((d1 (diff-review-forge-test--by-node 101)))
      (should (equal (gethash "lines" d1) [2 2]))
      (should (equal (gethash "blob" d1) (diff-review--git "rev-parse" (concat head ":long.txt"))))
      (should (equal (mapcar (lambda (r) (gethash "body" r)) (gethash "replies" d1)) '("Because.")))
      (should (equal (diff-review-forge--get d1 "forge" "thread") "D1"))
      (should-not (gethash "resolved" d1)))
    (let ((d2 (diff-review-forge-test--by-node 103)))
      (should (equal (gethash "blob" d2) (diff-review--git "rev-parse" (concat base ":long.txt"))))
      (should (gethash "resolved" d2)))
    (let ((d3 (diff-review-forge-test--by-node 104)))
      (should-not (gethash "path" d3))
      (should (equal (diff-review-forge--get d3 "forge" "thread") "D3"))
      ;; Not resolvable: nothing to resolve on GitLab.
      (should (eq (gethash "resolved" (gethash "forge" d3) 'none) 'none)))))

(ert-deftest diff-review-gitlab-test-submit ()
  "Submit places new threads on their lines by both sides' numbers, an added
line by the new side's alone, a file the merge request leaves alone in the
text; replies to any discussion, resolves, approves; asking for changes
then revokes the approval."
  (diff-review-gitlab-test--with-mr
    (diff-review-gitlab-mr 7)
    (let ((blob (diff-review--git "rev-parse" (concat head ":long.txt"))))
      (diff-review--add-comment "long.txt" '(2 . 2) blob "Added.")
      (diff-review--add-comment "long.txt" '(14 . 15) blob "Far from it.")
      (diff-review--add-comment "a.txt" '(1 . 1) (diff-review--git "rev-parse" (concat head ":a.txt"))
                                "Not in the merge request.")
      (diff-review--add-comment nil nil nil "Overall good."))
    (diff-review--change-comment (diff-review-forge-test--by-node 101)
                                 (lambda (c) (puthash "resolved" t c)))
    (diff-review--change-comment
     (diff-review-forge-test--by-node 104)
     (lambda (c) (puthash "replies" (vector (diff-review--message "me" "Pong.")) c)))
    (diff-review-approve)
    (diff-review-forge-submit)
    (let ((posted (mapcar #'caddr (seq-filter (lambda (call)
                                                (equal (cadr call) "merge_requests/7/discussions"))
                                              diff-review-gitlab-test--calls))))
      (should (equal (mapcar (lambda (d) (gethash "body" d)) (reverse posted))
                     '("Added." "lines 14-15: Far from it." "`a.txt` line 1: Not in the merge request."
                       "Overall good.")))
      (let ((added (diff-review-forge--get (car (last posted)) "position"))
            (far (diff-review-forge--get (cadr (reverse posted)) "position")))
        (should (equal (gethash "position_type" added) "text"))
        (should (equal (gethash "new_line" added) 2))
        (should-not (gethash "old_line" added))
        (should (equal (gethash "head_sha" added) head))
        (should (equal (gethash "base_sha" added) base))
        (should (equal (gethash "new_line" far) 15))
        (should (equal (gethash "old_line" far) 15)))
      (should-not (seq-some (lambda (d) (gethash "position" d)) (seq-take posted 2))))
    (should (equal (gethash "body" (diff-review-gitlab-test--sent
                                    "POST" "merge_requests/7/discussions/D3/notes"))
                   "Pong."))
    (should (eq (gethash "resolved" (diff-review-gitlab-test--sent
                                     "PUT" "merge_requests/7/discussions/D1"))
                t))
    (should (assoc "merge_requests/7/approve" (mapcar #'cdr diff-review-gitlab-test--calls)))
    (should (equal (cadr (should-error (diff-review-forge-submit) :type 'user-error))
                   "Nothing to send to #7"))
    (diff-review-request-changes)
    (diff-review-forge-submit)
    (should (assoc "merge_requests/7/unapprove" (mapcar #'cdr diff-review-gitlab-test--calls)))))

(ert-deftest diff-review-gitlab-test-outdated-goes-on-its-file ()
  "A thread whose lines a push changed before it was sent goes on its file,
its lines as written in its text, on the version of the new tip."
  (diff-review-gitlab-test--with-mr
    (diff-review-gitlab-mr 7)
    (diff-review--add-comment "long.txt" '(2 . 2) (diff-review--git "rev-parse" (concat head ":long.txt"))
                              "Shouting.")
    (diff-review-test--commit-in feature "long.txt"
                                 (string-replace "l2\n" "LL2\n" long)
                                 "louder")
    (diff-review-test--git "push" "-q" "origin" "feature:refs/merge-requests/7/head")
    (diff-review-forge-pull)
    (diff-review-forge-submit)
    (let* ((posted (diff-review-gitlab-test--sent "POST" "merge_requests/7/discussions"))
           (position (gethash "position" posted)))
      (should (equal (gethash "position_type" position) "file"))
      (should (equal (gethash "head_sha" position) diff-review--tip))
      (should (equal (gethash "body" posted) "line 2: Shouting.")))))

(ert-deftest diff-review-gitlab-test-approvals ()
  "Others' approvals show in the summary; the reviewer's own becomes the
review's verdict, on the tip, and is not sent again."
  (diff-review-gitlab-test--with-mr
    (setq diff-review-gitlab-test--approvers '("alice" "me"))
    (diff-review-gitlab-mr 7)
    (should (string-match-p "  alice approved$" (diff-review--summary)))
    (should (equal (diff-review-forge--get diff-review--data "verdict" "state") "approved"))
    (should (equal (diff-review-forge--get diff-review--data "verdict" "head") head))
    (should (equal (cadr (should-error (diff-review-forge-submit) :type 'user-error))
                   "Nothing to send to #7"))))

;;; diff-review-gitlab-test.el ends here
