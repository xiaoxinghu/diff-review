;;; diff-review-github-test.el --- Tests for diff-review-github  -*- lexical-binding: t; -*-

;;; Commentary:

;; GitHub is a fake gh, in Lisp, that serves a pull request and its
;; threads and records what is sent; the pull request is
;; diff-review-forge-test.el's, its head at refs/pull/5/head.

;;; Code:

(require 'ert)
(require 'diff-review-forge-test
         (expand-file-name "diff-review-forge-test.el"
                           (file-name-directory (or load-file-name buffer-file-name))))
(require 'diff-review-github)

(defvar diff-review-github-test--pr nil "What the fake gh's pr view prints, as Lisp.")
(defvar diff-review-github-test--threads nil "The fake pull request's review threads, as Lisp.")
(defvar diff-review-github-test--calls nil "The fake gh's calls, newest first: (ARGS . INPUT).")

(defun diff-review-github-test--gh (input &rest args)
  "The fake gh: answer ARGS, recording them with INPUT, parsed."
  (let ((sent (and input (json-parse-string input :null-object nil :false-object nil))))
    (push (cons args sent) diff-review-github-test--calls)
    (pcase args
      (`("pr" "view" . ,_) (diff-review-forge-test--json diff-review-github-test--pr))
      (`("api" "graphql" . ,_)
       (diff-review-forge-test--json
        (if (string-prefix-p "mutation" (gethash "query" sent))
            '((data . ((addPullRequestReviewThreadReply . ((comment . ((id . "NR")))))
                    (resolveReviewThread . ((thread . ((id . "T1"))))))))
          `((data . ((node . ((reviewThreads
                               . ((pageInfo . ((hasNextPage . :false)))
                                  (nodes . ,(vconcat diff-review-github-test--threads))))))))))))
      (`("api" "--method" "POST" . ,_)
       (let ((n (length diff-review-github-test--calls)))
         (diff-review-forge-test--json `((id . ,n) (node_id . ,(format "N%d" n)))))))))

(defun diff-review-github-test--thread (id line resolved commit &rest comments)
  "A review thread ID on long.txt's LINE, nil for its file, written on COMMIT.
COMMENTS are (NODE AUTHOR BODY).  No databaseId: GitHub's reads null
for a comment whose id outgrew it, as new ones have."
  `((id . ,id) (isResolved . ,(if resolved t :false)) (path . "long.txt") (diffSide . "RIGHT")
    (originalLine . ,(or line :null)) (originalStartLine . :null)
    (comments
     . ((nodes
         . ,(vconcat
             (mapcar (pcase-lambda (`(,node ,author ,body))
                       `((id . ,node) (body . ,body)
                         (createdAt . "2026-11-01T10:00:00Z") (author . ((login . ,author)))
                         (originalCommit . ((oid . ,commit)))))
                     comments)))))))

(defmacro diff-review-github-test--with-pr (&rest body)
  "Run BODY with pull request 5 on a bare origin, `head' its tip, gh faked."
  (declare (indent 0))
  `(diff-review-forge-test--with-pr "refs/pull/5/head"
     (let ((diff-review-github-test--calls nil)
           (diff-review-github-test--pr
            `((id . "PR5") (baseRefName . "main")
              (reviews . [((id . "R1") (author . ((login . "alice"))) (body . "Overall fine.")
                           (state . "COMMENTED") (submittedAt . "2026-11-01T11:00:00Z"))
                          ((id . "R2") (author . ((login . "alice"))) (body . "")
                           (state . "APPROVED") (submittedAt . "2026-11-01T12:00:00Z"))])
              (comments . [((id . "IC1") (author . ((login . "bob"))) (body . "Ping.")
                            (createdAt . "2026-11-01T09:00:00Z"))])))
           (diff-review-github-test--threads
            (list (diff-review-github-test--thread "T1" 2 nil head
                                                   '("C1" "alice" "Why shout?")
                                                   '("C2" "bob" "Because."))
                  (diff-review-github-test--thread "T2" nil nil head
                                                   '("C3" "alice" "Whole file.")))))
       (cl-letf (((symbol-function 'diff-review-github--gh) #'diff-review-github-test--gh))
         ,@body))))

(defun diff-review-github-test--sent (endpoint)
  "What the fake gh was last sent at ENDPOINT, or by the graphql query naming it."
  (cdr (seq-find (lambda (call)
                   (or (member endpoint (car call))
                       (and (cdr call) (string-match-p (regexp-quote endpoint)
                                                       (or (gethash "query" (cdr call)) "")))))
                 diff-review-github-test--calls)))

(ert-deftest diff-review-github-test-gh-on-the-remote-s-repo ()
  "Gh runs on the remote's repository, its input on stdin; a failure says what gh said."
  (let* ((dir (make-temp-file "diff-review-gh" t))
         (gh (expand-file-name "gh" dir))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (progn
          (with-temp-file gh
            (insert "#!/bin/sh\n"
                    "[ \"$1\" = fail ] && { echo 'no such pull request' >&2; exit 1; }\n"
                    "printf '{\"repo\": \"%s\", \"host\": \"%s\", \"args\": \"%s\", \"in\": \"%s\"}'"
                    " \"$GH_REPO\" \"$GH_HOST\" \"$*\" \"$(cat)\"\n"))
          (set-file-modes gh #o755)
          (diff-review-test--git "init" "-q")
          (diff-review-test--git "remote" "add" "up" "git@github.com:o/r.git")
          (let* ((diff-review-github-program gh)
                 (diff-review-github-remote "up")
                 (out (diff-review-github--gh "hello" "api" "x")))
            (should (equal (gethash "repo" out) "github.com/o/r"))
            (should (equal (gethash "host" out) "github.com"))
            (should (equal (gethash "args" out) "api x"))
            (should (equal (gethash "in" out) "hello"))
            (should (string-match-p "no such pull request"
                                    (cadr (should-error (diff-review-github--gh nil "fail")
                                                        :type 'user-error))))))
      (delete-directory dir t))))

(ert-deftest diff-review-github-test-open-a-pull-request ()
  "A pull request opens as a review of its fetched head against its base,
with GitHub's threads on their lines, its file and the review; open again,
it is the same review, pulled again."
  (diff-review-github-test--with-pr
    (diff-review-github-pr 5)
    (should (derived-mode-p 'diff-review-mode))
    (should (diff-review-test--same-file-p
             buffer-file-name
             (expand-file-name "review/github/5.json" (diff-review--common-dir repo))))
    (should (equal diff-review--head "pull/5"))
    (should (equal diff-review--tip head))
    (should (equal (gethash "base" diff-review--data) "origin/main"))
    (should (equal (diff-review-forge--get diff-review--data "forge" "number") 5))
    (should (equal (mapcar (lambda (f) (plist-get f :path)) diff-review--files) '("long.txt")))
    ;; R2 is an approval with no body: nothing to read.
    (should (equal (mapcar #'diff-review-forge--node diff-review--comments)
                   '("C1" "C3" "IC1" "R1")))
    (let ((t1 (diff-review-forge-test--by-node "C1")))
      (should (equal (gethash "lines" t1) [2 2]))
      (should (equal (gethash "blob" t1) (diff-review--git "rev-parse" (concat head ":long.txt"))))
      (should (equal (gethash "author" t1) "alice"))
      (should (equal (mapcar (lambda (r) (gethash "body" r)) (gethash "replies" t1)) '("Because.")))
      (should-not (gethash "resolved" t1))
      (should (equal (diff-review-forge--get t1 "forge" "thread") "T1")))

    (let ((t2 (diff-review-forge-test--by-node "C3")))
      (should (equal (gethash "path" t2) "long.txt"))
      (should-not (gethash "lines" t2)))
    (should-not (gethash "path" (diff-review-forge-test--by-node "R1")))
    (should (string-match-p "long.txt:2  alice  Why shout\\?  \\+1" (buffer-string)))
    (kill-buffer)
    (let ((default-directory repo))
      (diff-review-github-pr 5))
    (should (equal (length diff-review--comments) 4))))

(ert-deftest diff-review-github-test-pull-merges ()
  "A pull takes GitHub's new threads, replies and resolved changes, drops the
threads GitHub dropped, keeps what was not sent, and follows a push."
  (diff-review-github-test--with-pr
    (diff-review-github-pr 5)
    (let ((t1 (diff-review-forge-test--by-node "C1")))
      (diff-review--change-comment
       t1 (lambda (c)
            (puthash "resolved" t c)
            (puthash "replies" (vconcat (gethash "replies" c)
                                        (vector (diff-review--message "me" "Not yet sent.")))
                     c))))
    (diff-review--add-comment nil nil nil "Mine, not sent.")
    (diff-review-test--commit-in (concat (directory-file-name repo) "-feature/")
                                 "new.txt" "n\n" "push")
    (diff-review-test--git "push" "-q" "origin" "feature:refs/pull/5/head")
    (setq diff-review-github-test--threads
          (list (diff-review-github-test--thread "T1" 2 nil head
                                                 '("C1" "alice" "Why shout?")
                                                 '("C2" "bob" "Because.")
                                                 '("C4" "carol" "Agreed."))
                (diff-review-github-test--thread "T3" 4 t head '("C5" "carol" "Done."))))
    (diff-review-forge-pull)
    (should (equal (diff-review--git "rev-parse" "pull/5") diff-review--tip))
    (should-not (equal diff-review--tip head))
    (should (equal (mapcar (lambda (f) (plist-get f :path)) diff-review--files)
                   '("long.txt" "new.txt")))
    (should-not (diff-review-forge-test--by-node "C3"))
    (let ((t1 (diff-review-forge-test--by-node "C1")))
      (should (gethash "resolved" t1))  ; resolved here, unchanged on GitHub
      (should (equal (mapcar (lambda (r) (gethash "body" r)) (gethash "replies" t1))
                     '("Because." "Agreed." "Not yet sent."))))
    (should (gethash "resolved" (diff-review-forge-test--by-node "C5")))
    (should (seq-find (lambda (c) (equal (gethash "body" c) "Mine, not sent."))
                      diff-review--comments))
    ;; Reopened on GitHub: GitHub's change wins.
    (setf (alist-get 'isResolved (cadr diff-review-github-test--threads)) :false)
    (diff-review-forge-pull)
    (should-not (gethash "resolved" (diff-review-forge-test--by-node "C5")))))

(ert-deftest diff-review-github-test-submit ()
  "Submit sends new threads as one review with the verdict, a thread outside
the hunks on its file, one on a file the pull request leaves alone in the
body, replies and a resolve; then there is nothing left to send."
  (diff-review-github-test--with-pr
    (diff-review-github-pr 5)
    (let ((blob (diff-review--git "rev-parse" (concat head ":long.txt"))))
      (diff-review--add-comment "long.txt" '(2 . 3) blob "In the hunk.")
      (diff-review--add-comment "long.txt" '(15 . 15) blob "Far from it.")
      (diff-review--add-comment "a.txt" '(1 . 1) (diff-review--git "rev-parse" (concat head ":a.txt"))
                                "Not in the pull request.")
      (diff-review--add-comment nil nil nil "Overall good."))
    (diff-review--change-comment
     (diff-review-forge-test--by-node "C1")
     (lambda (c)
       (puthash "resolved" t c)
       (puthash "replies" (vconcat (gethash "replies" c) (vector (diff-review--message "me" "Ok.")))
                c)))
    (diff-review--change-comment
     (diff-review-forge-test--by-node "IC1")
     (lambda (c) (puthash "replies" (vector (diff-review--message "me" "Pong.")) c)))
    (diff-review-approve)
    (diff-review-forge-submit)
    (let ((review (diff-review-github-test--sent "repos/{owner}/{repo}/pulls/5/reviews")))
      (should (equal (gethash "event" review) "APPROVE"))
      (should (equal (gethash "commit_id" review) head))
      (should (equal (gethash "body" review)
                     "`a.txt` line 1: Not in the pull request.\n\nOverall good."))
      (should (equal (json-serialize (gethash "comments" review))
                     (json-serialize
                      [((path . "long.txt") (body . "In the hunk.") (line . 3) (side . "RIGHT")
                        (start_line . 2) (start_side . "RIGHT"))]))))
    (let ((file (diff-review-github-test--sent "repos/{owner}/{repo}/pulls/5/comments")))
      (should (equal (gethash "subject_type" file) "file"))
      (should (equal (gethash "path" file) "long.txt"))
      (should (equal (gethash "body" file) "line 15: Far from it.")))
    ;; A thread's reply goes to the thread; only the conversation's is a comment.
    (let ((reply (diff-review-github-test--sent "addPullRequestReviewThreadReply")))
      (should (equal (diff-review-forge--get reply "variables" "thread") "T1"))
      (should (equal (diff-review-forge--get reply "variables" "body") "Ok.")))
    (should (equal (mapcar (lambda (call) (gethash "body" (cdr call)))
                           (seq-filter (lambda (call)
                                         (member "repos/{owner}/{repo}/issues/5/comments"
                                                 (car call)))
                                       diff-review-github-test--calls))
                   '("> Ping.\n\nPong.")))
    (should (equal (diff-review-forge--get (diff-review-github-test--sent "resolveReviewThread")
                                            "variables" "id")
                   "T1"))
    (should (equal (diff-review-forge--get diff-review--data "verdict" "forge" "state")
                   "approved"))
    ;; The fake GitHub kept none of it, so the pull dropped what was sent.
    (should (equal (mapcar #'diff-review-forge--node diff-review--comments)
                   '("C1" "C3" "IC1" "R1")))
    (should (equal (cadr (should-error (diff-review-forge-submit) :type 'user-error))
                   "Nothing to send to #5"))))

;;; diff-review-github-test.el ends here
