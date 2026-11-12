;;; diff-review-github.el --- Review GitHub pull requests -*- lexical-binding: t; -*-

;; Author: Xiaoxing Hu <hi@xiaoxing.dev>
;; URL: https://github.com/xiaoxinghu/diff-review

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A pull request is a branch review whose threads are also on GitHub.
;; `diff-review-github-pr' fetches the pull request's head into
;; refs/pull/N and its base, and opens review/github/N.json under the
;; common git directory with that ref as the head, so the review follows
;; the pull request through its pushes.  GitHub's threads come in with
;; their lines counted where they were written, in the blob of the commit
;; they were on, and the review follows them to the tip as it does its
;; own.
;;
;; `diff-review-github-pull' fetches again and brings the threads up to
;; GitHub's.  `diff-review-github-submit' sends what was written here:
;; new threads as one review, the verdict its event, replies, and threads
;; resolved or reopened.  A thread GitHub cannot place on a line, outside
;; the pull request's hunks, goes as a comment on its file; one on a file
;; the pull request leaves alone, in the review's body.
;;
;; The gh command line does the talking, so its login is all the setup.
;; Whatever came from GitHub carries a "github" key in the review file,
;; and a thread without one has not been sent.

;;; Code:

(require 'diff-review)

(defcustom diff-review-github-program "gh"
  "The gh program: a name the variable `exec-path' finds, or a file."
  :type 'string
  :group 'diff-review)

(defcustom diff-review-github-remote "origin"
  "The remote whose GitHub repository has the pull requests, and serves them.
A fork's is usually \"upstream\"."
  :type 'string
  :group 'diff-review)

;;;; gh

(defun diff-review-github--repo (url)
  "The HOST/OWNER/REPO remote URL names, or nil for a URL with no host."
  (and (string-match (concat "\\`\\(?:[a-z+]+://\\)?\\(?:[^@/]+@\\)?\\([^:/]+\\)"
                             "\\(?::[0-9]+\\)?[:/]\\([^/]+/[^/]+?\\)\\(?:\\.git\\)?/?\\'")
                     url)
       (concat (match-string 1 url) "/" (match-string 2 url))))

(defun diff-review-github--gh (input &rest args)
  "Run gh with ARGS, INPUT on its stdin; its JSON, parsed, or nil for none.
`default-directory' is in the repository.  The GitHub repository is
`diff-review-github-remote''s, not the one gh would pick among several."
  (let* ((repo (diff-review-github--repo
                (diff-review--git-at default-directory "remote" "get-url"
                                     diff-review-github-remote)))
         (process-environment
          (append (list "GH_PROMPT_DISABLED=1")
                  (and repo (list (concat "GH_REPO=" repo)
                                  (concat "GH_HOST=" (car (split-string repo "/")))))
                  process-environment))
         (errors (make-temp-file "diff-review-gh")))
    (unwind-protect
        (with-temp-buffer
          (when input (insert input))
          (unless (eql 0 (apply #'call-process-region (point-min) (point-max)
                                diff-review-github-program t (list t errors) nil args))
            (user-error "GitHub: gh %s: %s" (string-join args " ")
                        (with-temp-buffer
                          (insert-file-contents errors)
                          (string-trim (buffer-string)))))
          (goto-char (point-min))
          (skip-chars-forward " \t\n")
          (and (not (eobp)) (json-parse-buffer :null-object nil :false-object nil)))
      (delete-file errors))))

(defun diff-review-github--graphql (query variables)
  "Run QUERY with VARIABLES, an alist, on GitHub; the response."
  (diff-review-github--gh (json-serialize `((query . ,query) (variables . ,variables)))
                          "api" "graphql" "--input" "-"))

(defun diff-review-github--post (endpoint body)
  "Post BODY, an alist, to the REST ENDPOINT; the response."
  (diff-review-github--gh (json-serialize body)
                          "api" "--method" "POST" endpoint "--input" "-"))

(defun diff-review-github--get (object &rest keys)
  "The value under KEYS in OBJECT, nested hash tables; nil where one is missing."
  (dolist (key keys object)
    (setq object (and (hash-table-p object) (gethash key object)))))

;;;; The pull request

(defun diff-review-github--fetch (number)
  "Fetch pull request NUMBER's head into refs/pull/NUMBER, and its base.
Returns the pull request, as gh shows it."
  (let ((pr (diff-review-github--gh nil "pr" "view" (number-to-string number)
                                    "--json" "id,baseRefName,reviews,comments"))
        (remote diff-review-github-remote))
    (diff-review--git-at default-directory "fetch" "--quiet" "--no-tags" remote
                         (format "+refs/pull/%d/head:refs/pull/%d" number number)
                         (format "+refs/heads/%1$s:refs/remotes/%2$s/%1$s"
                                 (gethash "baseRefName" pr) remote))
    pr))

(defconst diff-review-github--threads-query
  "query($id: ID!, $after: String) {
  node(id: $id) { ... on PullRequest {
    reviewThreads(first: 50, after: $after) {
      pageInfo { hasNextPage endCursor }
      nodes { id isResolved path diffSide originalLine originalStartLine
        comments(first: 100) { nodes {
          id body createdAt author { login } originalCommit { oid } } } } } } } }"
  "The review threads of a pull request, by its node id, a page at a time.")

(defconst diff-review-github--reply-mutation
  "mutation($thread: ID!, $body: String!) {
  addPullRequestReviewThreadReply(input: {pullRequestReviewThreadId: $thread, body: $body}) {
    comment { id } } }"
  "A reply to a review thread, by the thread's node id.")

(defun diff-review-github--threads (id)
  "The review threads of the pull request with node ID."
  (let (threads after more)
    (while (progn
             (let ((page (diff-review-github--get
                          (diff-review-github--graphql
                           diff-review-github--threads-query
                           `((id . ,id) (after . ,(or after :null))))
                          "data" "node" "reviewThreads")))
               (setq threads (append threads (gethash "nodes" page) nil)
                     after (diff-review-github--get page "pageInfo" "endCursor")
                     more (diff-review-github--get page "pageInfo" "hasNextPage")))
             more))
    threads))

(defun diff-review-github--number ()
  "The pull request's number.  The review buffer must be current."
  (or (diff-review-github--get diff-review--data "github" "number")
      (user-error "Not the review of a GitHub pull request")))

;;;; From GitHub

(defun diff-review-github--node (item)
  "The GitHub node id of ITEM, a thread or a reply, or nil."
  (diff-review-github--get item "github" "node"))

(defun diff-review-github--message (comment)
  "COMMENT from GitHub as a message of the review, a new hash table."
  (let ((m (make-hash-table :test #'equal))
        (github (make-hash-table :test #'equal)))
    (puthash "author" (or (diff-review-github--get comment "author" "login") "ghost") m)
    (puthash "at" (or (gethash "createdAt" comment) (gethash "submittedAt" comment)) m)
    (puthash "body" (or (gethash "body" comment) "") m)
    (puthash "node" (gethash "id" comment) github)
    (puthash "github" github m)
    m))

(defun diff-review-github--review-thread (thread base)
  "THREAD, a review thread from GitHub, as a thread of the review.
Its lines are counted where it was written: in the blob of the commit it
was on or, on a removed line, of that commit's merge base with BASE.  So
the review follows them to the tip, and says when they are outdated.
`default-directory' is the root."
  (let* ((comments (append (diff-review-github--get thread "comments" "nodes") nil))
         (c (diff-review-github--message (car comments)))
         (github (gethash "github" c))
         (path (gethash "path" thread))
         (last (gethash "originalLine" thread))
         (commit (diff-review-github--get (car comments) "originalCommit" "oid")))
    (puthash "path" path c)
    (when last
      (puthash "lines" (vector (or (gethash "originalStartLine" thread) last) last) c)
      (when-let* ((rev (if (equal (gethash "diffSide" thread) "LEFT")
                           (and commit (diff-review--git-maybe "merge-base" base commit))
                         commit))
                  (blob (diff-review--git-maybe "rev-parse" "--verify" "--quiet"
                                                (concat rev ":" path))))
        (puthash "blob" blob c)))
    (puthash "replies" (vconcat (mapcar #'diff-review-github--message (cdr comments))) c)
    (when (gethash "isResolved" thread) (puthash "resolved" t c))
    (puthash "thread" (gethash "id" thread) github)
    (puthash "resolved" (and (gethash "isResolved" thread) t) github)
    c))

(defun diff-review-github--conversation (pr)
  "PR's comments and its reviews' bodies, each a thread on the review as a whole."
  (sort (mapcar (lambda (comment)
                  (let ((c (diff-review-github--message comment)))
                    (puthash "replies" [] c)
                    c))
                (seq-filter (lambda (c)
                              (and (not (equal (gethash "state" c) "PENDING"))
                                   (not (string-empty-p (string-trim (or (gethash "body" c) ""))))))
                            (append (gethash "reviews" pr) (gethash "comments" pr) nil)))
        (lambda (a b) (string< (or (gethash "at" a) "") (or (gethash "at" b) "")))))

(defun diff-review-github--merge (local remote)
  "Bring LOCAL, a thread of the review, up to REMOTE, GitHub's copy; LOCAL.
What is on GitHub was written there, so its text wins, as does its
resolved state once it changed since the last pull.  Replies not sent stay."
  (let ((was (diff-review-github--get local "github" "resolved"))
        (now (diff-review-github--get remote "github" "resolved"))
        (line-thread (diff-review-github--get remote "github" "thread")))
    (dolist (key '("author" "at" "body" "path" "lines" "blob" "github"))
      (if-let* ((value (gethash key remote)))
          (puthash key value local)
        (remhash key local)))
    (when line-thread
      (unless (eq was now)
        (if now (puthash "resolved" t local) (remhash "resolved" local)))
      ;; A conversation item has no replies on GitHub: the ones sent are
      ;; comments of their own there, which the pull leaves out.
      (let ((mine (append (gethash "replies" local) nil)))
        (puthash "replies"
                 (vconcat
                  (mapcar (lambda (reply)
                            (if-let* ((same (seq-find (lambda (r)
                                                        (equal (diff-review-github--node r)
                                                               (diff-review-github--node reply)))
                                                      mine)))
                                (progn (dolist (key '("author" "at" "body" "github"))
                                         (puthash key (gethash key reply) same))
                                       same)
                              reply))
                          (gethash "replies" remote))
                  (seq-remove #'diff-review-github--node mine))
                 local)))
    local))

(defun diff-review-github--fetch-commits (threads)
  "Fetch the commits THREADS were written on that git has not.
A force push leaves them out of the pull request's head; GitHub still
serves them, or the threads draw where they were."
  (let ((missing (seq-remove
                  (lambda (oid) (diff-review--git-maybe "cat-file" "-e" (concat oid "^{commit}")))
                  (seq-uniq (delq nil (mapcar (lambda (thread)
                                                (diff-review-github--get
                                                 (seq-first (diff-review-github--get
                                                             thread "comments" "nodes"))
                                                 "originalCommit" "oid"))
                                              threads))))))
    (when missing
      (apply #'diff-review--git-maybe "fetch" "--quiet" "--no-tags" diff-review-github-remote
             missing))))

(defun diff-review-github--update (pr)
  "Bring the review up to PR, GitHub's view of it, write it and rescan.
GitHub's threads join the review or update their copies here.  A thread
from GitHub that it no longer has goes, as does one sent that comes back
as GitHub's own; one never sent stays.  The review buffer must be current."
  (let* ((default-directory diff-review--root)
         (base (concat diff-review-github-remote "/" (gethash "baseRefName" pr)))
         (threads (diff-review-github--threads (gethash "id" pr)))
         (theirs (make-hash-table :test #'equal))
         kept)
    (diff-review-github--fetch-commits threads)
    (let ((remote-threads (append (mapcar (lambda (thread)
                                            (diff-review-github--review-thread thread base))
                                          threads)
                                  (diff-review-github--conversation pr))))
      (dolist (r remote-threads)
        (puthash (diff-review-github--node r) r theirs))
      (unless (verify-visited-file-modtime) ; someone else wrote it: take theirs first
        (diff-review--revert))
      (let ((replied (mapcan (lambda (c)
                               (delq nil (mapcar #'diff-review-github--node
                                                 (gethash "replies" c))))
                             (append diff-review--comments nil))))
        (seq-doseq (c diff-review--comments)
          (let ((same (gethash (diff-review-github--node c) theirs)))
            (cond ((not (gethash "github" c)) (push c kept))
                  (same (push (diff-review-github--merge c same) kept)
                        (remhash (diff-review-github--node c) theirs)))))
        (dolist (r remote-threads)
          (let ((node (diff-review-github--node r)))
            (when (and (gethash node theirs) (not (member node replied)))
              (puthash "id" (diff-review--comment-id) r)
              (push r kept))))))
    (setq diff-review--comments (vconcat (nreverse kept)))
    (puthash "comments" diff-review--comments diff-review--data)
    (puthash "base" base diff-review--data)
    (diff-review--write)
    (revert-buffer)))

;;;; To GitHub

(defun diff-review-github--hunks (file)
  "The pull request's hunks in FILE at the tip, as GitHub draws them.
The new side, with three lines of context: (FIRST . LAST) each."
  (let ((text (apply #'diff-review--git "diff" "-U3" "-M"
                     (append (diff-review--range)
                             (list "--" (or (plist-get file :old) (plist-get file :path))
                                   (plist-get file :path)))))
        hunks)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward diff-hunk-header-re-unified nil t)
        (let ((start (diff-review--hunk-field 3))
              (count (diff-review--hunk-field 4)))
          (when (> count 0)
            (push (cons start (+ start count -1)) hunks)))))
    (nreverse hunks)))

(defun diff-review-github--placement (comment)
  "Where thread COMMENT goes on GitHub, as of the tip.
\(line FIRST . LAST) on lines inside one hunk, (file . LINES) on its file
otherwise, LINES as the tip has them or as written, nil for none; or
\(body) for a path the pull request leaves alone.  The review buffer must
be current."
  (let* ((path (gethash "path" comment))
         (index (and path (diff-review--file-index path)))
         (file (and index (diff-review--file index)))
         (written (diff-review--comment-lines comment))
         (blob (gethash "blob" comment))
         (tip-blob (diff-review--git-maybe "rev-parse" "--verify" "--quiet"
                                           (concat diff-review--tip ":" path)))
         (lines (and written
                     (if (and blob tip-blob (not (equal blob tip-blob)))
                         (let ((hunks (diff-review--hunks-between blob tip-blob)))
                           (and (listp hunks) (diff-review--map-lines hunks written)))
                       written))))
    (cond ((or (null file) (equal (plist-get file :status) " ")) '(body))
          ((and lines tip-blob
                (seq-some (lambda (h) (and (<= (car h) (car lines)) (<= (cdr lines) (cdr h))))
                          (diff-review-github--hunks file)))
           (cons 'line lines))
          (t (cons 'file (or lines written))))))

(defun diff-review-github--text (comment &optional where)
  "The text of COMMENT for GitHub, its replies after it, WHERE before it."
  (string-join
   (cons (concat where (gethash "body" comment))
         (mapcar (lambda (r) (format "**%s**: %s" (gethash "author" r) (gethash "body" r)))
                 (gethash "replies" comment)))
   "\n\n"))

(defun diff-review-github--where (path lines)
  "PATH and LINES, (FIRST . LAST) or nil, as a body begins with them."
  (concat (and path (format "`%s` " path))
          (cond ((null lines) "")
                ((= (car lines) (cdr lines)) (format "line %d" (car lines)))
                (t (format "lines %d-%d" (car lines) (cdr lines))))
          (and (or path lines) ": ")))

(defun diff-review-github--mark (item &rest pairs)
  "Put PAIRS, keys and values, in ITEM's \"github\" table, made when missing."
  (let ((github (or (gethash "github" item)
                    (puthash "github" (make-hash-table :test #'equal) item))))
    (while pairs
      (puthash (pop pairs) (pop pairs) github))))

(defun diff-review-github--event ()
  "The review event the verdict asks for, or nil when GitHub has it already.
A stale verdict asks for none.  The review buffer must be current."
  (let* ((verdict (gethash "verdict" diff-review--data))
         (sent (and (hash-table-p verdict) (gethash "github" verdict))))
    (and (hash-table-p verdict)
         (not (eq (diff-review--turn) 'reviewer))
         (not (and (hash-table-p sent)
                   (equal (gethash "state" sent) (gethash "state" verdict))
                   (equal (gethash "head" sent) (gethash "head" verdict))))
         (pcase (gethash "state" verdict)
           ("approved" "APPROVE")
           ("changes" "REQUEST_CHANGES")))))

;;;; Commands

(defun diff-review-github--read-pr ()
  "Read a pull request of `diff-review-github-remote''s repository: its number."
  (let* ((default-directory (diff-review--root-here))
         (prs (diff-review-github--gh nil "pr" "list" "--json"
                                      "number,title,headRefName,author" "--limit" "100"))
         (choice (completing-read
                  "Review pull request: "
                  (mapcar (lambda (pr)
                            (format "#%d %s  (%s, %s)" (gethash "number" pr)
                                    (gethash "title" pr) (gethash "headRefName" pr)
                                    (diff-review-github--get pr "author" "login")))
                          prs))))
    (if (string-match "\\`\\s-*#?\\([0-9]+\\)" choice)
        (string-to-number (match-string 1 choice))
      (user-error "No pull request number in %S" choice))))

;;;###autoload
(defun diff-review-github-pr (number)
  "Review pull request NUMBER of `diff-review-github-remote''s GitHub repository.
Interactively, pick among the open ones, or type a number.  Its head is
fetched into refs/pull/NUMBER, and the review file is
review/github/NUMBER.json under the common git directory, its head that
ref and its base the pull request's, so every worktree opens the same
review.  GitHub's threads join it; one already open is pulled again."
  (interactive (list (diff-review-github--read-pr)))
  (let* ((root (diff-review--root-here))
         ;; The main worktree as the root: the file outlives the worktree it was made from.
         (default-directory (or (diff-review--main-worktree root) root))
         ;; Before the file opens: its scan needs the head.
         (pr (diff-review-github--fetch number)))
    (diff-review-find-file (expand-file-name (format "review/github/%d.json" number)
                                             (diff-review--common-dir default-directory))
                           default-directory
                           (concat diff-review-github-remote "/" (gethash "baseRefName" pr))
                           (format "pull/%d" number))
    (diff-review-github--mark diff-review--data "number" number)
    (diff-review-github--update pr)))

;;;###autoload
(defun diff-review-github-pull ()
  "Fetch the pull request again and bring the review's threads up to GitHub's.
GitHub's text wins, and its resolved state once it changed there; a
thread or reply not sent yet stays."
  (interactive)
  (diff-review--in-review
    (let ((default-directory diff-review--root)
          (number (diff-review-github--number)))
      (diff-review-github--update (diff-review-github--fetch number))
      (message "Pulled #%d" number))))

;;;###autoload
(defun diff-review-github-submit ()
  "Send what was written here to the pull request, then pull it again.
New threads go as one review, on the tip the review shows, its event the
verdict when GitHub does not have it yet; review-wide threads are its
body.  A thread outside the pull request's hunks goes on its file, its
lines in its text; one on a file the pull request leaves alone, in the
body.  Replies follow their threads, and resolving or reopening a thread
does it on GitHub.  Asks first."
  (interactive)
  (diff-review--in-review
    (unless (verify-visited-file-modtime) ; someone else wrote it: take theirs first
      (diff-review--revert))
    (let* ((default-directory diff-review--root)
           (number (diff-review-github--number))
           (prefix (format "repos/{owner}/{repo}/pulls/%d" number))
           (comments (append diff-review--comments nil))
           (new (seq-remove (lambda (c) (gethash "github" c)) comments))
           (sent (seq-filter #'diff-review-github--node comments))
           ;; A thread sent on its file this time round is no review thread
           ;; here until the pull after: its replies wait for that.
           (replies (mapcan (lambda (c)
                              (and (or (diff-review-github--get c "github" "thread")
                                       (not (gethash "path" c)))
                                   (mapcar (lambda (r) (cons c r))
                                           (seq-remove (lambda (r) (gethash "github" r))
                                                       (gethash "replies" c)))))
                            sent))
           (resolves (seq-filter (lambda (c)
                                   (and (diff-review-github--get c "github" "thread")
                                        (not (eq (and (gethash "resolved" c) t)
                                                 (diff-review-github--get
                                                  c "github" "resolved")))))
                                 sent))
           (event (diff-review-github--event))
           (placed (mapcar (lambda (c) (cons c (and (gethash "path" c)
                                                    (diff-review-github--placement c))))
                           new))
           (lines (seq-filter (lambda (p) (eq (cadr p) 'line)) placed))
           (files (seq-filter (lambda (p) (eq (cadr p) 'file)) placed))
           (body (seq-remove (lambda (p) (memq (cadr p) '(line file))) placed)))
      (unless (or new replies resolves event)
        (user-error "Nothing to send to #%d" number))
      (unless (y-or-n-p (format "Send to #%d: %d new threads, %d replies, %d resolved or \
reopened%s? " number (length new) (length replies) (length resolves)
                                (if event (concat ", " (string-replace "_" " " (downcase event))) "")))
        (user-error "Not sent"))
      ;; Each send is marked as it goes, and the marks written whatever
      ;; fails, so nothing is sent twice.
      (unwind-protect
          (progn
            (pcase-dolist (`(,thread . ,reply) replies)
              (diff-review-github--mark
               reply "node"
               (if-let* ((id (diff-review-github--get thread "github" "thread")))
                   ;; By the thread's node: REST's comment ids outgrew GraphQL's
                   ;; databaseId, which reads null for new ones.
                   (diff-review-github--get
                    (diff-review-github--graphql
                     diff-review-github--reply-mutation
                     `((thread . ,id) (body . ,(gethash "body" reply))))
                    "data" "addPullRequestReviewThreadReply" "comment" "id")
                 ;; A conversation item takes no replies: quote it, as GitHub does.
                 (gethash "node_id"
                          (diff-review-github--post
                           (format "repos/{owner}/{repo}/issues/%d/comments" number)
                           `((body . ,(concat (replace-regexp-in-string
                                               "^" "> " (string-trim (gethash "body" thread)))
                                              "\n\n" (gethash "body" reply)))))))))
            (dolist (thread resolves)
              (let ((resolved (and (gethash "resolved" thread) t)))
                (diff-review-github--graphql
                 (format "mutation($id: ID!) { %s(input: {threadId: $id}) { thread { id } } }"
                         (if resolved "resolveReviewThread" "unresolveReviewThread"))
                 `((id . ,(diff-review-github--get thread "github" "thread"))))
                (diff-review-github--mark thread "resolved" resolved)))
            (pcase-dolist (`(,thread file . ,at) files)
              (let ((response (diff-review-github--post
                               (concat prefix "/comments")
                               `((body . ,(diff-review-github--text
                                           thread (diff-review-github--where nil at)))
                                 (commit_id . ,diff-review--tip)
                                 (path . ,(gethash "path" thread))
                                 (subject_type . "file")))))
                (diff-review-github--mark thread "node" (gethash "node_id" response))))
            (when (or lines body event)
              (let ((response
                     (diff-review-github--post
                      (concat prefix "/reviews")
                      `((commit_id . ,diff-review--tip)
                        (event . ,(or event "COMMENT"))
                        ,@(and body
                               `((body . ,(mapconcat
                                           (pcase-lambda (`(,c . ,_))
                                             (diff-review-github--text
                                              c (diff-review-github--where
                                                 (gethash "path" c)
                                                 (diff-review--comment-lines c))))
                                           body "\n\n"))))
                        (comments
                         . ,(vconcat
                             (mapcar (pcase-lambda (`(,c line ,first . ,last))
                                       `((path . ,(gethash "path" c))
                                         (body . ,(diff-review-github--text c))
                                         (line . ,last) (side . "RIGHT")
                                         ,@(and (/= first last)
                                                `((start_line . ,first)
                                                  (start_side . "RIGHT")))))
                                     lines)))))))
                (dolist (p (append lines body))
                  (diff-review-github--mark (car p) "review" (gethash "node_id" response)))
                (when event
                  (let ((verdict (gethash "verdict" diff-review--data))
                        (mark (make-hash-table :test #'equal)))
                    (puthash "state" (gethash "state" verdict) mark)
                    (puthash "head" (gethash "head" verdict) mark)
                    (puthash "github" mark verdict))))))
        (diff-review--write))
      (diff-review-github-pull)
      (message "Sent to #%d" number))))

(provide 'diff-review-github)
;;; diff-review-github.el ends here
