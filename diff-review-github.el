;;; diff-review-github.el --- Review GitHub pull requests -*- lexical-binding: t; -*-

;; Author: Xiaoxing Hu <hi@xiaoxing.dev>
;; URL: https://github.com/xiaoxinghu/diff-review

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The GitHub backend of diff-review-forge.el.  `diff-review-github-pr'
;; reviews a pull request: its head is fetched into refs/pull/N, and its
;; review threads come in with their lines counted where they were
;; written, in the blob of the commit they were on, so the review follows
;; them to the tip as it does its own.  Its comments and its reviews'
;; bodies are threads on the review as a whole.
;;
;; Sent, new threads go as one review, the verdict its event.  GitHub
;; takes a line comment only inside the pull request's hunks, so a thread
;; outside them goes as a comment on its file, and one on a file the pull
;; request leaves alone in the review's body.
;;
;; The gh command line does the talking, so its login is all the setup.

;;; Code:

(require 'diff-review-forge)

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

(defun diff-review-github--gh (input &rest args)
  "Run gh with ARGS, INPUT on its stdin; its JSON, parsed, or nil for none.
`default-directory' is in the repository.  The GitHub repository is
`diff-review-github-remote''s, not the one gh would pick among several."
  (let ((remote (diff-review-forge--remote diff-review-github-remote)))
    (apply #'diff-review-forge--run diff-review-github-program
           (cons "GH_PROMPT_DISABLED=1"
                 (and remote (list (format "GH_REPO=%s/%s" (car remote) (cdr remote))
                                   (concat "GH_HOST=" (car remote)))))
           input args)))

(defun diff-review-github--graphql (query variables)
  "Run QUERY with VARIABLES, an alist, on GitHub; the response."
  (diff-review-github--gh (json-serialize `((query . ,query) (variables . ,variables)))
                          "api" "graphql" "--input" "-"))

(defun diff-review-github--post (endpoint body)
  "Post BODY, an alist, to the REST ENDPOINT; the response."
  (diff-review-github--gh (json-serialize body)
                          "api" "--method" "POST" endpoint "--input" "-"))

;;;; Fetch

(defconst diff-review-github--threads-query
  "query($id: ID!, $after: String) {
  node(id: $id) { ... on PullRequest {
    reviewThreads(first: 50, after: $after) {
      pageInfo { hasNextPage endCursor }
      nodes { id isResolved path diffSide originalLine originalStartLine
        comments(first: 100) { nodes {
          id body createdAt author { login } originalCommit { oid } } } } } } } }"
  "The review threads of a pull request, by its node id, a page at a time.")

(defun diff-review-github--threads (id)
  "The review threads of the pull request with node ID."
  (let (threads after more)
    (while (progn
             (let ((page (diff-review-forge--get
                          (diff-review-github--graphql
                           diff-review-github--threads-query
                           `((id . ,id) (after . ,(or after :null))))
                          "data" "node" "reviewThreads")))
               (setq threads (append threads (gethash "nodes" page) nil)
                     after (diff-review-forge--get page "pageInfo" "endCursor")
                     more (diff-review-forge--get page "pageInfo" "hasNextPage")))
             more))
    threads))

(defun diff-review-github--message (comment)
  "COMMENT from GitHub as a message of the review."
  (diff-review-forge-message (or (diff-review-forge--get comment "author" "login") "ghost")
                             (or (gethash "createdAt" comment) (gethash "submittedAt" comment))
                             (or (gethash "body" comment) "")
                             (gethash "id" comment)))

(defun diff-review-github--review-thread (thread base)
  "THREAD, a review thread from GitHub, as a thread of the review.
Its lines are counted where it was written: in the blob of the commit it
was on or, on a removed line, of that commit's merge base with BASE.  So
the review follows them to the tip, and says when they are outdated.
`default-directory' is the root."
  (let* ((comments (append (diff-review-forge--get thread "comments" "nodes") nil))
         (c (diff-review-github--message (car comments)))
         (path (gethash "path" thread))
         (last (gethash "originalLine" thread))
         (commit (diff-review-forge--get (car comments) "originalCommit" "oid"))
         (resolved (and (gethash "isResolved" thread) t)))
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
    (when resolved (puthash "resolved" t c))
    (diff-review-forge-mark c "thread" (gethash "id" thread) "resolved" resolved)
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

(cl-defmethod diff-review-forge-fetch ((_ (eql 'github)) number)
  "Fetch pull request NUMBER into refs/pull/NUMBER, and its base; its threads.
They are its review threads, then its comments and reviews' bodies."
  (let* ((pr (diff-review-github--gh nil "pr" "view" (number-to-string number)
                                     "--json" "id,baseRefName,reviews,comments"))
         (remote diff-review-github-remote)
         (base (concat remote "/" (gethash "baseRefName" pr)))
         (threads (diff-review-github--threads (gethash "id" pr))))
    (diff-review--git-at default-directory "fetch" "--quiet" "--no-tags" remote
                         (format "+refs/pull/%d/head:refs/pull/%d" number number)
                         (format "+refs/heads/%s:refs/remotes/%s"
                                 (gethash "baseRefName" pr) base))
    (diff-review-forge--fetch-commits
     remote (mapcar (lambda (thread)
                      (diff-review-forge--get
                       (seq-first (diff-review-forge--get thread "comments" "nodes"))
                       "originalCommit" "oid"))
                    threads))
    (list :head (format "pull/%d" number)
          :base base
          :threads (append (mapcar (lambda (thread)
                                     (diff-review-github--review-thread thread base))
                                   threads)
                           (diff-review-github--conversation pr)))))

;;;; Send

(defconst diff-review-github--reply-mutation
  "mutation($thread: ID!, $body: String!) {
  addPullRequestReviewThreadReply(input: {pullRequestReviewThreadId: $thread, body: $body}) {
    comment { id } } }"
  "A reply to a review thread, by the thread's node id.")

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
\(body) for no path, or one the pull request leaves alone.  The review
buffer must be current."
  (let ((file (diff-review-forge--file comment))
        (lines (diff-review-forge--tip-lines comment)))
    (cond ((null file) '(body))
          ((and lines
                (seq-some (lambda (h) (and (<= (car h) (car lines)) (<= (cdr lines) (cdr h))))
                          (diff-review-github--hunks file)))
           (cons 'line lines))
          (t (cons 'file (or lines (diff-review--comment-lines comment)))))))

(cl-defmethod diff-review-forge-send ((_ (eql 'github)) number new replies resolves verdict)
  "Send to pull request NUMBER: NEW threads, REPLIES, RESOLVES and VERDICT.
New threads go as one review, VERDICT its event, the threads on no
path, or on one it leaves alone, its body.  A thread outside the pull
request's hunks goes on its file, its lines in its text."
  (let* ((prefix (format "repos/{owner}/{repo}/pulls/%d" number))
         (placed (mapcar (lambda (c) (cons c (diff-review-github--placement c))) new))
         (lines (seq-filter (lambda (p) (eq (cadr p) 'line)) placed))
         (files (seq-filter (lambda (p) (eq (cadr p) 'file)) placed))
         (body (seq-filter (lambda (p) (eq (cadr p) 'body)) placed)))
    (pcase-dolist (`(,thread . ,reply) replies)
      (let ((id (diff-review-forge--get thread "forge" "thread")))
        (cond
         ;; By the thread's node: REST's comment ids outgrew GraphQL's
         ;; databaseId, which reads null for new ones.
         (id (diff-review-forge-mark
              reply "node"
              (diff-review-forge--get
               (diff-review-github--graphql diff-review-github--reply-mutation
                                            `((thread . ,id) (body . ,(gethash "body" reply))))
               "data" "addPullRequestReviewThreadReply" "comment" "id")))
         ;; A comment, or a review's body, takes no replies: quote it, as GitHub does.
         ((not (gethash "path" thread))
          (diff-review-forge-mark
           reply "node"
           (gethash "node_id"
                    (diff-review-github--post
                     (format "repos/{owner}/{repo}/issues/%d/comments" number)
                     `((body . ,(concat (replace-regexp-in-string
                                         "^" "> " (string-trim (gethash "body" thread)))
                                        "\n\n" (gethash "body" reply))))))))
         ;; Else a thread sent on its file and not pulled since: the reply waits.
         )))
    (dolist (thread resolves)
      (let ((resolved (and (gethash "resolved" thread) t)))
        (diff-review-github--graphql
         (format "mutation($id: ID!) { %s(input: {threadId: $id}) { thread { id } } }"
                 (if resolved "resolveReviewThread" "unresolveReviewThread"))
         `((id . ,(diff-review-forge--get thread "forge" "thread"))))
        (diff-review-forge-mark thread "resolved" resolved)))
    (pcase-dolist (`(,thread file . ,at) files)
      (diff-review-forge-mark
       thread "node"
       (gethash "node_id"
                (diff-review-github--post
                 (concat prefix "/comments")
                 `((body . ,(diff-review-forge--text thread (diff-review-forge--where nil at)))
                   (commit_id . ,diff-review--tip)
                   (path . ,(gethash "path" thread))
                   (subject_type . "file"))))))
    (when (or lines body verdict)
      (let ((review
             (diff-review-github--post
              (concat prefix "/reviews")
              `((commit_id . ,diff-review--tip)
                (event . ,(pcase verdict
                            ("approved" "APPROVE")
                            ("changes" "REQUEST_CHANGES")
                            (_ "COMMENT")))
                ,@(and body
                       `((body . ,(mapconcat
                                   (pcase-lambda (`(,c . ,_))
                                     (diff-review-forge--text
                                      c (diff-review-forge--where
                                         (gethash "path" c) (diff-review--comment-lines c))))
                                   body "\n\n"))))
                (comments
                 . ,(vconcat
                     (mapcar (pcase-lambda (`(,c line ,first . ,last))
                               `((path . ,(gethash "path" c))
                                 (body . ,(diff-review-forge--text c))
                                 (line . ,last) (side . "RIGHT")
                                 ,@(and (/= first last)
                                        `((start_line . ,first) (start_side . "RIGHT")))))
                             lines)))))))
        (dolist (p (append lines body))
          (diff-review-forge-mark (car p) "review" (gethash "node_id" review)))))))

;;;; The entry point

(defun diff-review-github--read-pr ()
  "Read a pull request of `diff-review-github-remote''s repository: its number."
  (let ((default-directory (diff-review--root-here)))
    (diff-review-forge-read
     "Review pull request: "
     (lambda ()
       (mapcar (lambda (pr)
                 (cons (format "#%d %s  (%s, %s)" (gethash "number" pr)
                               (gethash "title" pr) (gethash "headRefName" pr)
                               (diff-review-forge--get pr "author" "login"))
                       (gethash "number" pr)))
               (diff-review-github--gh nil "pr" "list" "--json"
                                       "number,title,headRefName,author" "--limit" "100"))))))

;;;###autoload
(defun diff-review-github-pr (number)
  "Review pull request NUMBER of `diff-review-github-remote''s GitHub repository.
Interactively, pick among the open ones or type a number; with a prefix
argument, read the number alone.  The review
is review/github/NUMBER.json under the common git directory, following
refs/pull/NUMBER; `diff-review-forge-pull' and `diff-review-forge-submit'
keep it and GitHub in step."
  (interactive (list (diff-review-github--read-pr)))
  (diff-review-forge-open 'github number))

(provide 'diff-review-github)
;;; diff-review-github.el ends here
