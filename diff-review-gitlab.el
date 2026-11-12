;;; diff-review-gitlab.el --- Review GitLab merge requests -*- lexical-binding: t; -*-

;; Author: Xiaoxing Hu <hi@xiaoxing.dev>
;; URL: https://github.com/xiaoxinghu/diff-review

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The GitLab backend of diff-review-forge.el.  `diff-review-gitlab-mr'
;; reviews a merge request: its head is fetched into
;; refs/merge-requests/N, and its discussions come in with their lines
;; counted where they were written, on the new side in the blob of the
;; head they were on, on the old side in the base's, so the review
;; follows them to the tip as it does its own.  A discussion on no line
;; is a thread on the review as a whole; every discussion takes replies,
;; and a resolvable one resolves.
;;
;; Sent, a new thread is a discussion on its last line at the tip, the
;; range in its text; on its file when the tip changed its lines; and a
;; plain discussion when the merge request leaves its file alone.
;; Approving approves the merge request, and asking for changes revokes
;; an approval sent before: GitLab's API has no request for changes.
;;
;; The glab command line does the talking, so its login is all the setup.

;;; Code:

(require 'diff-review-forge)
(require 'url-util)

(defcustom diff-review-gitlab-program "glab"
  "The glab program: a name the variable `exec-path' finds, or a file."
  :type 'string
  :group 'diff-review)

(defcustom diff-review-gitlab-remote "origin"
  "The remote whose GitLab project has the merge requests, and serves them.
A fork's is usually \"upstream\"."
  :type 'string
  :group 'diff-review)

(defun diff-review-gitlab--api (method path &optional body pages)
  "Call METHOD on PATH, under the project, in GitLab's API; the response.
BODY is an alist sent as JSON; PAGES, every page of a list.  The project
is `diff-review-gitlab-remote''s, not the one glab would pick among
several.  `default-directory' is in the repository."
  (let ((remote (diff-review-forge--remote diff-review-gitlab-remote)))
    (apply #'diff-review-forge--run diff-review-gitlab-program nil
           (and body (json-serialize body))
           "api" "--method" method
           (concat "projects/" (if remote (url-hexify-string (cdr remote)) ":id") "/" path)
           (append (and remote (list "--hostname" (car remote)))
                   (and body '("--input" "-"))
                   (and pages '("--paginate"))))))

;;;; Fetch

(defun diff-review-gitlab--message (note)
  "NOTE from GitLab as a message of the review."
  (diff-review-forge-message (or (diff-review-forge--get note "author" "username") "ghost")
                             (gethash "created_at" note)
                             (or (gethash "body" note) "")
                             (gethash "id" note)))

(defun diff-review-gitlab--discussion (discussion)
  "DISCUSSION from GitLab as a thread of the review, or nil for system notes.
Its lines are counted where they were written: on the new side in the
blob of the head it was on, on the old side in the base's.  So the
review follows them to the tip, and says when they are outdated.
`default-directory' is the root."
  (when-let* ((notes (seq-remove (lambda (n) (gethash "system" n)) (gethash "notes" discussion))))
    (let* ((first (car notes))
           (c (diff-review-gitlab--message first))
           (position (gethash "position" first))
           (new (diff-review-forge--get position "new_line"))
           (last (or new (diff-review-forge--get position "old_line"))))
      (when position
        (puthash "path" (or (gethash "new_path" position) (gethash "old_path" position)) c))
      (when last
        (puthash "lines" (vector (or (diff-review-forge--get position "line_range" "start"
                                                             (if new "new_line" "old_line"))
                                     last)
                                 last)
                 c)
        (when-let* ((blob (diff-review--git-maybe
                           "rev-parse" "--verify" "--quiet"
                           (if new
                               (concat (gethash "head_sha" position) ":" (gethash "new_path" position))
                             (concat (gethash "base_sha" position) ":"
                                     (gethash "old_path" position))))))
          (puthash "blob" blob c)))
      (puthash "replies" (vconcat (mapcar #'diff-review-gitlab--message (cdr notes))) c)
      (diff-review-forge-mark c "thread" (gethash "id" discussion))
      (when (gethash "resolvable" first)
        (let ((resolved (and (gethash "resolved" first) t)))
          (when resolved (puthash "resolved" t c))
          (diff-review-forge-mark c "resolved" resolved)))
      c)))

(cl-defmethod diff-review-forge-fetch ((_ (eql 'gitlab)) number)
  "Fetch merge request NUMBER into refs/merge-requests/NUMBER, and its target.
Its threads are its discussions."
  (let* ((mr (diff-review-gitlab--api "GET" (format "merge_requests/%d" number)))
         (discussions (append (diff-review-gitlab--api
                               "GET" (format "merge_requests/%d/discussions?per_page=100" number)
                               nil t)
                              nil))
         (remote diff-review-gitlab-remote)
         (base (concat remote "/" (gethash "target_branch" mr))))
    (diff-review--git-at default-directory "fetch" "--quiet" "--no-tags" remote
                         (format "+refs/merge-requests/%d/head:refs/merge-requests/%d"
                                 number number)
                         (format "+refs/heads/%s:refs/remotes/%s" (gethash "target_branch" mr) base))
    (diff-review-forge--fetch-commits
     remote (mapcan (lambda (d)
                      (let ((note (seq-first (gethash "notes" d))))
                        (list (diff-review-forge--get note "position" "head_sha")
                              (diff-review-forge--get note "position" "base_sha"))))
                    discussions))
    (list :head (format "merge-requests/%d" number)
          :base base
          :threads (delq nil (mapcar #'diff-review-gitlab--discussion discussions)))))

;;;; Send

(defun diff-review-gitlab--version (mr)
  "The diff version of MR, a path, whose head is the tip the review shows.
A position on the diff names it.  The review buffer must be current."
  (or (seq-find (lambda (v) (equal (gethash "head_commit_sha" v) diff-review--tip))
                (diff-review-gitlab--api "GET" (concat mr "/versions") nil t))
      (user-error "GitLab has no version of the merge request at %s: pull first"
                  (substring diff-review--tip 0 8))))

(defun diff-review-gitlab--old-line (file line version)
  "LINE of FILE at the tip as VERSION's base has it, or nil for a line it adds.
GitLab places a comment on a line both sides have by both numbers.  The
review buffer must be current."
  (let ((tip (diff-review--git-maybe "rev-parse" "--verify" "--quiet"
                                     (concat diff-review--tip ":" (plist-get file :path))))
        (base (diff-review--git-maybe "rev-parse" "--verify" "--quiet"
                                      (concat (gethash "base_commit_sha" version) ":"
                                              (or (plist-get file :old) (plist-get file :path))))))
    (and tip base
         (let ((hunks (diff-review--hunks-between tip base)))
           (car (and (listp hunks) (diff-review--map-lines hunks (cons line line))))))))

(defun diff-review-gitlab--position (comment version)
  "Where COMMENT goes on VERSION's diff, a GitLab position, or nil for none.
None for no path, or one the merge request leaves alone; its last line
at the tip; or its file, when the tip changed its lines.  The review
buffer must be current."
  (when-let* ((file (diff-review-forge--file comment)))
    (let ((lines (diff-review-forge--tip-lines comment))
          (refs `((base_sha . ,(gethash "base_commit_sha" version))
                  (start_sha . ,(gethash "start_commit_sha" version))
                  (head_sha . ,(gethash "head_commit_sha" version))
                  (old_path . ,(or (plist-get file :old) (plist-get file :path)))
                  (new_path . ,(plist-get file :path)))))
      (if (null lines)
          `((position_type . "file") ,@refs)
        (let ((old (diff-review-gitlab--old-line file (cdr lines) version)))
          `((position_type . "text") (new_line . ,(cdr lines))
            ,@(and old `((old_line . ,old)))
            ,@refs))))))

(cl-defmethod diff-review-forge-send ((_ (eql 'gitlab)) number new replies resolves verdict)
  "Send to merge request NUMBER: NEW threads, REPLIES, RESOLVES and VERDICT.
A new thread is a discussion: on its last line at the tip, its range in
its text; on its file when the tip changed its lines; plain, its path
and lines in its text, on a file the merge request leaves alone.
Approving approves, and asking for changes revokes an approval sent before."
  (let* ((mr (format "merge_requests/%d" number))
         (version (and (seq-some #'diff-review-forge--file new) (diff-review-gitlab--version mr))))
    (pcase-dolist (`(,thread . ,reply) replies)
      (diff-review-forge-mark
       reply "node"
       (gethash "id" (diff-review-gitlab--api
                      "POST" (format "%s/discussions/%s/notes" mr
                                     (diff-review-forge--get thread "forge" "thread"))
                      `((body . ,(gethash "body" reply)))))))
    (dolist (thread resolves)
      (let ((resolved (and (gethash "resolved" thread) t)))
        (diff-review-gitlab--api "PUT" (format "%s/discussions/%s" mr
                                               (diff-review-forge--get thread "forge" "thread"))
                                 `((resolved . ,(or resolved :false))))
        (diff-review-forge-mark thread "resolved" resolved)))
    (dolist (thread new)
      (let* ((position (and version (diff-review-gitlab--position thread version)))
             (lines (diff-review-forge--tip-lines thread))
             (where (cond ((null position)
                           (diff-review-forge--where (gethash "path" thread)
                                                     (diff-review--comment-lines thread)))
                          ((equal (alist-get 'position_type position) "file")
                           (diff-review-forge--where nil (diff-review--comment-lines thread)))
                          ((/= (car lines) (cdr lines)) (diff-review-forge--where nil lines))))
             (discussion (diff-review-gitlab--api
                          "POST" (concat mr "/discussions")
                          `((body . ,(diff-review-forge--text thread where))
                            ,@(and position `((position . ,position)))))))
        (diff-review-forge-mark thread
                                "node" (gethash "id" (seq-first (gethash "notes" discussion)))
                                "thread" (gethash "id" discussion))))
    (pcase verdict
      ("approved" (diff-review-gitlab--api "POST" (concat mr "/approve")))
      ("changes"
       (when (equal (diff-review-forge--get diff-review--data "verdict" "forge" "state")
                    "approved")
         (diff-review-gitlab--api "POST" (concat mr "/unapprove")))))))

;;;; The entry point

(defun diff-review-gitlab--read-mr ()
  "Read a merge request of `diff-review-gitlab-remote''s project: its number."
  (let ((default-directory (diff-review--root-here)))
    (diff-review-forge-read
     "Review merge request: "
     (lambda ()
       (mapcar (lambda (mr)
                 (cons (format "!%d %s  (%s, %s)" (gethash "iid" mr)
                               (gethash "title" mr) (gethash "source_branch" mr)
                               (diff-review-forge--get mr "author" "username"))
                       (gethash "iid" mr)))
               (diff-review-gitlab--api "GET" "merge_requests?state=opened&per_page=100"))))))

;;;###autoload
(defun diff-review-gitlab-mr (number)
  "Review merge request NUMBER of `diff-review-gitlab-remote''s GitLab project.
Interactively, pick among the open ones or type a number; with a prefix
argument, read the number alone.  The review is review/gitlab/NUMBER.json
under the common git directory, following refs/merge-requests/NUMBER;
`diff-review-forge-pull' and `diff-review-forge-submit' keep it and
GitLab in step."
  (interactive (list (diff-review-gitlab--read-mr)))
  (diff-review-forge-open 'gitlab number))

(provide 'diff-review-gitlab)
;;; diff-review-gitlab.el ends here
