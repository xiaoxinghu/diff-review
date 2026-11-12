;;; diff-review-forge.el --- Review pull requests on a forge -*- lexical-binding: t; -*-

;; Author: Xiaoxing Hu <hi@xiaoxing.dev>
;; URL: https://github.com/xiaoxinghu/diff-review

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A pull request is a branch review whose threads are also on a forge.
;; This is what every forge shares: the review follows the ref the pull
;; request's head is fetched into, the forge's threads come in on the
;; lines they were written on and merge with those written here, and
;; what was written here goes back.  A backend, a file per forge, does
;; the talking through two methods:
;;
;;   `diff-review-forge-fetch'  fetch the head and the base; the threads
;;   `diff-review-forge-send'   send threads, replies, resolves, a verdict
;;
;; and an entry point that calls `diff-review-forge-open'.  The review
;; file names its backend, "forge": {"backend": "github", "number": 5},
;; and the backend is the feature diff-review-BACKEND, loaded when the
;; review needs it.  What came from the forge or went to it carries a
;; "forge" key; a thread without one has not been sent.
;;
;; `diff-review-forge-pull' brings the review's threads up to the
;; forge's, and `diff-review-forge-submit' sends what was written here.

;;; Code:

(require 'diff-review)

;;;; The backend's methods

(cl-defgeneric diff-review-forge-fetch (backend number)
  "Fetch BACKEND's pull request NUMBER into git, and return its threads.
A plist: :head, the ref the head was fetched into; :base, the base as a
revision; :threads, the forge's threads in the review's own format.  Each
thread has a \"forge\" table, made by `diff-review-forge-message': \"node\",
the forge's id for it; \"thread\" when its replies are on the forge; and
\"resolved\", its state there, when it resolves there.  :verdicts, the
reviewers' last words, (:by NAME :state STATE :head COMMIT) each, STATE
\"approved\" or \"changes\" and COMMIT nil when it holds for any; and
:me, the forge's name for whoever runs this.  `default-directory' is the
root.")

(cl-defgeneric diff-review-forge-send (backend number new replies resolves verdict)
  "Send to BACKEND's pull request NUMBER what was written in the review.
NEW are threads never sent, REPLIES (THREAD . REPLY) pairs on threads
from the forge, RESOLVES threads to resolve or reopen as the review has
them, and VERDICT \"approved\", \"changes\" or nil.  Each item is marked
sent as it goes, with `diff-review-forge-mark', so a failure part way
sends nothing twice; the verdict is sent once this returns.  The review
buffer is current.")

;;;; For backends

(defun diff-review-forge-read (prompt candidates)
  "Read a pull request's number with PROMPT, among CANDIDATES or typed.
CANDIDATES is a function returning (LABEL . NUMBER) pairs, called only
without a prefix argument: with one, the number alone is read, for a
pull request the list leaves out, closed or a page past it."
  (if current-prefix-arg
      (read-number prompt)
    (let* ((candidates (funcall candidates))
           (choice (completing-read prompt candidates)))
      (or (cdr (assoc choice candidates))
          (and (string-match "\\`\\s-*[#!]?\\([0-9]+\\)\\s-*\\'" choice)
               (string-to-number (match-string 1 choice)))
          (user-error "No pull request %S" choice)))))

(defun diff-review-forge--remote (remote)
  "The host and path of REMOTE's URL, (HOST . PATH), or nil for a URL with no host.
`default-directory' is in the repository."
  (diff-review-forge--url (diff-review--git-at default-directory "remote" "get-url" remote)))

(defun diff-review-forge--url (url)
  "The host and path URL names, (HOST . PATH), or nil for a URL with no host."
  (let ((case-fold-search t))
    (and (string-match (concat "\\`\\(?:[a-z+]+://\\)?\\(?:[^@/]+@\\)?\\([^:/]+\\)"
                               "\\(?::[0-9]+\\)?[:/]\\([^/].*?\\)\\(?:\\.git\\)?/?\\'")
                       url)
         (cons (match-string 1 url) (match-string 2 url)))))

(defun diff-review-forge--run (program env input &rest args)
  "Run PROGRAM with ARGS, ENV added to the environment, INPUT on its stdin.
Its output as JSON, parsed, or nil for none.  A failure is an error
with what it said."
  (let ((process-environment (append env process-environment))
        (errors (make-temp-file "diff-review-forge")))
    (unwind-protect
        (with-temp-buffer
          (when input (insert input))
          (unless (eql 0 (apply #'call-process-region (point-min) (point-max)
                                program t (list t errors) nil args))
            (user-error "%s %s: %s" program (string-join args " ")
                        (with-temp-buffer
                          (insert-file-contents errors)
                          (string-trim (buffer-string)))))
          (goto-char (point-min))
          (skip-chars-forward " \t\n")
          (and (not (eobp)) (json-parse-buffer :null-object nil :false-object nil)))
      (delete-file errors))))

(defun diff-review-forge--fetch-commits (remote commits)
  "Fetch those of COMMITS git has not from REMOTE, as far as it serves them.
A force push leaves the commits threads were written on out of the
pull request's head; without them the threads draw where they were."
  (when-let* ((missing (seq-remove (lambda (oid)
                                     (diff-review--git-maybe "cat-file" "-e"
                                                             (concat oid "^{commit}")))
                                   (seq-uniq (delq nil commits)))))
    (apply #'diff-review--git-maybe "fetch" "--quiet" "--no-tags" remote missing)))

(defun diff-review-forge-mark (item &rest pairs)
  "Put PAIRS, keys and values, in ITEM's \"forge\" table, made when missing."
  (let ((forge (or (gethash "forge" item)
                   (puthash "forge" (make-hash-table :test #'equal) item))))
    (while pairs
      (puthash (pop pairs) (pop pairs) forge))))

(defun diff-review-forge-message (author at body node)
  "A message of the review by AUTHOR at AT saying BODY, NODE its forge id."
  (let ((m (make-hash-table :test #'equal)))
    (puthash "author" author m)
    (puthash "at" at m)
    (puthash "body" body m)
    (diff-review-forge-mark m "node" node)
    m))

(defun diff-review-forge--get (object &rest keys)
  "The value under KEYS in OBJECT, nested hash tables; nil where one is missing."
  (dolist (key keys object)
    (setq object (and (hash-table-p object) (gethash key object)))))

(defun diff-review-forge--node (item)
  "The forge's id for ITEM, a thread or a reply, or nil."
  (diff-review-forge--get item "forge" "node"))

;;;; Sending

(defun diff-review-forge--file (comment)
  "The file COMMENT is on, a plist, when the pull request changes it; else nil.
The review buffer must be current."
  (when-let* ((path (gethash "path" comment))
              (index (diff-review--file-index path))
              (file (diff-review--file index)))
    (and (not (equal (plist-get file :status) " ")) file)))

(defun diff-review-forge--tip-lines (comment)
  "COMMENT's lines as the tip has them, (FIRST . LAST).
Nil when it has none, or the tip changed them.  The review buffer must be
current."
  (let* ((written (diff-review--comment-lines comment))
         (blob (gethash "blob" comment))
         (tip-blob (and written (diff-review--git-maybe
                                 "rev-parse" "--verify" "--quiet"
                                 (concat diff-review--tip ":" (gethash "path" comment))))))
    (and tip-blob
         (if (and blob (not (equal blob tip-blob)))
             (let ((hunks (diff-review--hunks-between blob tip-blob)))
               (and (listp hunks) (diff-review--map-lines hunks written)))
           written))))

(defun diff-review-forge--where (path lines)
  "PATH and LINES, (FIRST . LAST) or nil, as a body begins with them."
  (concat (and path (format "`%s` " path))
          (cond ((null lines) "")
                ((= (car lines) (cdr lines)) (format "line %d" (car lines)))
                (t (format "lines %d-%d" (car lines) (cdr lines))))
          (and (or path lines) ": ")))

(defun diff-review-forge--text (comment &optional where)
  "The text of COMMENT for a forge, its replies after it, WHERE before it."
  (string-join
   (cons (concat where (gethash "body" comment))
         (mapcar (lambda (r) (format "**%s**: %s" (gethash "author" r) (gethash "body" r)))
                 (gethash "replies" comment)))
   "\n\n"))

;;;; The review

(defun diff-review-forge--backend ()
  "The review's backend, its file loaded.  The review buffer must be current."
  (let ((name (or (diff-review-forge--get diff-review--data "forge" "backend")
                  (user-error "Not the review of a pull request"))))
    (unless (require (intern (concat "diff-review-" name)) nil t)
      (user-error "No backend %s: diff-review-%s is not on the load path" name name))
    (intern name)))

(defun diff-review-forge--merge (local remote)
  "Bring LOCAL, a thread of the review, up to REMOTE, the forge's copy; LOCAL.
What is on the forge was written there, so its text wins, as does its
resolved state once it changed since the last pull.  Replies not sent stay."
  (let ((was (diff-review-forge--get local "forge" "resolved"))
        (now (diff-review-forge--get remote "forge" "resolved"))
        (thread (diff-review-forge--get remote "forge" "thread")))
    (dolist (key '("author" "at" "body" "path" "lines" "blob" "forge"))
      (if-let* ((value (gethash key remote)))
          (puthash key value local)
        (remhash key local)))
    ;; One that is no thread on the forge has no replies there: the ones
    ;; sent are its own items, which the pull leaves out.
    (when thread
      (unless (eq was now)
        (if now (puthash "resolved" t local) (remhash "resolved" local)))
      (let ((mine (append (gethash "replies" local) nil)))
        (puthash "replies"
                 (vconcat
                  (mapcar (lambda (reply)
                            (if-let* ((same (seq-find (lambda (r)
                                                        (equal (diff-review-forge--node r)
                                                               (diff-review-forge--node reply)))
                                                      mine)))
                                (progn (dolist (key '("author" "at" "body" "forge"))
                                         (puthash key (gethash key reply) same))
                                       same)
                              reply))
                          (gethash "replies" remote))
                  (seq-remove #'diff-review-forge--node mine))
                 local)))
    local))

(defun diff-review-forge--update (fetched)
  "Bring the review up to FETCHED, from `diff-review-forge-fetch', and rescan.
The forge's threads join the review or update their copies here.  One
from the forge that it no longer has goes, as does one sent that comes
back as the forge's own; one never sent stays.  The review buffer must
be current."
  (let ((theirs (make-hash-table :test #'equal))
        (threads (plist-get fetched :threads))
        kept)
    (dolist (r threads)
      (puthash (diff-review-forge--node r) r theirs))
    (unless (verify-visited-file-modtime) ; someone else wrote it: take theirs first
      (diff-review--revert))
    (let ((replied (mapcan (lambda (c)
                             (delq nil (mapcar #'diff-review-forge--node (gethash "replies" c))))
                           (append diff-review--comments nil))))
      (seq-doseq (c diff-review--comments)
        (let ((same (gethash (diff-review-forge--node c) theirs)))
          (cond ((not (gethash "forge" c)) (push c kept))
                (same (push (diff-review-forge--merge c same) kept)
                      (remhash (diff-review-forge--node c) theirs)))))
      (dolist (r threads)
        (let ((node (diff-review-forge--node r)))
          (when (and (gethash node theirs) (not (member node replied)))
            (puthash "id" (diff-review--comment-id) r)
            (push r kept)))))
    (setq diff-review--comments (vconcat (nreverse kept)))
    (puthash "comments" diff-review--comments diff-review--data)
    (puthash "base" (plist-get fetched :base) diff-review--data)
    (let* ((me (plist-get fetched :me))
           (verdicts (plist-get fetched :verdicts))
           (mine (seq-find (lambda (v) (equal (plist-get v :by) me)) verdicts)))
      (diff-review-forge-mark
       diff-review--data "verdicts"
       (vconcat (mapcar (lambda (v)
                          (let ((table (make-hash-table :test #'equal)))
                            (puthash "by" (plist-get v :by) table)
                            (puthash "state" (plist-get v :state) table)
                            (when (plist-get v :head) (puthash "head" (plist-get v :head) table))
                            table))
                        (remq mine verdicts))))
      (when mine
        (diff-review-forge--adopt
         mine (diff-review--git "rev-parse" (concat (plist-get fetched :head) "^{commit}")))))
    (diff-review--write)
    (revert-buffer)))

(defun diff-review-forge--unsent-p (verdict)
  "Whether VERDICT changed since it went to the forge, or came from it."
  (let ((sent (diff-review-forge--get verdict "forge")))
    (not (and sent
              (equal (gethash "state" sent) (gethash "state" verdict))
              (equal (gethash "head" sent) (gethash "head" verdict))))))

(defun diff-review-forge--verdict ()
  "The verdict's state when the forge does not have it yet, else nil.
A stale verdict has none.  The review buffer must be current."
  (let ((verdict (gethash "verdict" diff-review--data)))
    (and (hash-table-p verdict)
         (not (eq (diff-review--turn) 'reviewer))
         (diff-review-forge--unsent-p verdict)
         (gethash "state" verdict))))

(defun diff-review-forge--adopt (verdict tip)
  "Make VERDICT, this reviewer's last word on the forge, the review's.
A verdict given here on TIP and not sent yet stays: it is the newer.
The review buffer must be current."
  (let ((local (gethash "verdict" diff-review--data))
        (state (plist-get verdict :state))
        (head (or (plist-get verdict :head) tip)))
    (unless (and (hash-table-p local)
                 (equal (gethash "head" local) tip)
                 (diff-review-forge--unsent-p local))
      (let ((table (if (hash-table-p local) local (make-hash-table :test #'equal))))
        (unless (and (equal (gethash "state" table) state) (equal (gethash "head" table) head))
          (puthash "state" state table)
          (puthash "head" head table)
          (puthash "by" (plist-get verdict :by) table)
          (puthash "at" (format-time-string "%FT%T%:z") table))
        (diff-review-forge-mark table "state" state "head" head)
        (puthash "verdict" table diff-review--data)))))

(defun diff-review-forge--others ()
  "The other reviewers' verdicts, as the summary shows them, or nil.
One on a commit the head has moved past is shadowed.  For
`diff-review-summary-functions'."
  (when-let* ((verdicts (diff-review-forge--get diff-review--data "forge" "verdicts"))
              ((> (length verdicts) 0)))
    (mapconcat (lambda (v)
                 (let ((head (gethash "head" v))
                       (approved (equal (gethash "state" v) "approved")))
                   (propertize (concat (gethash "by" v)
                                       (if approved " approved" " requested changes"))
                               'face (cond ((and head (not (equal head (or diff-review--moved
                                                                           diff-review--tip))))
                                            'shadow)
                                           (approved 'success)
                                           (t 'warning)))))
               verdicts ", ")))

(add-hook 'diff-review-summary-functions #'diff-review-forge--others)

;;;; Commands

(defun diff-review-forge-open (backend number)
  "Review BACKEND's pull request NUMBER: fetch it and open its review.
The review file is review/BACKEND/NUMBER.json under the common git
directory, so every worktree opens the same review, its head the ref
the pull request was fetched into.  The forge's threads join it; one
already open is pulled again."
  (let* ((root (diff-review--root-here))
         ;; The main worktree as the root: the file outlives the worktree it was made from.
         (default-directory (or (diff-review--main-worktree root) root))
         ;; Before the file opens: its scan needs the head.
         (fetched (diff-review-forge-fetch backend number)))
    (diff-review-find-file (expand-file-name (format "review/%s/%d.json" backend number)
                                             (diff-review--common-dir default-directory))
                           default-directory (plist-get fetched :base) (plist-get fetched :head))
    (diff-review-forge-mark diff-review--data "backend" (symbol-name backend) "number" number)
    (diff-review-forge--update fetched)))

;;;###autoload
(defun diff-review-forge-pull ()
  "Fetch the pull request again and bring the review's threads up to the forge's.
The forge's text wins, and its resolved state once it changed there; a
thread or reply not sent yet stays."
  (interactive)
  (diff-review--in-review
    (let ((default-directory diff-review--root)
          (backend (diff-review-forge--backend))
          (number (diff-review-forge--get diff-review--data "forge" "number")))
      (diff-review-forge--update (diff-review-forge-fetch backend number))
      (message "Pulled #%d" number))))

;;;###autoload
(defun diff-review-forge-submit ()
  "Send what was written here to the pull request, then pull it again.
New threads, replies to the forge's, threads resolved or reopened here,
and the verdict when the forge does not have it yet.  Asks first."
  (interactive)
  (diff-review--in-review
    (unless (verify-visited-file-modtime) ; someone else wrote it: take theirs first
      (diff-review--revert))
    (let* ((default-directory diff-review--root)
           (backend (diff-review-forge--backend))
           (number (diff-review-forge--get diff-review--data "forge" "number"))
           (comments (append diff-review--comments nil))
           (new (seq-remove (lambda (c) (gethash "forge" c)) comments))
           (sent (seq-filter #'diff-review-forge--node comments))
           (replies (mapcan (lambda (c)
                              (mapcar (lambda (r) (cons c r))
                                      (seq-remove (lambda (r) (gethash "forge" r))
                                                  (gethash "replies" c))))
                            sent))
           (resolves (seq-filter (lambda (c)
                                   (let ((forge (gethash "forge" c)))
                                     (and (not (eq (gethash "resolved" forge 'no) 'no))
                                          (not (eq (and (gethash "resolved" c) t)
                                                   (gethash "resolved" forge))))))
                                 sent))
           (verdict (diff-review-forge--verdict)))
      (unless (or new replies resolves verdict)
        (user-error "Nothing to send to #%d" number))
      (unless (y-or-n-p (format "Send to #%d: %d new threads, %d replies, %d resolved or \
reopened%s? " number (length new) (length replies) (length resolves)
                                (pcase verdict
                                  ("approved" ", approve")
                                  ("changes" ", request changes")
                                  (_ ""))))
        (user-error "Not sent"))
      ;; The marks are written whatever fails, so nothing is sent twice.
      (unwind-protect
          (progn
            (diff-review-forge-send backend number new replies resolves verdict)
            (when verdict
              (let ((table (gethash "verdict" diff-review--data)))
                (diff-review-forge-mark table "state" verdict "head" (gethash "head" table)))))
        (diff-review--write))
      (diff-review-forge-pull)
      (message "Sent to #%d" number))))

(provide 'diff-review-forge)
;;; diff-review-forge.el ends here
