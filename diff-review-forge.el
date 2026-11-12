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
the forge's id for it, and for one that takes replies and resolves,
\"thread\" and \"resolved\", its state on the forge.  `default-directory'
is the root.")

(cl-defgeneric diff-review-forge-send (backend number new replies resolves verdict)
  "Send to BACKEND's pull request NUMBER what was written in the review.
NEW are threads never sent, REPLIES (THREAD . REPLY) pairs on threads
from the forge, RESOLVES threads to resolve or reopen as the review has
them, and VERDICT \"approved\", \"changes\" or nil.  Each item is marked
sent as it goes, with `diff-review-forge-mark', so a failure part way
sends nothing twice; the verdict is sent once this returns.  The review
buffer is current.")

;;;; For backends

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
    (diff-review--write)
    (revert-buffer)))

(defun diff-review-forge--verdict ()
  "The verdict's state when the forge does not have it yet, else nil.
A stale verdict has none.  The review buffer must be current."
  (let* ((verdict (gethash "verdict" diff-review--data))
         (sent (diff-review-forge--get verdict "forge")))
    (and (hash-table-p verdict)
         (not (eq (diff-review--turn) 'reviewer))
         (not (and sent
                   (equal (gethash "state" sent) (gethash "state" verdict))
                   (equal (gethash "head" sent) (gethash "head" verdict))))
         (gethash "state" verdict))))

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
                                   (and (diff-review-forge--get c "forge" "thread")
                                        (not (eq (and (gethash "resolved" c) t)
                                                 (diff-review-forge--get c "forge" "resolved")))))
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
