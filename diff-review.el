;;; diff-review.el --- Review a git diff from a review file -*- lexical-binding: t; -*-

;; Author: Xiaoxing Hu <hi@xiaoxing.dev>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit-section "4.0"))
;; Keywords: vc, tools
;; URL: https://github.com/xiaoxinghu/diff-review

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A review is a file.  Visiting it shows an overview of what a worktree
;; changed since the merge-base with a base revision, the way a pull
;; request page does: the counts, then the changed files as a foldable
;; tree.  Picking a file shows the real file in its own major mode, so
;; every editing command, `xref' and the language server work.  A review
;; with a head revision compares two commits instead, and shows the files
;; read-only, as they are at the head.  Overlays add what a diff shows:
;; removed lines inline above where they were, a tint on added lines, and
;; a focus mode that folds the unchanged lines.
;;
;; A sidebar shows the same tree beside the window the review shows in,
;; as its atomic partner: the two split, move and close as one window, so
;; the review can sit in any split the user likes and the tree stays next
;; to it.  Files are kept in the tree's order, directories first, so
;; walking them follows the tree.  The sidebar appears with any window
;; showing the overview or one of its files, goes when the window shows
;; anything else, and `diff-review-toggle-sidebar' turns it off for a
;; review altogether.
;;
;; The review file is JSON and starts with a `format' key, which is how
;; `diff-review-mode' is chosen, whatever the file is called or wherever
;; it lives.  It records what git cannot derive: the base, the viewed
;; marks with the blob hash each file had when it was ticked, so a mark
;; clears when the file changes again.  Everything else comes from git
;; when the file is visited.  Nothing here knows about branches, tasks or
;; worktrees beyond what git reports; whoever creates the file decides
;; where it goes, see `diff-review-find-file'.
;;
;;   {"format": "diff-review/1",
;;    "root": "/path/to/worktree/",   optional: the file's repository
;;    "base": "main",                  optional: `diff-review-default-base'
;;    "head": "abc1234",               optional: the worktree
;;    "viewed": {"src/a.ts": "<blob hash>"},
;;    "comments": [...]}               optional: see below
;;
;; Comments are threads on some lines of a file, as on a pull request
;; page, kept in the same file so an agent reads and writes one thing.
;; A thread's first message carries the location; replies carry none.
;; Lines count in the file content "blob" names, so a thread follows its
;; lines when the file changes, through `git diff' of the two blobs, or
;; says it is outdated when they changed.  Both "path" and "lines" absent
;; is a comment on the review as a whole.  Keys nothing here knows are
;; kept as they are, in a comment too.
;;
;;   {"id": "k3f9a1",                 any string unique among the comments
;;    "path": "src/a.ts",             relative to the root, any file under it
;;    "lines": [12, 15],              first and last, inclusive
;;    "blob": "<blob hash>",          optional: the file as it is now
;;    "author": "xiaoxing",
;;    "at": "2026-11-06T12:40:11+13:00",
;;    "body": "Retries forever when the socket is gone.",
;;    "replies": [{"author": "claude", "at": "...", "body": "Capped in 3b1c0e2."}],
;;    "resolved": true}               optional
;;
;; The review buffer is an ordinary buffer: hide it, show it, kill it to
;; end the review.  Files keep a pointer back to it.  The files it shows
;; in its window are not recorded in the window's history, so ending the
;; review returns the window to what it showed before.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'diff-mode)
(require 'magit-section)

(defgroup diff-review nil
  "Review a git diff from a review file."
  :group 'tools
  :prefix "diff-review-")

(defcustom diff-review-default-base #'diff-review-default-branch
  "The base of a new review: a revision, or a function returning one.
The function is called with `default-directory' at the repository's
root.  A review file records the base it got, so changing this does
not move an existing review."
  :type '(choice (string :tag "Revision")
                 (function :tag "Function returning a revision")))

(defcustom diff-review-context 3
  "Unchanged lines kept around each hunk in focus mode."
  :type 'integer)

(defcustom diff-review-inline-removed t
  "Whether to show removed lines inline, above where they were."
  :type 'boolean)

(defcustom diff-review-file-header t
  "The header line of a file under review.
t shows the review's header, nil leaves the buffer's header line alone,
and a function is called in the file's buffer for the string to show.
The header line the buffer had comes back when the review ends."
  :type '(choice (const :tag "The review's header" t)
                 (const :tag "Leave the header line alone" nil)
                 (function :tag "A function returning the header")))

(defcustom diff-review-file-header-keys t
  "Whether the review's file header ends with hints for its keys."
  :type 'boolean)

(defcustom diff-review-sidebar t
  "Whether a review starts with its sidebar shown."
  :type 'boolean)

(defcustom diff-review-sidebar-width 36
  "Width of the sidebar, in columns, when a review starts.
Resizing the sidebar changes its width for the rest of that review."
  :type 'integer)

(defcustom diff-review-sidebar-side 'left
  "Which side of the review's window the sidebar takes."
  :type '(choice (const left) (const right)))

(defcustom diff-review-tint-added t
  "Whether to tint added lines with `diff-review-added'.
Without the tint they keep the fringe bar, which a terminal has not."
  :type 'boolean)

(defcustom diff-review-tint-removed t
  "Whether to tint the removed lines shown inline with `diff-review-removed'.
Without the tint they keep the fringe bar, which a terminal has not."
  :type 'boolean)

(defcustom diff-review-fringe 'left
  "The fringe the hunk bars are drawn in, or nil for no bars."
  :type '(choice (const left) (const right) (const :tag "No bars" nil)))

(defcustom diff-review-hunk-recenter 6
  "The window line a hunk is shown on when moved to, or nil to center it."
  :type '(choice (integer :tag "Line from the top") (const :tag "Center" nil)))

(defcustom diff-review-author nil
  "The name your comments carry, or nil for git's `user.name', else your login."
  :type '(choice (const :tag "Git's user.name" nil) string))

(defface diff-review-added '((t :extend t))
  "Face for added lines, over the syntax faces.
Without a background of its own, the background of `diff-added' is used.")
(defface diff-review-removed '((t :extend t))
  "Face for removed lines, shown inline, over the syntax faces.
Without a background of its own, the background of `diff-removed' is used.")
(defface diff-review-title '((t :inherit bold :height 1.2))
  "Face for the overview's title, the head and the base.")
(defface diff-review-file-added '((t :inherit (bold success)))
  "Face for the status letter of an added file.")
(defface diff-review-file-deleted '((t :inherit (bold error)))
  "Face for the status letter of a deleted file.")
(defface diff-review-file-changed '((t :inherit (bold warning)))
  "Face for the status letter of a modified, renamed or copied file.")
(defface diff-review-unchanged '((t :inherit shadow))
  "Face for the line that stands for folded unchanged lines.")
(defface diff-review-comment '((t :inherit (font-lock-comment-face highlight) :extend t))
  "Face for a comment thread, drawn under the lines it is on.
A review comment is a comment, so it takes the file's comment colour;
the background is what says it is not the file's own.")
(defface diff-review-comment-range '((t :extend t))
  "Face for the lines of an open comment thread, over the syntax faces.
Without a background of its own, it takes `diff-review-comment's.")
(defface diff-review-comment-author '((t :inherit bold))
  "Face for the author of a comment.")

(defconst diff-review-format "diff-review/1"
  "The value of the format key a review file starts with.")

;;;; State

;; In the review buffer.
(defvar-local diff-review--data nil "The review file as parsed: a hash table, written back as is.")
(defvar-local diff-review--root nil "Repository root, with a trailing slash.")
(defvar-local diff-review--base nil "Full SHA the review compares against.")
(defvar-local diff-review--base-name nil "The revision the file names.")
(defvar-local diff-review--head nil "The revision under review, or nil for the worktree.")
(defvar-local diff-review--head-name nil "The branch under review, or the head revision.")
(defvar-local diff-review--files nil "Vector of file plists, in tree order.")
(defvar-local diff-review--index nil "Index of the file shown, or nil for none.")
(defvar-local diff-review--viewed nil "Hash table of path to the blob hash it was viewed at.")
(defvar-local diff-review--comments nil "Vector of the comments, hash tables as parsed.")
(defvar-local diff-review--sidebar nil "The sidebar buffer, once created.")
(defvar-local diff-review--sidebar-enabled nil "Whether this review shows its sidebar.")
(defvar-local diff-review--sidebar-width nil "The width of this review's sidebar, in columns.")
(defvar-local diff-review--inline-removed nil "Whether this review shows removed lines inline.")
(defvar-local diff-review--threads-open nil "Whether this review draws every thread open.")

;; In a file buffer or the sidebar.
(defvar-local diff-review--review nil "The review buffer this buffer belongs to.")
(defvar-local diff-review--hunks nil
  "In a file buffer, its hunks: plists with :line, :count and :removed.")
(defvar-local diff-review--overlays nil "In a file buffer, its review overlays.")
(defvar-local diff-review--threads nil
  "In a file buffer, its threads as drawn: (COMMENT FIRST LAST STALE OVERLAY).")
(defvar-local diff-review--current nil "In a file buffer, the ids of the threads point is at.")
(defvar-local diff-review--maps nil
  "In a file buffer, the hunks between blobs: ((OLD . NEW) . HUNKS).")
(defvar-local diff-review--focus nil "In a file buffer, whether unchanged lines are folded.")
(defvar-local diff-review--deleted nil "In a buffer showing a deleted file's base version, t.")
(defvar-local diff-review--blob nil "In a buffer showing a revision of a file, what identifies it.")
(defvar-local diff-review--saved nil
  "In a file buffer, the variables the review set: (VAR LOCAL . VALUE) from before.")
(defvar diff-review-file-mode)

(defun diff-review--review-buffer ()
  "The review buffer this buffer belongs to, or nil."
  (cond ((derived-mode-p 'diff-review-mode) (current-buffer))
        ((buffer-live-p diff-review--review) diff-review--review)))

(defmacro diff-review--in-review (&rest body)
  "Run BODY in this buffer's review buffer."
  (declare (indent 0))
  `(with-current-buffer (or (diff-review--review-buffer)
                            (user-error "This buffer's review is closed"))
     ,@body))

;;;; Git

(defun diff-review--git (&rest args)
  "Run git with ARGS at the root and return its output without trailing newlines."
  (let ((default-directory diff-review--root))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (error "Git %s: %s" (string-join args " ") (string-trim (buffer-string))))
      (string-trim-right (buffer-string) "\n+"))))

(defun diff-review--git-maybe (&rest args)
  "Run git with ARGS in `default-directory': its output, or nil when it fails."
  (with-temp-buffer
    (and (zerop (apply #'process-file "git" nil '(t nil) nil args))
         (string-trim (buffer-string)))))

(defun diff-review--toplevel (directory)
  "The root of the repository holding DIRECTORY, or nil."
  (let ((default-directory directory))
    (when-let* ((top (diff-review--git-maybe "rev-parse" "--show-toplevel")))
      (file-name-as-directory top))))

(defun diff-review-default-branch ()
  "The default branch of the repository at `default-directory'.
What origin/HEAD points at, else the first branch of `init.defaultBranch',
main and master that exists, else main."
  (or (when-let* ((ref (diff-review--git-maybe
                        "symbolic-ref" "--short" "refs/remotes/origin/HEAD")))
        (string-remove-prefix "origin/" ref))
      (seq-find (lambda (branch)
                  (diff-review--git-maybe "rev-parse" "--verify" "--quiet"
                                          (concat "refs/heads/" branch)))
                (delq nil (list (diff-review--git-maybe "config" "init.defaultBranch")
                                "main" "master")))
      "main"))

(defun diff-review--default-base (root)
  "The base of a new review of the repository at ROOT."
  (if (functionp diff-review-default-base)
      (let ((default-directory root))
        (funcall diff-review-default-base))
    diff-review-default-base))

(defun diff-review--parse-name-status (output)
  "Parse the NUL-separated OUTPUT of `git diff --name-status -M -z'.
Return a list of (PATH STATUS OLD-PATH)."
  (let ((fields (split-string output "\0" t)) result)
    (while fields
      (let ((status (substring (pop fields) 0 1)))
        (if (member status '("R" "C"))
            (let ((old (pop fields)) (new (pop fields)))
              (push (list new status old) result))
          (push (list (pop fields) status nil) result))))
    (nreverse result)))

(defun diff-review--parse-numstat (output)
  "Parse the NUL-separated OUTPUT of `git diff --numstat -M -z'.
Return a hash table of path to (ADDED . REMOVED)."
  (let ((fields (split-string output "\0" t))
        (table (make-hash-table :test #'equal)))
    (while fields
      (pcase-let ((`(,added ,removed ,path) (split-string (pop fields) "\t")))
        (when (string-empty-p path)   ; a rename: old and new follow
          (pop fields)
          (setq path (pop fields)))
        (puthash path (cons (string-to-number added) (string-to-number removed))
                 table)))
    table))

(defun diff-review--tree-less-p (a b)
  "Non-nil when path A comes before path B in a tree, directories first."
  (let ((as (split-string a "/")) (bs (split-string b "/")))
    (while (and (cdr as) (cdr bs) (equal (car as) (car bs)))
      (pop as)
      (pop bs))
    (cond ((and (cdr as) (not (cdr bs))) t)
          ((and (cdr bs) (not (cdr as))) nil)
          (t (string< (car as) (car bs))))))

(defun diff-review--range ()
  "The revisions to give `git diff': the base, and the head if there is one."
  (delq nil (list diff-review--base diff-review--head)))

(defun diff-review--hashes (paths &optional rev)
  "The blob hashes of the files at PATHS, at REV or the head, in one git call."
  (setq rev (or rev diff-review--head))
  (cond
   ((null paths) nil)
   (rev
    (split-string (apply #'diff-review--git "rev-parse"
                         (mapcar (lambda (p) (concat rev ":" p)) paths))
                  "\n" t))
   (t
    (let ((default-directory diff-review--root))
      (with-temp-buffer
        (insert (string-join paths "\n") "\n")
        (unless (zerop (call-process-region (point-min) (point-max) "git" t t nil
                                            "hash-object" "--stdin-paths"))
          (error "Git hash-object: %s" (string-trim (buffer-string))))
        (split-string (buffer-string) "\n" t))))))

(defun diff-review--scan ()
  "Fill the file vector from git: four calls for any number of files."
  (setq diff-review--head-name (or diff-review--head
                                   (diff-review--git "rev-parse" "--abbrev-ref" "HEAD"))
        diff-review--base
        (condition-case err
            (diff-review--git "merge-base" diff-review--base-name
                              (or diff-review--head "HEAD"))
          (error (user-error "No merge base with base %S: set \"base\" in the review \
file, or `diff-review-default-base' for new reviews (%s)"
                             diff-review--base-name (error-message-string err)))))
  (let* ((entries (sort (diff-review--parse-name-status
                         (apply #'diff-review--git "diff" "--name-status" "-M" "-z"
                                (diff-review--range)))
                        (lambda (a b) (diff-review--tree-less-p (car a) (car b)))))
         (numstat (diff-review--parse-numstat
                   (apply #'diff-review--git "diff" "--numstat" "-M" "-z"
                          (diff-review--range))))
         (deleted (mapcar #'car (seq-filter (lambda (e) (equal (nth 1 e) "D")) entries)))
         (present (mapcar #'car (seq-remove (lambda (e) (equal (nth 1 e) "D")) entries)))
         ;; A deleted file's blob is its base one: every file has the blob it shows.
         (hashes (append (cl-mapcar #'cons present (diff-review--hashes present))
                         (cl-mapcar #'cons deleted
                                    (diff-review--hashes deleted diff-review--base)))))
    (setq diff-review--files
          (vconcat
           (mapcar (lambda (entry)
                     (pcase-let ((`(,path ,status ,old) entry))
                       (let ((stat (gethash path numstat)))
                         (list :path path :status status :old old
                               :added (or (car stat) 0) :removed (or (cdr stat) 0)
                               :hash (cdr (assoc path hashes))))))
                   entries)))))

(defun diff-review--file (&optional index)
  "The file plist at INDEX, by default the one shown, or nil."
  (and diff-review--files
       (aref diff-review--files (or index diff-review--index))))

;;;; The file

(defun diff-review--read ()
  "Parse the review file in the current buffer into the buffer's state."
  (let* ((data (condition-case err
                   (json-parse-string (buffer-string) :null-object nil :false-object nil)
                 (error (user-error "Not a review file: %s" (error-message-string err)))))
         (root (gethash "root" data))
         (file-root (and buffer-file-name
                         (diff-review--toplevel (file-name-directory buffer-file-name)))))
    (unless (equal (gethash "format" data) diff-review-format)
      (user-error "Not a review file: format is %S" (gethash "format" data)))
    (setq diff-review--root
          (cond (root (file-name-as-directory (expand-file-name root)))
                (file-root)
                (t (user-error "The review names no root and is not inside a repository"))))
    (unless (file-directory-p diff-review--root)
      (user-error "The review's root %s does not exist" diff-review--root))
    (puthash "root" diff-review--root data)
    (puthash "base" (or (gethash "base" data) (diff-review--default-base diff-review--root))
             data)
    (unless (hash-table-p (gethash "viewed" data))
      (puthash "viewed" (make-hash-table :test #'equal) data))
    (setq diff-review--data data
          diff-review--base-name (gethash "base" data)
          diff-review--head (gethash "head" data)
          diff-review--viewed (gethash "viewed" data)
          diff-review--comments (let ((comments (gethash "comments" data)))
                                  (if (vectorp comments) comments [])))))

(defun diff-review--write ()
  "Write the buffer's state to its file, leaving the buffer unmodified.
Returns t, as `write-contents-functions' expects."
  (let ((file buffer-file-name)
        ;; Parsed null and false are nil: written back as null, not as {}.
        (json (json-serialize diff-review--data :null-object nil)))   ; here: no data in the temp buffer
    (with-temp-buffer
      (insert json)
      (json-pretty-print-buffer)
      (insert "\n")
      (write-region (point-min) (point-max) file nil 'quiet)))
  (set-visited-file-modtime)
  (set-buffer-modified-p nil)
  t)

;;;; Viewed marks

(defun diff-review--viewed-p (file)
  "Non-nil when FILE was marked viewed at its current contents."
  (equal (gethash (plist-get file :path) diff-review--viewed) (plist-get file :hash)))

(defun diff-review--set-viewed (file flag)
  "Mark FILE viewed at its current contents when FLAG, else unmark it, and write."
  (unless (verify-visited-file-modtime)   ; someone else wrote it: take theirs first
    (diff-review--revert))
  (if flag
      (puthash (plist-get file :path) (plist-get file :hash) diff-review--viewed)
    (remhash (plist-get file :path) diff-review--viewed))
  (diff-review--write))

;;;; Comments

;; A comment is the hash table the file holds, read and written as is.

(defun diff-review--author ()
  "The name a comment written here carries.  The review buffer must be current."
  (or diff-review-author
      (let ((default-directory diff-review--root))
        (diff-review--git-maybe "config" "user.name"))
      user-login-name))

(defun diff-review--comment-id ()
  "A short id no comment of the review has."
  (let (id)
    (while (or (null id) (diff-review--comment id))
      (setq id (format "%06x" (random #x1000000))))
    id))

(defun diff-review--comment (id)
  "The comment with ID, or nil."
  (seq-find (lambda (c) (equal (gethash "id" c) id)) diff-review--comments))

(defun diff-review--message (author body)
  "A message hash table for a comment or a reply: AUTHOR, now, BODY."
  (let ((m (make-hash-table :test #'equal)))
    (puthash "author" author m)
    (puthash "at" (format-time-string "%FT%T%:z") m)
    (puthash "body" body m)
    m))

(defun diff-review--comment-lines (comment)
  "COMMENT's lines as (FIRST . LAST), or nil for a comment on the review."
  (let ((lines (gethash "lines" comment)))
    (and (vectorp lines) (= (length lines) 2)
         (cons (aref lines 0) (aref lines 1)))))

(defun diff-review--file-comments (path)
  "The comments on PATH, in the review's order."
  (seq-filter (lambda (c) (equal (gethash "path" c) path)) diff-review--comments))

(defun diff-review--open-count (path)
  "How many threads on PATH are not resolved."
  (seq-count (lambda (c) (not (gethash "resolved" c))) (diff-review--file-comments path)))

(defun diff-review--modify-comments (fn)
  "Call FN to change the comments, then write and redraw.
Someone else's write since the last read is taken first, so an agent
appending beside Emacs loses nothing.  The review buffer must be current."
  (unless (verify-visited-file-modtime)
    (diff-review--revert))
  (funcall fn)
  (puthash "comments" diff-review--comments diff-review--data)
  (diff-review--write)
  (diff-review--render-all)
  (diff-review--redecorate-files))

(defun diff-review--add-comment (path lines blob body)
  "Add a thread of BODY on LINES of PATH as BLOB has it, and return it.
Without PATH it is on the review as a whole.  The review buffer must be current."
  (let ((c (make-hash-table :test #'equal)))
    (puthash "id" (diff-review--comment-id) c)
    (when path
      (puthash "path" path c)
      (puthash "lines" (vector (car lines) (cdr lines)) c)
      (when blob (puthash "blob" blob c)))
    (let ((m (diff-review--message (diff-review--author) body)))
      (maphash (lambda (k v) (puthash k v c)) m))
    (puthash "replies" [] c)
    (diff-review--modify-comments
     (lambda () (setq diff-review--comments (vconcat diff-review--comments (vector c)))))
    c))

(defun diff-review--hunks-to-here (old)
  "The hunks from blob OLD to what this file buffer shows, or the symbol `error'.
Each is (OLD-START OLD-COUNT NEW-COUNT).  A revision buffer diffs two
blobs; a file buffer diffs OLD against the saved file, whose blob git has
not stored.  OLD may be a blob git has not either: then `error'."
  (let ((root (diff-review--in-review diff-review--root))
        (path (diff-review--in-review (plist-get (diff-review--file) :path)))
        (blob diff-review--blob)
        hunks)
    (condition-case nil
        (with-temp-buffer
          (let ((default-directory root)
                (status
                 (if blob
                     (process-file "git" nil t nil "diff" "-U0" old blob)
                   (let ((file (make-temp-file "diff-review-blob")))
                     (unwind-protect
                         (and (zerop (process-file "git" nil `(:file ,file) nil
                                                   "cat-file" "blob" old))
                              (process-file "git" nil t nil "diff" "--no-index" "-U0"
                                            file (expand-file-name path root)))
                       (delete-file file))))))
            (unless (memq status '(0 1))   ; 1: --no-index found differences
              (error "No diff")))
          (goto-char (point-min))
          (while (re-search-forward diff-hunk-header-re-unified nil t)
            (push (list (string-to-number (match-string 1))
                        (if (match-string 2) (string-to-number (match-string 2)) 1)
                        (if (match-string 4) (string-to-number (match-string 4)) 1))
                  hunks))
          (nreverse hunks))
      (error 'error))))

(defun diff-review--map-lines (hunks lines)
  "LINES, (FIRST . LAST) in the old blob, as they are in the new one.
HUNKS is between the two.  Nil when a hunk changed lines inside the range:
then the thread is outdated, as a pull request page says."
  (let ((shift 0) (first (car lines)) (last (cdr lines)) stale)
    (pcase-dolist (`(,start ,count ,new-count) hunks)
      (cond
       ;; A pure insertion sits after line START: lines up to it stay.
       ((zerop count)
        (cond ((< start first) (cl-incf shift new-count))
              ((< start last) (setq stale t))))
       ((< (+ start count -1) first) (cl-incf shift (- new-count count)))
       ((<= start last) (setq stale t))))
    (unless stale (cons (+ first shift) (+ last shift)))))

;;;; Rendering

(defun diff-review--glyph (glyph ascii)
  "GLYPH where the frame can draw it, else its stand-in ASCII."
  (if (or (display-graphic-p) (seq-every-p #'char-displayable-p glyph))
      glyph
    ascii))

(defun diff-review--status-face (status)
  "The face for a git STATUS letter."
  (pcase status
    ("A" 'diff-review-file-added)
    ("D" 'diff-review-file-deleted)
    (_ 'diff-review-file-changed)))

(defun diff-review--counts (added removed)
  "+ADDED and −REMOVED, in the diff indicator faces."
  (concat (propertize (format "+%d" added) 'face 'diff-indicator-added) " "
          (propertize (format "%s%d" (diff-review--glyph "−" "-") removed) 'face 'diff-indicator-removed)))

(defun diff-review--sum (files key)
  "The sum of KEY over the plists FILES."
  (apply #'+ (mapcar (lambda (f) (plist-get f key)) files)))

(defun diff-review--summary ()
  "The viewed count, the line counts and the comment counts of the review."
  (let ((n (length diff-review--comments))
        (open (seq-count (lambda (c) (not (gethash "resolved" c))) diff-review--comments)))
    (concat
     (format "%d/%d viewed  %s"
             (seq-count #'diff-review--viewed-p diff-review--files)
             (length diff-review--files)
             (diff-review--counts (diff-review--sum diff-review--files :added)
                                  (diff-review--sum diff-review--files :removed)))
     (if (zerop n) ""
       (format "  %d comment%s, %d open" n (if (= n 1) "" "s") open)))))

(defun diff-review--comment-mark (path)
  "The mark a list line carries for PATH's open threads, or an empty string."
  (let ((open (diff-review--open-count path)))
    (if (zerop open) ""
      (propertize (format "%s%d " (diff-review--glyph "◆" "#") open)
                  'face 'diff-review-comment-author))))

(defun diff-review--stats (files &optional mark)
  "The added and removed counts of FILES, after MARK, against the right edge."
  (let ((stats (concat mark (diff-review--counts (diff-review--sum files :added)
                                                 (diff-review--sum files :removed)))))
    (concat (propertize " " 'display `(space :align-to (- right ,(1+ (string-width stats)))))
            stats)))

(defvar-keymap diff-review-file-line-map
  :doc "Keymap on a file's line in a list: a click shows the file."
  "<mouse-2>" #'diff-review-mouse-open)

(defun diff-review--file-line (i indent)
  "The list line of file I, after INDENT."
  (let* ((file (diff-review--file i))
         (path (plist-get file :path))
         (viewed (diff-review--viewed-p file)))
    (propertize (concat indent
                        (propertize (plist-get file :status)
                                    'face (diff-review--status-face (plist-get file :status)))
                        " "
                        (propertize (file-name-nondirectory path)
                                    'face (cond ((eql i diff-review--index) 'bold)
                                                (viewed 'shadow)))
                        (diff-review--stats
                         (list file)
                         (concat (diff-review--comment-mark path)
                                 (and viewed (propertize (concat (diff-review--glyph "✓" "*") " ")
                                                         'face 'success))))
                        "\n")
                'diff-review-index i
                'keymap diff-review-file-line-map
                'follow-link t
                'mouse-face 'highlight
                'help-echo (if (plist-get file :old)
                               (format "%s %s %s"
                                       (plist-get file :old) (diff-review--glyph "→" "->") path)
                             path))))

(defun diff-review--tree ()
  "The files as a tree: an alist of name to a file index or a subtree.
A directory holding only a directory is merged with it, as in a/b."
  (let ((root (list nil)))
    (dotimes (i (length diff-review--files))
      (let ((node root)
            (parts (split-string (plist-get (diff-review--file i) :path) "/")))
        (while (cdr parts)
          (setq node (or (assoc (car parts) (cdr node))
                         (car (setcdr (last node) (list (list (car parts)))))))
          (pop parts))
        (setcdr (last node) (list (cons (car parts) i)))))
    (diff-review--merge-dirs (cdr root))))

(defun diff-review--merge-dirs (tree)
  "TREE with each directory holding only a directory merged with it."
  (mapcar (lambda (entry)
            (let ((name (car entry)) (child (cdr entry)))
              (while (and (consp child) (null (cdr child)) (consp (cdar child)))
                (setq name (concat name "/" (caar child))
                      child (cdar child)))
              (cons name (if (consp child) (diff-review--merge-dirs child) child))))
          tree))

(defun diff-review--tree-files (tree)
  "The file plists under TREE."
  (mapcan (lambda (entry)
            (if (integerp (cdr entry))
                (list (diff-review--file (cdr entry)))
              (diff-review--tree-files (cdr entry))))
          tree))

(defun diff-review--insert-tree (tree indent)
  "Insert TREE as sections, its entries after INDENT."
  (pcase-dolist (`(,name . ,child) tree)
    (if (integerp child)
        (magit-insert-section (diff-review-file child)
          (insert (diff-review--file-line child indent)))
      (magit-insert-section (diff-review-dir name)
        (let ((files (diff-review--tree-files child)))
          (magit-insert-heading
            indent
            (propertize (concat name "/")
                        'face (if (seq-every-p #'diff-review--viewed-p files)
                                  'shadow
                                'font-lock-comment-face))
            (diff-review--stats files)))
        (diff-review--insert-tree child (concat indent "  "))))))

(defun diff-review--insert-files ()
  "Insert the file tree, inside a root section."
  (if (zerop (length diff-review--files))
      (insert (propertize "No changes.\n" 'face 'shadow))
    (diff-review--insert-tree (diff-review--tree) "")))

(defun diff-review--comment-line (comment)
  "The overview line of COMMENT: where it is, who wrote it, how it starts."
  (let* ((path (gethash "path" comment))
         (lines (diff-review--comment-lines comment))
         (body (or (gethash "body" comment) ""))
         (first (car (split-string body "\n")))
         (resolved (gethash "resolved" comment))
         (replies (length (or (gethash "replies" comment) []))))
    (propertize
     (concat
      "  "
      (propertize (cond ((null path) "review")
                        ((null lines) path)
                        ((= (car lines) (cdr lines)) (format "%s:%d" path (car lines)))
                        (t (format "%s:%d-%d" path (car lines) (cdr lines))))
                  'face (if resolved 'shadow 'default))
      "  "
      (propertize (or (gethash "author" comment) "?") 'face 'diff-review-comment-author)
      "  "
      (propertize first 'face (and resolved 'shadow))
      (if (zerop replies) ""
        (propertize (format "  +%d" replies) 'face 'shadow))
      (if resolved
          (propertize (concat "  " (diff-review--glyph "✓" "*")) 'face 'success)
        "")
      "\n")
     'diff-review-comment (gethash "id" comment)
     'keymap diff-review-file-line-map
     'follow-link t
     'mouse-face 'highlight)))

(defun diff-review--insert-comments ()
  "Insert the comment threads as a section, when there are any."
  (when (> (length diff-review--comments) 0)
    (insert "\n")
    (magit-insert-section (diff-review-comments)
      (magit-insert-heading "Comments")
      ;; Review-wide comments first, then the files in the tree's order.
      (let ((order (mapcar (lambda (f) (plist-get f :path)) diff-review--files)))
        (dolist (c (sort (append diff-review--comments nil)
                         (lambda (a b)
                           (let ((pa (gethash "path" a)) (pb (gethash "path" b)))
                             (cond ((null pa) (not (null pb)))
                                   ((null pb) nil)
                                   ((not (equal pa pb))
                                    (< (or (cl-position pa order) most-positive-fixnum)
                                       (or (cl-position pb order) most-positive-fixnum)))
                                   (t (< (or (car (diff-review--comment-lines a)) 0)
                                         (or (car (diff-review--comment-lines b)) 0))))))))
          (insert (diff-review--comment-line c)))))))

(defun diff-review--goto-current ()
  "Put point on the current file's line, or the first file."
  (goto-char (point-min))
  (let ((pos (or (and diff-review--index
                      (text-property-any (point-min) (point-max)
                                         'diff-review-index diff-review--index))
                 (next-single-property-change (point-min) 'diff-review-index))))
    (when pos (goto-char pos)))
  (dolist (window (get-buffer-window-list nil nil t))
    (set-window-point window (point))))

(defun diff-review--render ()
  "Redraw the overview.  The review buffer must be current."
  (let ((inhibit-read-only t)
        (n (length diff-review--files)))
    (erase-buffer)
    (magit-insert-section (diff-review-root)
      (insert (propertize (format "%s %s %s" diff-review--head-name
                                  (diff-review--glyph "←" "<-") diff-review--base-name)
                          'face 'diff-review-title)
              "\n\n"
              (format "%d file%s changed, " n (if (= n 1) "" "s"))
              (diff-review--summary) "\n"
              (propertize (format "%s\nmerge-base %s\n%s\n"
                                  (abbreviate-file-name diff-review--root)
                                  (substring diff-review--base 0 10)
                                  (abbreviate-file-name buffer-file-name))
                          'face 'shadow)
              "\n")
      (diff-review--insert-files)
      (diff-review--insert-comments))
    (set-buffer-modified-p nil)
    (diff-review--goto-current)))

(defun diff-review--render-all ()
  "Redraw the overview and the sidebar.  The review buffer must be current."
  (diff-review--render)
  (when (buffer-live-p diff-review--sidebar)
    (with-current-buffer diff-review--sidebar (diff-review-sidebar--render)))
  (force-mode-line-update t))

(defun diff-review--redecorate-files ()
  "Redraw the decorations of this review's open files.
The review buffer must be current."
  (let ((review (current-buffer)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and diff-review-file-mode (eq diff-review--review review))
          (diff-review--decorate))))))

(defun diff-review--load ()
  "Read the file, scan git and render.  The buffer holds the file's text."
  (diff-review--read)
  (diff-review--scan)
  (let ((path (and diff-review--index
                   (< diff-review--index (length diff-review--files))
                   (plist-get (diff-review--file) :path))))
    (setq diff-review--index
          (and path (cl-position path diff-review--files
                                 :test (lambda (p f) (equal p (plist-get f :path)))))))
  (diff-review--render-all)
  (diff-review--redecorate-files))   ; an agent may have written comments

(defun diff-review--revert (&rest _)
  "Reload the review from its file, as `revert-buffer-function'."
  (let ((inhibit-read-only t))
    (set-visited-file-modtime)          ; the disk copy is what we want: no supersession prompt
    (insert-file-contents buffer-file-name nil nil nil t))
  (diff-review--load))

;;;; Commands on a list, in the overview or the sidebar

(defun diff-review--index-at-point ()
  "The index of the file on this line, or nil."
  (get-text-property (line-beginning-position) 'diff-review-index))

(defun diff-review-open-at-point ()
  "Show the file on this line, or the comment; on a directory, fold or unfold it."
  (interactive)
  (if-let* ((i (diff-review--index-at-point)))
      (diff-review--in-review (diff-review--open i))
    (if-let* ((id (get-text-property (line-beginning-position) 'diff-review-comment)))
        (diff-review--in-review (diff-review--goto-comment id))
      (magit-section-toggle (magit-current-section)))))

(defun diff-review-mouse-open (event)
  "Show the file clicked in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (diff-review-open-at-point))

(defun diff-review-next-file ()
  "Show the next file."
  (interactive)
  (diff-review--in-review
    (diff-review--open (if diff-review--index
                           (min (1- (length diff-review--files)) (1+ diff-review--index))
                         0))))

(defun diff-review-previous-file ()
  "Show the previous file."
  (interactive)
  (diff-review--in-review
    (diff-review--open (if diff-review--index (max 0 (1- diff-review--index)) 0))))

(defun diff-review-next-unviewed ()
  "Show the next file that is not viewed, wrapping around."
  (interactive)
  (diff-review--in-review
    (let* ((n (length diff-review--files))
           (from (or diff-review--index -1))
           (i (cl-loop for k from 1 to n
                       for j = (mod (+ from k) n)
                       unless (diff-review--viewed-p (diff-review--file j))
                       return j)))
      (if i (diff-review--open i) (message "Every file is viewed")))))

(defun diff-review-toggle-viewed ()
  "Toggle the viewed mark of the file at point, or of the file shown."
  (interactive)
  (let ((i (diff-review--index-at-point)))
    (diff-review--in-review
      (let ((file (or (diff-review--file (or i diff-review--index))
                      (user-error "No file to mark"))))
        (diff-review--set-viewed file (not (diff-review--viewed-p file)))
        (diff-review--render-all)))))

(defun diff-review-refresh ()
  "Reread the file and rescan the diff.
A file that changed since it was viewed loses its mark."
  (interactive)
  (diff-review--in-review (revert-buffer)))

(defun diff-review-overview ()
  "Show the overview in the main window."
  (interactive)
  (diff-review--show-in-main (or (diff-review--review-buffer)
                                 (user-error "Not in a review"))))

(defun diff-review-quit ()
  "End the review: its buffer, sidebar and decorations go, as does what it showed."
  (interactive)
  (kill-buffer (diff-review--in-review (current-buffer))))

(defun diff-review-toggle-sidebar ()
  "Show or hide this review's sidebar."
  (interactive)
  (when-let* ((main (window-parameter (selected-window) 'diff-review-main)))
    (select-window main))               ; this sidebar may be about to go
  (diff-review--in-review
    (setq diff-review--sidebar-enabled (not diff-review--sidebar-enabled)))
  (mapc #'diff-review--follow (frame-list)))

(defvar-keymap diff-review-mode-map
  :doc "Keymap for `diff-review-mode', shared with the sidebar."
  :parent magit-section-mode-map
  "RET" #'diff-review-open-at-point
  "n" #'diff-review-next-file
  "p" #'diff-review-previous-file
  "u" #'diff-review-next-unviewed
  "v" #'diff-review-toggle-viewed
  "g" #'diff-review-refresh
  "t" #'diff-review-toggle-sidebar
  "q" #'diff-review-quit)

;;;###autoload
(define-derived-mode diff-review-mode magit-section-mode "Review"
  "Major mode for a review file: an overview of what a worktree changed.
\\<diff-review-mode-map>\\[diff-review-open-at-point] shows the file at point in this window,
or folds a directory; a click on a file shows it too.
\\[diff-review-next-file] and \\[diff-review-previous-file] walk the files;
\\[diff-review-next-unviewed] skips to the next unviewed one.
\\[diff-review-toggle-viewed] toggles the viewed mark, \\[diff-review-refresh] rescans
and \\[diff-review-toggle-sidebar] shows or hides the sidebar.
\\[diff-review-quit] ends the review, as does killing the buffer.

\\{diff-review-mode-map}"
  ;; `setq-local': in Emacs 31 a plain `setq' sets the global default, and
  ;; every file buffer's auto-revert then runs this, leaving it modified.
  (setq-local revert-buffer-function #'diff-review--revert)
  (setq diff-review--sidebar-enabled diff-review-sidebar
        diff-review--sidebar-width diff-review-sidebar-width
        diff-review--inline-removed diff-review-inline-removed)
  (add-hook 'write-contents-functions #'diff-review--write nil t)
  (add-hook 'kill-buffer-hook #'diff-review--close nil t)
  ;; Global, so it lives only while there is a review: see `diff-review--close'.
  (add-hook 'window-buffer-change-functions #'diff-review--follow)
  (display-line-numbers-mode -1)
  (auto-revert-mode 1)
  (diff-review--load))

;;;###autoload
(add-to-list 'magic-mode-alist
             (cons "\\`\\s-*{\\s-*\"format\"\\s-*:\\s-*\"diff-review/" #'diff-review-mode))

(defun diff-review--leave (buffer leave)
  "Call LEAVE, then show what came before BUFFER in the windows that showed it.
Their dedication is lifted meanwhile, else killing BUFFER deletes a
dedicated one."
  (let* ((windows (get-buffer-window-list buffer nil t))
         (dedicated (mapcar #'window-dedicated-p windows)))
    (dolist (window windows) (set-window-dedicated-p window nil))
    (funcall leave)
    (cl-loop for window in windows
             for was in dedicated
             ;; A killed buffer has left its windows already.
             do (when (eq (window-buffer window) buffer)
                  (switch-to-prev-buffer window))
                (set-window-dedicated-p window was))))

(defun diff-review--close ()
  "End the review being killed: its files' decorations and its sidebar.
Windows showing the review or its files go back to what they showed
before it; buffers showing a revision of a file are killed.  The last
review to go takes `diff-review--follow' off the window hook."
  (let ((review (current-buffer)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and diff-review-file-mode (eq diff-review--review review))
          (diff-review--leave buffer (lambda ()
                                       (if buffer-file-name
                                           (diff-review-file-mode -1)
                                         (kill-buffer)))))))
    (diff-review--leave review #'ignore)
    (when (buffer-live-p diff-review--sidebar)
      (kill-buffer diff-review--sidebar))
    (unless (seq-some (lambda (buffer)
                        (and (not (eq buffer review))
                             (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                                      'diff-review-mode)))
                      (buffer-list))
      (remove-hook 'window-buffer-change-functions #'diff-review--follow))))

;;;; The sidebar

(defvar-keymap diff-review-sidebar-mode-map
  :doc "Keymap for `diff-review-sidebar-mode'."
  :parent diff-review-mode-map
  "q" #'diff-review-toggle-sidebar)

(define-derived-mode diff-review-sidebar-mode magit-section-mode "Review-Sidebar"
  "Major mode for a review's sidebar: its changed files as a tree.
The sidebar shows beside the window the review shows in, the two
as one atomic window.  The keys are those of `diff-review-mode';
\\<diff-review-sidebar-mode-map>\\[diff-review-toggle-sidebar] hides the sidebar.

\\{diff-review-sidebar-mode-map}"
  ;; The header line carries the summary; a mode line under the tree adds nothing.
  (setq mode-line-format nil
        header-line-format '(:eval (diff-review-sidebar--header)))
  (add-hook 'kill-buffer-hook #'diff-review-sidebar--close nil t)
  (add-hook 'window-size-change-functions #'diff-review-sidebar--resized nil t)
  (display-line-numbers-mode -1))

(defun diff-review--sidebar-buffer (review)
  "REVIEW's sidebar buffer, created if needed."
  (with-current-buffer review
    (unless (buffer-live-p diff-review--sidebar)
      (let ((sidebar (generate-new-buffer
                      (format " *review sidebar: %s*" (buffer-name review)))))
        (with-current-buffer sidebar
          (diff-review-sidebar-mode)
          (setq diff-review--review review)
          (diff-review-sidebar--render))
        (setq diff-review--sidebar sidebar)))
    diff-review--sidebar))

(defun diff-review-sidebar--header ()
  "The sidebar's header line: the head and the review's summary."
  (diff-review--in-review
    (concat (propertize (format " %s  " diff-review--head-name) 'face 'bold)
            (diff-review--summary))))

(defun diff-review-sidebar--render ()
  "Redraw the sidebar.  The sidebar buffer must be current."
  (let ((inhibit-read-only t)
        (review (diff-review--review-buffer)))
    (erase-buffer)
    (when review
      (let ((moved (not (eql diff-review--index (buffer-local-value 'diff-review--index review)))))
        ;; Local copies of the review's state, for the rendering it shares.
        (dolist (var '(diff-review--files diff-review--index diff-review--viewed
                       diff-review--comments))
          (set (make-local-variable var) (buffer-local-value var review)))
        (magit-insert-section (diff-review-root)
          (diff-review--insert-files))
        (diff-review--goto-current)
        (when (and moved diff-review--index)   ; show the new file, else keep the folds
          (magit-section-reveal (magit-current-section)))))))

;; A sidebar window and the window it is attached to point at each other
;; through window parameters; the link holds while both ends agree.

(defun diff-review--sidebar-window (window)
  "The sidebar window attached to WINDOW, or nil."
  (let ((sidebar (window-parameter window 'diff-review-sidebar)))
    (and (window-live-p sidebar)
         (eq (window-parameter sidebar 'diff-review-main) window)
         sidebar)))

(defun diff-review--attach (window sidebar)
  "Show SIDEBAR beside WINDOW, the two as one atomic window."
  (when-let* ((new (display-buffer-in-atom-window
                    sidebar `((window . ,window)
                              (side . ,diff-review-sidebar-side)
                              (window-width
                               . ,(with-current-buffer sidebar
                                   (diff-review--in-review diff-review--sidebar-width)))
                              ;; Kept through balancing and frame resizes;
                              ;; the user's own resizing still works.
                              (preserve-size . (t . nil))
                              (dedicated . t)))))
    (set-window-parameter new 'diff-review-main window)
    (set-window-parameter window 'diff-review-sidebar new)
    (with-current-buffer sidebar (diff-review-sidebar--render))))

(defun diff-review-sidebar--resized (window)
  "Keep the width WINDOW, a sidebar, was resized to.
The review shows its sidebar at that width from now on, and the width is
preserved again: a resize does not update a preserved width, nor does
`enlarge-window' leave it preserved."
  (when-let* ((review (with-current-buffer (window-buffer window)
                        (diff-review--review-buffer))))
    (with-current-buffer review
      (setq diff-review--sidebar-width (window-total-width window)))
    (window-preserve-size window t t)))

(defun diff-review--detach (sidebar)
  "Delete the sidebar window SIDEBAR, freeing the window it was attached to."
  (let ((main (window-parameter sidebar 'diff-review-main)))
    (let ((ignore-window-parameters t))   ; else the whole atomic window goes
      (delete-window sidebar))
    (when (window-live-p main)
      (set-window-parameter main 'window-atom nil)
      (set-window-parameter main 'diff-review-sidebar nil))))

(defun diff-review-sidebar--close ()
  "Delete the windows of the sidebar buffer being killed."
  (mapc #'diff-review--detach (get-buffer-window-list nil nil t)))

(defun diff-review--follow (&optional frame)
  "Give each window of FRAME the sidebar its buffer's review wants, or none.
Runs after a window of FRAME changes buffer."
  (dolist (window (window-list (if (framep frame) frame (selected-frame)) 'no-minibuffer))
    (when (window-live-p window)        ; not deleted earlier in this loop
      (with-current-buffer (window-buffer window)
        (if (derived-mode-p 'diff-review-sidebar-mode)
            (let ((main (window-parameter window 'diff-review-main)))
              (unless (and (window-live-p main)
                           (eq (diff-review--sidebar-window main) window))
                (diff-review--detach window)))   ; an orphan: restored or left behind
          (let* ((review (diff-review--review-buffer))
                 (wanted (and review
                              (buffer-local-value 'diff-review--sidebar-enabled review)
                              (diff-review--sidebar-buffer review)))
                 (sidebar (diff-review--sidebar-window window)))
            (unless (eq (and sidebar (window-buffer sidebar)) wanted)
              (when sidebar (diff-review--detach sidebar))
              (when wanted (diff-review--attach window wanted)))))))))

;;;; Showing a file

(defun diff-review--main-window ()
  "The window to show the review in.
The selected one, or the one the selected sidebar is attached to."
  (or (window-parameter (selected-window) 'diff-review-main)
      (selected-window)))

(defun diff-review--undedicated (window fn &rest args)
  "Apply FN to ARGS with WINDOW undedicated, then dedicate it as it was."
  (let ((dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window nil)
    (unwind-protect (apply fn args)
      (when (window-live-p window)
        (set-window-dedicated-p window dedicated)))))

(defun diff-review--show-in-main (buffer)
  "Show BUFFER in the review's window and select it.
The window's history is left as it was, so quitting or killing the
review goes back to what the window showed before it.  A window
dedicated to something outside the review is not the review's to
take: then BUFFER goes where `display-buffer' says."
  (let ((window (diff-review--main-window)))
    (if (and (window-dedicated-p window)
             (not (eq (with-current-buffer (window-buffer window) (diff-review--review-buffer))
                      (with-current-buffer buffer (diff-review--review-buffer)))))
        (progn (pop-to-buffer-same-window buffer)
               (selected-window))
      (let ((prev (window-prev-buffers window))
            (next (window-next-buffers window)))
        (diff-review--undedicated window #'set-window-buffer window buffer)
        (set-window-prev-buffers window prev)
        (set-window-next-buffers window next)
        (select-window window)
        window))))

(defun diff-review--blob-buffer (rev path key &optional deleted)
  "A read-only buffer with PATH as it is at REV; DELETED when the file is gone.
KEY identifies the content, a blob hash or a commit: the buffer is built
once and reused while its KEY holds, so revisits cost no git call and no
mode start, and its overlays are not emptied onto the first line."
  (let ((buffer (get-buffer-create (format "*%s: %s*" (if deleted "deleted" rev) path))))
    (unless (equal (buffer-local-value 'diff-review--blob buffer) key)
      (let ((contents (diff-review--git "show" (concat rev ":" path)))
            (root diff-review--root))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (delete-all-overlays)
            (erase-buffer)
            (insert contents)
            (let ((buffer-file-name (expand-file-name path root)))
              (set-auto-mode))
            (setq diff-review--deleted deleted
                  diff-review--blob key)
            (read-only-mode 1)))))
    buffer))

(defun diff-review--open (i)
  "Show file I in the main window, at its first hunk, and sync the lists.
The review buffer must be current."
  (unless (< -1 i (length diff-review--files))
    (user-error "The review has no files"))
  (setq diff-review--index i)
  (let* ((review (current-buffer))
         (file (diff-review--file i))
         (path (plist-get file :path))
         (buffer (cond ((equal (plist-get file :status) "D")
                        (diff-review--blob-buffer diff-review--base path (plist-get file :hash) t))
                       (diff-review--head
                        (diff-review--blob-buffer diff-review--head path (plist-get file :hash)))
                       (t (find-file-noselect (expand-file-name path diff-review--root))))))
    (with-current-buffer buffer
      (setq diff-review--review review)
      (diff-review-file-mode 1))
    (diff-review--render-all)
    (diff-review--show-in-main buffer)
    (with-current-buffer buffer          ; the caller may be in the review buffer
      (goto-char (point-min))
      (when diff-review--hunks
        (diff-review--goto-hunk (car diff-review--hunks))))))

;;;; The file side

(defun diff-review--parse-hunks (file)
  "Parse `git diff -U0' of FILE into hunk plists, in the review buffer."
  (let ((old (or (plist-get file :old) (plist-get file :path)))
        (default-directory diff-review--root)
        (range (diff-review--range))
        hunks)
    (with-temp-buffer
      (apply #'process-file "git" nil t nil "diff" "-U0" "-M"
             (append range (list "--" old (plist-get file :path))))
      (goto-char (point-min))
      (while (re-search-forward diff-hunk-header-re-unified nil t)
        (let ((line (string-to-number (match-string 3)))   ; the new side
              (count (if (match-string 4) (string-to-number (match-string 4)) 1))
              removed)
          (forward-line 1)
          (while (and (not (eobp)) (looking-at "^\\([-+]\\)\\(.*\\)$"))
            (when (equal (match-string 1) "-")
              (push (match-string 2) removed))
            (forward-line 1))
          (push (list :line line :count count :removed (nreverse removed)) hunks))))
    (nreverse hunks)))

(defun diff-review--line-pos (line)
  "The position where LINE starts."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun diff-review--hunk-line (hunk)
  "The buffer line HUNK starts on; a pure deletion sits before the next line."
  (if (zerop (plist-get hunk :count))
      (1+ (plist-get hunk :line))
    (plist-get hunk :line)))

(when (fboundp 'define-fringe-bitmap)   ; absent from a build without a window system
  (define-fringe-bitmap 'diff-review-bar [224] nil nil '(center repeated)))

(defun diff-review--tint (face source)
  "FACE as an overlay face, with SOURCE's background when FACE sets none."
  (let ((own (face-attribute face :background nil t))
        (borrowed (face-attribute source :background nil t)))
    (if (or (stringp own) (not (stringp borrowed)))
        face
      (list face `(:background ,borrowed :extend t)))))

(defun diff-review--fringe-mark (face)
  "A string that draws a bar in `diff-review-fringe' on its line, in FACE.
Empty without that option or the bitmap.  A text terminal draws nothing for
it, so it is the same string on every frame."
  (if (and diff-review-fringe (fboundp 'fringe-bitmap-p) (fringe-bitmap-p 'diff-review-bar))
      (propertize " " 'display `(,(if (eq diff-review-fringe 'right) 'right-fringe 'left-fringe)
                                 diff-review-bar ,face))
    ""))

(defun diff-review--bars (start lines mark)
  "Put MARK before each of LINES lines from START, in zero-width overlays.
Not `line-prefix': an overlay's would hide the one `org-indent-mode' draws."
  (unless (string-empty-p mark)
    (save-excursion
      (goto-char start)
      (dotimes (_ lines)
        (let ((overlay (make-overlay (point) (point))))
          (overlay-put overlay 'before-string mark)
          (diff-review--keep overlay))
        (forward-line 1)))))

(defun diff-review--fontify (lines mode)
  "Copies of LINES with the syntax faces MODE gives them, as one text.
One mode start for all of them: starting a mode is the slow part."
  (condition-case nil
      (if (or (null lines) (eq mode 'fundamental-mode))
          (mapcar #'copy-sequence lines)   ; copies: the hunks' own strings stay bare
        (with-temp-buffer
          (insert (string-join lines "\n"))
          (delay-mode-hooks (funcall mode))
          (font-lock-ensure)
          (split-string (buffer-string) "\n")))
    (error (mapcar #'copy-sequence lines))))

(defun diff-review--removed-string (lines tint)
  "The inline rendering of removed LINES, fontified already: fringe-marked.
TINT, a face or nil, goes over the syntax faces."
  (let ((mark (diff-review--fringe-mark 'diff-indicator-removed)))
    (mapconcat (lambda (text)
                 (when tint (add-face-text-property 0 (length text) tint t text))
                 (concat mark text (propertize "\n" 'face tint)))
               lines)))

(defun diff-review--decorate ()
  "Add the inline removed lines, the added tint, the fringe bars and the folds."
  (diff-review--undecorate)
  (let* ((inline (diff-review--in-review diff-review--inline-removed))
         (added (and diff-review-tint-added (diff-review--tint 'diff-review-added 'diff-added)))
         (removed (and diff-review-tint-removed
                       (diff-review--tint 'diff-review-removed 'diff-removed)))
         (texts (and inline      ; the file's removed lines, fontified together
                     (diff-review--fontify
                      (seq-mapcat (lambda (hunk) (plist-get hunk :removed)) diff-review--hunks)
                      major-mode))))
    (diff-review--decorate-comments)   ; first: the folds keep its lines
    (if diff-review--deleted
        (let ((overlay (make-overlay (point-min) (point-max))))
          (overlay-put overlay 'face removed)
          (diff-review--keep overlay)
          (diff-review--bars (point-min) (count-lines (point-min) (point-max))
                             (diff-review--fringe-mark 'diff-indicator-removed)))
      (dolist (hunk diff-review--hunks)
        (let* ((line (diff-review--hunk-line hunk))
               (start (diff-review--line-pos line))
               (count (plist-get hunk :count))
               (end (if (zerop count) start
                      (diff-review--line-pos (+ (plist-get hunk :line) count))))
               (overlay (make-overlay start end nil t nil))
               (mark (if (> count 0) (diff-review--fringe-mark 'diff-indicator-added) ""))
               above)
          (overlay-put overlay 'face (and (> count 0) added))
          (when (and inline (plist-get hunk :removed))
            (let ((n (length (plist-get hunk :removed))))
              (setq above (diff-review--removed-string (seq-take texts n) removed))
              (setq texts (nthcdr n texts))))
          ;; The first added line's bar goes after the removed lines, in one
          ;; string: two before-strings at one place have no reliable order.
          (overlay-put overlay 'before-string (concat above mark))
          (diff-review--keep overlay)
          (when (> count 1)
            (diff-review--bars (diff-review--line-pos (1+ line)) (1- count) mark))))
      (when diff-review--focus (diff-review--fold)))))

(defun diff-review--fold ()
  "Hide unchanged lines beyond `diff-review-context' of any hunk."
  (let ((last (line-number-at-pos (point-max)))
        (dots (diff-review--glyph "⋯" "..."))
        keep)
    (dolist (hunk diff-review--hunks)
      (push (cons (max 1 (- (diff-review--hunk-line hunk) diff-review-context))
                  (min last (+ (plist-get hunk :line)
                               (max 0 (1- (plist-get hunk :count)))
                               diff-review-context)))
            keep))
    (pcase-dolist (`(,_ ,first ,end . ,_) diff-review--threads)   ; a thread stays in view
      (push (cons (max 1 (- first diff-review-context)) (min last (+ end diff-review-context)))
            keep))
    (setq keep (sort keep (lambda (a b) (< (car a) (car b)))))
    (let ((line 1))
      (dolist (range (append keep (list (cons (1+ last) (1+ last)))))
        (when (< line (car range))
          (let ((overlay (make-overlay (diff-review--line-pos line)
                                       (diff-review--line-pos (car range)))))
            (overlay-put overlay 'display
                         (propertize (format "   %s %d unchanged lines %s\n"
                                             dots (- (car range) line) dots)
                                     'face 'diff-review-unchanged))
            (diff-review--keep overlay)))
        (setq line (max line (1+ (cdr range))))))))

(defun diff-review--keep (overlay)
  "Mark OVERLAY as the review's, for `diff-review--undecorate' to find."
  (overlay-put overlay 'diff-review t)
  (push overlay diff-review--overlays))

(defun diff-review--undecorate ()
  "Remove the review overlays from this buffer.
By their mark, not the list: a revert kills the list and leaves them."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'diff-review) (delete-overlay overlay)))
  (setq diff-review--overlays nil
        diff-review--threads nil))

;;;; Comment threads in a file

(defun diff-review--buffer-blob (&optional store)
  "The blob hash of what this file buffer shows: the saved file, or the revision.
With STORE, a saved file's blob is written to git's objects, so the diff
that follows a thread after the file changes has something to start from."
  (or diff-review--blob
      (diff-review--in-review
        (let ((path (plist-get (diff-review--file) :path)))
          (if store
              (diff-review--git "hash-object" "-w" path)
            (car (diff-review--hashes (list path))))))))

(defun diff-review--thread-lines (comment blob)
  "Where COMMENT draws in a buffer showing BLOB: (FIRST LAST STALE).
Lines counted in another blob are mapped through `git diff' of the two,
once per pair; STALE when a change inside them makes that impossible, and
then they draw where they were.  Lines past the end draw at the end."
  (let* ((lines (diff-review--comment-lines comment))
         (old (gethash "blob" comment))
         (mapped (if (or (null old) (equal old blob))
                     lines
                   (let ((key (cons old blob)))
                     (unless (assoc key diff-review--maps)
                       (push (cons key (diff-review--hunks-to-here old)) diff-review--maps))
                     (let ((hunks (cdr (assoc key diff-review--maps))))
                       (and (listp hunks) (diff-review--map-lines hunks lines))))))
         (last (line-number-at-pos (point-max))))
    (if mapped
        (list (min (car mapped) last) (min (cdr mapped) last) nil)
      (list (min (car lines) last) (min (cdr lines) last) t))))

(defun diff-review--when (at)
  "AT, an RFC 3339 time, as its date and minute; anything else as it is."
  (if (and (stringp at) (string-match "\\`\\([0-9-]+\\)T\\([0-9]+:[0-9]+\\)" at))
      (concat (match-string 1 at) " " (match-string 2 at))
    (or at "")))

(defun diff-review--body-lines (body width)
  "BODY as lines no wider than WIDTH, each paragraph filled; blank lines kept."
  (with-temp-buffer
    (insert (or body ""))
    (let ((fill-column width))
      (fill-region (point-min) (point-max)))
    (split-string (buffer-string) "\n")))

(defun diff-review--thread-string (comment first last stale open)
  "COMMENT drawn under its lines FIRST to LAST: in full when OPEN, else one line.
STALE adds the outdated mark.  Every row carries the fringe bar the range
has, so range and thread read as one block.  Bodies fill at the buffer's
`fill-column', less the margin."
  (let* ((bubble (diff-review--glyph "💬" "#"))
         (bar (diff-review--fringe-mark 'diff-review-comment))
         (width (max 20 (- fill-column 4)))
         (who (lambda (m)
                (concat (propertize (or (gethash "author" m) "?") 'face 'diff-review-comment-author)
                        "  " (propertize (diff-review--when (gethash "at" m)) 'face 'shadow))))
         (marks (concat (if (gethash "resolved" comment)
                            (propertize (concat "  " (diff-review--glyph "✓" "*") " resolved")
                                        'face 'success)
                          "")
                        (if stale (propertize "  outdated" 'face 'warning) "")))
         (replies (or (gethash "replies" comment) []))
         lines)
    (if open
        (progn
          (push (concat bubble " " (funcall who comment) "  "
                        (propertize (if (= first last) (format "line %d" first)
                                      (format "lines %d-%d" first last))
                                    'face 'shadow)
                        marks)
                lines)
          (dolist (line (diff-review--body-lines (gethash "body" comment) width))
            (push (concat "  " line) lines))
          (seq-doseq (reply replies)
            (push (concat "  " (funcall who reply)) lines)
            (dolist (line (diff-review--body-lines (gethash "body" reply) width))
              (push (concat "    " line) lines)))
          (when diff-review-file-header-keys
            (push (concat "  " (diff-review--key-hints
                                '((diff-review-reply . "reply")
                                  (diff-review-toggle-resolved . "resolve")
                                  (diff-review-delete-comment . "delete"))))
                  lines)))
      (push (concat bubble " " (funcall who comment) "  "
                    (truncate-string-to-width
                     (car (split-string (or (gethash "body" comment) "") "\n"))
                     width nil nil (diff-review--glyph "…" "..."))
                    (if (zerop (length replies)) ""
                      (propertize (format "  +%d" (length replies)) 'face 'shadow))
                    marks)
            lines))
    (let ((text (concat (mapconcat (lambda (line) (concat bar line)) (nreverse lines) "\n")
                        "\n")))
      (add-face-text-property 0 (length text) 'diff-review-comment t text)
      text)))

(defun diff-review--thread-open-p (comment)
  "Whether COMMENT's thread draws in full here: all are, or it is point's."
  (or (diff-review--in-review diff-review--threads-open)
      (member (gethash "id" comment) diff-review--current)))

(defun diff-review--redraw-thread (thread)
  "Draw THREAD, (COMMENT FIRST LAST STALE OVERLAY), open or closed as it now is.
Open, its lines take a tint too; closed, they look as they did."
  (pcase-let* ((`(,comment ,first ,last ,stale ,overlay) thread)
               (open (diff-review--thread-open-p comment)))
    (overlay-put overlay 'face
                 (and open (diff-review--tint 'diff-review-comment-range 'diff-review-comment)))
    (overlay-put overlay 'after-string
                 (diff-review--thread-string comment first last stale open))))

(defun diff-review--decorate-comments ()
  "Draw this file's threads: each under the last line of its range.
The thread goes in the range's own overlay as its after-string, which
Emacs shows before a before-string at the same place, so the next hunk's
inline removed lines stay below it.  The range itself gets a fringe bar,
no tint: the code keeps the look it had."
  (let* ((path (diff-review--in-review (plist-get (diff-review--file) :path)))
         (comments (diff-review--in-review (diff-review--file-comments path)))
         (blob (and comments (diff-review--buffer-blob))))
    (dolist (comment comments)
      (when (diff-review--comment-lines comment)
        (pcase-let* ((`(,first ,last ,stale) (diff-review--thread-lines comment blob))
                     (overlay (make-overlay (diff-review--line-pos first)
                                            (diff-review--line-pos (1+ last)) nil t nil))
                     (thread (list comment first last stale overlay)))
          (overlay-put overlay 'diff-review-comment comment)
          (overlay-put overlay 'priority 1)      ; the open tint over the added one
          (overlay-put overlay 'evaporate t)     ; lines deleted under it: no stray thread
          (diff-review--keep overlay)
          (diff-review--bars (diff-review--line-pos first) (1+ (- last first))
                             (diff-review--fringe-mark 'diff-review-comment))
          (push thread diff-review--threads)
          (diff-review--redraw-thread thread))))
    (diff-review--track-thread)))

(defun diff-review--track-thread ()
  "Open the threads point is at and close the ones it left.
On `post-command-hook' in a file under review."
  (when diff-review--threads
    (let ((ids (mapcar (lambda (c) (gethash "id" c)) (diff-review--threads-at-point))))
      (unless (seq-set-equal-p ids diff-review--current)
        (let ((was diff-review--current))
          (setq diff-review--current ids)
          (dolist (thread diff-review--threads)
            (when (member (gethash "id" (car thread)) (append was ids))
              (diff-review--redraw-thread thread))))))))

(defun diff-review--threads-at-point ()
  "The comments at point: on their lines, or the line just under their threads.
Nearest first, by the start of their lines."
  (let ((bol (line-beginning-position)))
    (mapcar (lambda (o) (overlay-get o 'diff-review-comment))
            (sort (seq-filter
                   (lambda (o) (overlay-get o 'diff-review-comment))
                   (append (overlays-at (point))
                           (and (> bol (point-min))
                                (seq-filter (lambda (o) (= (overlay-end o) bol))
                                            (overlays-in (1- bol) bol)))))
                  (lambda (a b) (> (overlay-start a) (overlay-start b)))))))

(defun diff-review--thread-at-point ()
  "The comment at point, the nearest when there are several, or nil."
  (car (diff-review--threads-at-point)))

(defun diff-review--goto-line (line)
  "Move to LINE, near the top of the window, opening the thread there."
  (goto-char (diff-review--line-pos line))
  (recenter diff-review-hunk-recenter)
  (diff-review--track-thread))

(defun diff-review--goto-thread (comment)
  "Move to where COMMENT is drawn in this buffer."
  (when-let* ((thread (seq-find (lambda (th) (equal (gethash "id" (car th)) (gethash "id" comment)))
                                diff-review--threads)))
    (diff-review--goto-line (nth 1 thread))))

(defun diff-review--goto-comment (id)
  "Show comment ID where it is: its file at its lines.
A file outside the diff is visited plainly, there being no hunks to
draw.  The review buffer must be current."
  (let* ((comment (or (diff-review--comment id) (user-error "No comment %s" id)))
         (path (gethash "path" comment))
         (lines (diff-review--comment-lines comment))
         (i (and path (cl-position path diff-review--files
                                   :test (lambda (p f) (equal p (plist-get f :path)))))))
    (cond ((null path) (message "This comment is on the review as a whole"))
          (i (diff-review--open i)
             (with-current-buffer (window-buffer) (diff-review--goto-thread comment)))
          (t (diff-review--show-in-main
              (find-file-noselect (expand-file-name path diff-review--root)))
             (when lines (diff-review--goto-line (car lines)))))))

(defun diff-review--sorted-threads ()
  "This buffer's threads, by first line."
  (sort (copy-sequence diff-review--threads) (lambda (a b) (< (nth 1 a) (nth 1 b)))))

(defun diff-review--region-lines ()
  "The lines the region covers, else the line at point, as (FIRST . LAST).
A region ending at the start of a line stops before it."
  (if (use-region-p)
      (let ((first (line-number-at-pos (region-beginning))))
        (cons first (max first (line-number-at-pos (max (region-beginning) (1- (region-end)))))))
    (let ((line (line-number-at-pos))) (cons line line))))

(defun diff-review--read-body (prompt)
  "Read a comment body in a buffer, with PROMPT; empty is refused."
  (let ((body (string-trim-right (read-string-from-buffer prompt ""))))
    (when (string-empty-p body) (user-error "Nothing to say"))
    body))

(defun diff-review--thread-here ()
  "The comment at point, asked for when there are several; else an error."
  (let ((comments (diff-review--threads-at-point)))
    (cond ((null comments) (user-error "No comment on this line"))
          ((null (cdr comments)) (car comments))
          (t (let* ((names (mapcar (lambda (c)
                                     (cons (format "%s: %s" (gethash "author" c)
                                                   (car (split-string (or (gethash "body" c) "") "\n")))
                                           c))
                                   comments))
                    (pick (completing-read "Which thread: " (mapcar #'car names) nil t)))
               (cdr (assoc pick names)))))))

(defun diff-review-comment ()
  "Comment on the region's lines, or the line at point."
  (interactive)
  (unless diff-review-file-mode (user-error "Not in a file under review"))
  (let* ((lines (diff-review--region-lines))
         (path (diff-review--in-review (plist-get (diff-review--file) :path)))
         (blob (diff-review--buffer-blob t))
         (body (diff-review--read-body
                (format "Comment on %s:%d%s" path (car lines)
                        (if (= (car lines) (cdr lines)) "" (format "-%d" (cdr lines)))))))
    (deactivate-mark)
    (diff-review--in-review (diff-review--add-comment path lines blob body))))

(defun diff-review--change-comment (id fn)
  "Apply FN to the live table of comment ID, then write.
The table at point may be stale once someone else's write is taken."
  (diff-review--in-review
    (diff-review--modify-comments
     (lambda ()
       (funcall fn (or (diff-review--comment id)
                       (user-error "The comment was deleted meanwhile")))))))

(defun diff-review-reply ()
  "Reply to the comment thread at point."
  (interactive)
  (let* ((comment (diff-review--thread-here))
         (body (diff-review--read-body (format "Reply to %s" (gethash "author" comment)))))
    (diff-review--change-comment
     (gethash "id" comment)
     (lambda (c)
       (puthash "replies"
                (vconcat (or (gethash "replies" c) [])
                         (vector (diff-review--message (diff-review--author) body)))
                c)))))

(defun diff-review-toggle-resolved ()
  "Resolve the comment thread at point, or open it again."
  (interactive)
  (diff-review--change-comment
   (gethash "id" (diff-review--thread-here))
   (lambda (c) (if (gethash "resolved" c) (remhash "resolved" c) (puthash "resolved" t c)))))

(defun diff-review-toggle-threads ()
  "Draw every thread of the review open, or only the one at point again."
  (interactive)
  (let ((review (diff-review--in-review
                  (setq diff-review--threads-open (not diff-review--threads-open))
                  (current-buffer))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and diff-review-file-mode (eq diff-review--review review))
          (mapc #'diff-review--redraw-thread diff-review--threads))))))

(defun diff-review-delete-comment ()
  "Delete the comment thread at point, replies included."
  (interactive)
  (let ((id (gethash "id" (diff-review--thread-here))))
    (when (y-or-n-p "Delete this comment thread? ")
      (diff-review--in-review
        (diff-review--modify-comments
         (lambda ()
           (setq diff-review--comments
                 (vconcat (seq-remove (lambda (c) (equal (gethash "id" c) id))
                                      diff-review--comments)))))))))

(defun diff-review--file-with-comments (from step)
  "The index of the nearest file with comments from FROM, going STEP, or nil."
  (diff-review--in-review
    (let ((n (length diff-review--files)) (i (+ from step)) found)
      (while (and (not found) (< -1 i n))
        (when (diff-review--file-comments (plist-get (diff-review--file i) :path))
          (setq found i))
        (cl-incf i step))
      found)))

(defun diff-review-next-comment ()
  "Go to the next comment thread, continuing into later files."
  (interactive)
  (let ((line (line-number-at-pos)))
    (if-let* ((next (seq-find (lambda (th) (> (nth 1 th) line)) (diff-review--sorted-threads))))
        (diff-review--goto-line (nth 1 next))
      (if-let* ((i (diff-review--file-with-comments (diff-review--in-review diff-review--index) 1)))
          (progn (diff-review--in-review (diff-review--open i))
                 (with-current-buffer (window-buffer)
                   (when-let* ((th (car (diff-review--sorted-threads))))
                     (diff-review--goto-line (nth 1 th)))))
        (message "No comment after this one")))))

(defun diff-review-previous-comment ()
  "Go to the previous comment thread, continuing into earlier files."
  (interactive)
  (let ((line (line-number-at-pos)))
    (if-let* ((prev (seq-find (lambda (th) (< (nth 1 th) line))
                              (reverse (diff-review--sorted-threads)))))
        (diff-review--goto-line (nth 1 prev))
      (if-let* ((i (diff-review--file-with-comments (diff-review--in-review diff-review--index) -1)))
          (progn (diff-review--in-review (diff-review--open i))
                 (with-current-buffer (window-buffer)
                   (when-let* ((th (car (last (diff-review--sorted-threads)))))
                     (diff-review--goto-line (nth 1 th)))))
        (message "No comment before this one")))))

(defun diff-review--hunk-number ()
  "1-based index of the hunk at or before point, or nil."
  (let ((line (line-number-at-pos)) (n 0) found)
    (dolist (hunk diff-review--hunks)
      (cl-incf n)
      (when (<= (diff-review--hunk-line hunk) line) (setq found n)))
    found))

(defun diff-review--key-hints (hints)
  "HINTS, a list of (COMMAND . LABEL), as keys and dimmed labels.
Each key is the one the active maps give, as `substitute-command-keys'
finds it, so the hints follow the user's bindings.  A prefix every key
shares is shown once, in front."
  (let* ((keys (mapcar (lambda (hint) (where-is-internal (car hint) nil t)) hints))
         (prefixes (mapcar (lambda (key) (and (> (length key) 1) (substring key 0 -1)))
                           keys))
         (prefix (and (car prefixes)
                      (seq-every-p (lambda (p) (equal p (car prefixes))) prefixes)
                      (car prefixes)))
         (key-text (lambda (key)
                     (propertize (key-description key)
                                 'face 'help-key-binding 'font-lock-face 'help-key-binding))))
    (concat
     (if prefix (concat (funcall key-text prefix) "  ") "")
     (string-join
      (cl-mapcar (lambda (key hint)
                   (concat (cond ((null key) ; M-x cmd, as substitute-command-keys has it
                                  (substitute-command-keys (format "\\[%s]" (car hint))))
                                 (prefix (funcall key-text (substring key -1)))
                                 (t (funcall key-text key)))
                           (propertize (concat " " (cdr hint)) 'face 'shadow)))
                 keys hints)
      (propertize " · " 'face 'shadow)))))

(defun diff-review--file-header ()
  "The header line of a file: its place, status, path, counts, hunk and keys."
  (let* ((hunk (diff-review--hunk-number))
         (hunks (length diff-review--hunks))
         (focus diff-review--focus)
         (keys (if diff-review-file-header-keys
                   (concat "   " (diff-review--key-hints
                                  '((diff-review-next-hunk . "next hunk")
                                    (diff-review-viewed-and-next . "viewed")
                                    (diff-review-comment . "comment")
                                    (diff-review-toggle-focus . "focus")
                                    (diff-review-overview . "overview"))))
                 "")))
    (diff-review--in-review
      (let ((file (diff-review--file)))
        (concat
         (propertize (format " %d/%d " (1+ diff-review--index) (length diff-review--files))
                     'face 'shadow)
         (propertize (plist-get file :status)
                     'face (diff-review--status-face (plist-get file :status)))
         " "
         (propertize (plist-get file :path) 'face 'bold)
         (if-let* ((old (plist-get file :old)))
             (propertize (format "  (was %s)" old) 'face 'shadow)
           "")
         "   "
         (diff-review--counts (plist-get file :added) (plist-get file :removed))
         (propertize (format "   hunk %s/%d" (or hunk "–") hunks) 'face 'shadow)
         "   "
         (if (diff-review--viewed-p file)
             (propertize (concat (diff-review--glyph "✓" "*") " viewed") 'face 'success)
           (propertize (concat (diff-review--glyph "○" "o") " not viewed") 'face 'shadow))
         (propertize (if focus "   focus" "") 'face 'shadow)
         keys)))))

(defun diff-review--goto-hunk (hunk)
  "Move to where HUNK starts, near the top of the window."
  (goto-char (diff-review--line-pos (diff-review--hunk-line hunk)))
  (recenter diff-review-hunk-recenter))

(defun diff-review-next-hunk ()
  "Go to the next hunk, continuing into the next file."
  (interactive)
  (let* ((line (line-number-at-pos))
         (next (seq-find (lambda (h) (> (diff-review--hunk-line h) line))
                         diff-review--hunks)))
    (cond (next (diff-review--goto-hunk next))
          (t (diff-review--in-review
               (if (< diff-review--index (1- (length diff-review--files)))
                   (diff-review--open (1+ diff-review--index))
                 (message "Last hunk of the last file")))))))

(defun diff-review-previous-hunk ()
  "Go to the previous hunk, continuing into the previous file."
  (interactive)
  (let* ((line (line-number-at-pos))
         (prev (seq-find (lambda (h) (< (diff-review--hunk-line h) line))
                         (reverse diff-review--hunks))))
    (cond (prev (diff-review--goto-hunk prev))
          (t (diff-review--in-review
               (if (> diff-review--index 0)
                   (progn
                     (diff-review--open (1- diff-review--index))
                     (when diff-review--hunks
                       (diff-review--goto-hunk (car (last diff-review--hunks)))))
                 (message "First hunk of the first file")))))))

(defun diff-review-viewed-and-next ()
  "Mark this file viewed and show the next unviewed one."
  (interactive)
  (diff-review--in-review
    (diff-review--set-viewed (diff-review--file) t)
    (diff-review-next-unviewed)))

(defun diff-review-toggle-focus ()
  "Fold or unfold the unchanged lines of this file."
  (interactive)
  (setq diff-review--focus (not diff-review--focus))
  (diff-review--decorate))

(defun diff-review-toggle-inline-removed ()
  "Show or hide the removed lines, in this and later files of the review."
  (interactive)
  (diff-review--in-review
    (setq diff-review--inline-removed (not diff-review--inline-removed)))
  (diff-review--decorate))

(defun diff-review--next-error (n &optional reset)
  "Walk N hunks for `next-error', back when N is negative.
RESET starts from the first hunk of the file."
  ;; `next-error' calls this in the file it last walked from, which a walk
  ;; into the next file leaves behind: walk the file the review shows.
  (let ((review diff-review--review)
        (window (diff-review--main-window)))
    (if (with-current-buffer (window-buffer window)
          (and diff-review-file-mode (eq diff-review--review review)))
        (progn
          (select-window window)
          (when (and reset diff-review--hunks)
            (diff-review--goto-hunk (car diff-review--hunks))
            (setq n (max 0 (1- n))))
          (dotimes (_ (abs n))
            (with-current-buffer (window-buffer)   ; a step into another file shows it there
              (if (< n 0) (diff-review-previous-hunk) (diff-review-next-hunk)))))
      (diff-review--in-review (diff-review--open diff-review--index)))))

(defun diff-review--set (var value)
  "Set VAR to VALUE in this buffer until the review ends."
  (unless (assq var diff-review--saved)    ; on again for the next visit
    (push (cons var (cons (local-variable-p var) (symbol-value var))) diff-review--saved))
  (set (make-local-variable var) value))

(defvar-keymap diff-review-file-command-map
  :doc "The commands of `diff-review-file-mode', under `diff-review-command-prefix'."
  :repeat t
  "n" #'diff-review-next-hunk
  "p" #'diff-review-previous-hunk
  "v" #'diff-review-viewed-and-next
  "d" #'diff-review-toggle-inline-removed
  "z" #'diff-review-toggle-focus
  "c" #'diff-review-comment
  "r" #'diff-review-reply
  "x" #'diff-review-toggle-resolved
  "D" #'diff-review-delete-comment
  "o" #'diff-review-toggle-threads
  "N" #'diff-review-next-comment
  "P" #'diff-review-previous-comment
  "t" #'diff-review-toggle-sidebar
  "l" #'diff-review-overview
  "q" #'diff-review-quit)

(defvar diff-review-file-mode-map (make-sparse-keymap)
  "Keymap for `diff-review-file-mode'.
It holds `diff-review-file-command-map' under `diff-review-command-prefix'.")

;; A minor mode on the file's own buffer: C-c and a letter or a control
;; character belong to the user and the major mode, C-c and punctuation to
;; minor modes, so the commands sit under one such key.
(defcustom diff-review-command-prefix "C-c _"
  "Prefix key for the commands of `diff-review-file-mode'.
Nil binds no keys: bind `diff-review-file-command-map' or the commands
yourself."
  :type '(choice (const :tag "No keys" nil) key)
  :set (lambda (symbol value)
         (set-default symbol value)
         (setcdr diff-review-file-mode-map nil)
         (when value
           (keymap-set diff-review-file-mode-map value diff-review-file-command-map))))

(define-minor-mode diff-review-file-mode
  "Review decorations and keys in a file of a review.
\\<diff-review-file-mode-map>\\[diff-review-next-hunk] and \\[diff-review-previous-hunk] walk the
hunks, across files.  \\[diff-review-viewed-and-next] marks the file viewed and
moves on, \\[diff-review-toggle-focus] folds the unchanged lines,
\\[diff-review-toggle-inline-removed] hides the removed lines,
\\[diff-review-toggle-sidebar] shows or hides the sidebar,
\\[diff-review-overview] goes back to the overview and
\\[diff-review-quit] ends the review.
\\[diff-review-comment] comments on the region or the line, \\[diff-review-reply] replies to the
thread at point, \\[diff-review-toggle-resolved] resolves it, \\[diff-review-delete-comment] deletes it,
\\[diff-review-next-comment] and \\[diff-review-previous-comment] walk the threads, across files,
and \\[diff-review-toggle-threads] opens them all, or only the one at point again.
\\<global-map>\\[next-error] and \\[previous-error] walk the hunks too.  With `repeat-mode'
the prefix is needed once, for a run of these keys.

\\{diff-review-file-mode-map}"
  :lighter " Review"
  (if diff-review-file-mode
      (progn
        (setq diff-review--hunks
              (if diff-review--deleted
                  (list (list :line 1 :count 0 :removed nil))
                (diff-review--in-review (diff-review--parse-hunks (diff-review--file)))))
        (diff-review--decorate)
        (add-hook 'post-command-hook #'diff-review--track-thread nil t)
        (diff-review--set 'next-error-function #'diff-review--next-error)
        (when diff-review-file-header
          (diff-review--set 'header-line-format
                            `(:eval (funcall ',(if (functionp diff-review-file-header)
                                                   diff-review-file-header
                                                 #'diff-review--file-header))))))
    (diff-review--undecorate)
    (remove-hook 'post-command-hook #'diff-review--track-thread t)
    (pcase-dolist (`(,var ,local . ,value) diff-review--saved)
      (if local (set var value) (kill-local-variable var)))
    (kill-local-variable 'diff-review--saved)
    (setq diff-review--review nil)))

;;;; Evil

;; Normal state shadows n, p and g, so the lists get gj, gk and gr in their
;; place, as evil-collection does, and TAB, which motion state takes for
;; `evil-jump-forward'.  The rest mirror the Emacs keys.  A file under
;; review is being edited: its keys are the prefix, which evil leaves alone.

(defcustom diff-review-evil-keys t
  "Non-nil starts the lists in Evil's normal state, with keys for it.
Set it before Evil or this package loads; nil leaves Evil untouched."
  :type 'boolean)

(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key* "evil-core")

(with-eval-after-load 'evil
  (when diff-review-evil-keys
    (dolist (mode '(diff-review-mode diff-review-sidebar-mode))
      (evil-set-initial-state mode 'normal))
    (dolist (map (list diff-review-mode-map diff-review-sidebar-mode-map))
      (evil-define-key* 'normal map
        (kbd "RET") #'diff-review-open-at-point
        (kbd "TAB") #'magit-section-toggle
        "gj" #'diff-review-next-file
        "gk" #'diff-review-previous-file
        "gr" #'diff-review-refresh
        "u" #'diff-review-next-unviewed
        "v" #'diff-review-toggle-viewed
        "t" #'diff-review-toggle-sidebar
        "q" (if (eq map diff-review-sidebar-mode-map)
                #'diff-review-toggle-sidebar
              #'diff-review-quit)))))

;;;; Creating a review

;;;###autoload
(defun diff-review-find-file (file &optional root base head)
  "Visit review FILE, creating it first when it does not exist.
A new file reviews the repository at ROOT, by default the one holding
FILE, against BASE, by default `diff-review-default-base'.  It reviews
revision HEAD when given, else the worktree.  Where FILE lives is the
caller's choice."
  (interactive "FReview file: ")
  (unless (file-exists-p file)
    (make-directory (file-name-directory (expand-file-name file)) t)
    (let ((top (if root
                   (file-name-as-directory (expand-file-name root))
                 (diff-review--toplevel (file-name-directory (expand-file-name file))))))
      (setq base (or base (and top (diff-review--default-base top)))))
    (with-temp-file file
      (insert (json-serialize
               `((format . ,diff-review-format)
                 ,@(and root `((root . ,(file-name-as-directory (expand-file-name root)))))
                 ,@(and base `((base . ,base)))
                 ,@(and head `((head . ,head)))
                 (viewed . ,(make-hash-table)))))
      (json-pretty-print-buffer)
      (insert "\n")))
  (find-file file))

(provide 'diff-review)
;;; diff-review.el ends here
