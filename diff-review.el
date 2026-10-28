;;; diff-review.el --- Review a git diff from a review file -*- lexical-binding: t; -*-

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
;; A sidebar shows the same tree beside the main window, as a side
;; window.  Files are kept in the tree's order, directories first, so
;; walking them follows the tree.  It follows the review: it shows while
;; the selected window shows the overview or one of its files, hides when
;; you switch to anything else, and `diff-review-toggle-sidebar' turns it
;; off for a review altogether.
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
;;    "base": "main",                  optional: the default branch
;;    "head": "abc1234",               optional: the worktree
;;    "viewed": {"src/a.ts": "<blob hash>"}}
;;
;; The review buffer is an ordinary buffer: hide it, show it, kill it to
;; end the review.  Files keep a pointer back to it.

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

(defcustom diff-review-context 3
  "Unchanged lines kept around each hunk in focus mode."
  :type 'integer)

(defcustom diff-review-inline-removed t
  "Whether to show removed lines inline, above where they were."
  :type 'boolean)

(defcustom diff-review-sidebar t
  "Whether a review starts with its sidebar shown."
  :type 'boolean)

(defcustom diff-review-sidebar-width 36
  "Width of the sidebar, in columns."
  :type 'integer)

(defcustom diff-review-sidebar-side 'left
  "Which side of the frame the sidebar takes."
  :type '(choice (const left) (const right)))

(defface diff-review-added '((t :extend t))
  "Face for added lines, over the syntax faces.
Without a background of its own, the background of `diff-added' is used.")
(defface diff-review-removed '((t :extend t))
  "Face for removed lines, shown inline, over the syntax faces.
Without a background of its own, the background of `diff-removed' is used.")

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
(defvar-local diff-review--main-window nil "The window the overview and its files show in.")
(defvar-local diff-review--sidebar nil "The sidebar buffer, once created.")
(defvar-local diff-review--sidebar-enabled nil "Whether this review shows its sidebar.")

;; In a file buffer or the sidebar.
(defvar-local diff-review--review nil "The review buffer this buffer belongs to.")
(defvar-local diff-review--hunks nil
  "In a file buffer, its hunks: plists with :line, :count and :removed.")
(defvar-local diff-review--overlays nil "In a file buffer, its review overlays.")
(defvar-local diff-review--focus nil "In a file buffer, whether unchanged lines are folded.")
(defvar-local diff-review--deleted nil "In a buffer showing a deleted file's base version, t.")
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
        (error "git %s: %s" (string-join args " ") (string-trim (buffer-string))))
      (string-trim-right (buffer-string) "\n+"))))

(defun diff-review--toplevel (directory)
  "The root of the repository holding DIRECTORY, or nil."
  (let ((default-directory directory))
    (with-temp-buffer
      (and (zerop (process-file "git" nil t nil "rev-parse" "--show-toplevel"))
           (file-name-as-directory (string-trim (buffer-string)))))))

(defun diff-review--default-base ()
  "The default branch: what origin/HEAD points at, else main."
  (let ((ref (ignore-errors
               (diff-review--git "symbolic-ref" "--short" "refs/remotes/origin/HEAD"))))
    (if (and ref (string-prefix-p "origin/" ref)) (substring ref 7) "main")))

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

(defun diff-review--hashes (paths)
  "Return the blob hashes of the files at PATHS, at the head, in one git call."
  (cond
   ((null paths) nil)
   (diff-review--head
    (split-string (apply #'diff-review--git "rev-parse"
                         (mapcar (lambda (p) (concat diff-review--head ":" p)) paths))
                  "\n" t))
   (t
    (let ((default-directory diff-review--root))
      (with-temp-buffer
        (insert (string-join paths "\n") "\n")
        (unless (zerop (call-process-region (point-min) (point-max) "git" t t nil
                                            "hash-object" "--stdin-paths"))
          (error "git hash-object: %s" (string-trim (buffer-string))))
        (split-string (buffer-string) "\n" t))))))

(defun diff-review--scan ()
  "Fill the file vector from git: four calls for any number of files."
  (setq diff-review--head-name (or diff-review--head
                                   (diff-review--git "rev-parse" "--abbrev-ref" "HEAD"))
        diff-review--base (diff-review--git "merge-base" diff-review--base-name
                                            (or diff-review--head "HEAD")))
  (let* ((entries (sort (diff-review--parse-name-status
                         (apply #'diff-review--git "diff" "--name-status" "-M" "-z"
                                (diff-review--range)))
                        (lambda (a b) (diff-review--tree-less-p (car a) (car b)))))
         (numstat (diff-review--parse-numstat
                   (apply #'diff-review--git "diff" "--numstat" "-M" "-z"
                          (diff-review--range))))
         (present (seq-remove (lambda (e) (equal (nth 1 e) "D")) entries))
         (hashes (cl-mapcar #'cons (mapcar #'car present)
                            (diff-review--hashes (mapcar #'car present)))))
    (setq diff-review--files
          (vconcat
           (mapcar (lambda (entry)
                     (pcase-let ((`(,path ,status ,old) entry))
                       (let ((stat (gethash path numstat)))
                         (list :path path :status status :old old
                               :added (or (car stat) 0) :removed (or (cdr stat) 0)
                               :hash (or (cdr (assoc path hashes)) "deleted")))))
                   entries)))))

(defun diff-review--file (&optional index)
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
    (puthash "base" (or (gethash "base" data) (diff-review--default-base)) data)
    (unless (hash-table-p (gethash "viewed" data))
      (puthash "viewed" (make-hash-table :test #'equal) data))
    (setq diff-review--data data
          diff-review--base-name (gethash "base" data)
          diff-review--head (gethash "head" data)
          diff-review--viewed (gethash "viewed" data))))

(defun diff-review--write ()
  "Write the buffer's state to its file, leaving the buffer unmodified.
Returns t, as `write-contents-functions' expects."
  (let ((file buffer-file-name)
        (json (json-serialize diff-review--data)))   ; here: the temp buffer has no data
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
  (unless (verify-visited-file-modtime)   ; someone else wrote it: take theirs first
    (diff-review--revert))
  (if flag
      (puthash (plist-get file :path) (plist-get file :hash) diff-review--viewed)
    (remhash (plist-get file :path) diff-review--viewed))
  (diff-review--write))

;;;; Rendering

(defun diff-review--status-face (status)
  (pcase status
    ("A" '(bold success))
    ("D" '(bold error))
    (_ '(bold warning))))

(defun diff-review--counts (added removed)
  "+ADDED and −REMOVED, in the diff indicator faces."
  (concat (propertize (format "+%d" added) 'face 'diff-indicator-added) " "
          (propertize (format "−%d" removed) 'face 'diff-indicator-removed)))

(defun diff-review--sum (files key)
  (apply #'+ (mapcar (lambda (f) (plist-get f key)) files)))

(defun diff-review--summary ()
  "The viewed count and the line counts of the review, in one string."
  (format "%d/%d viewed  %s"
          (seq-count #'diff-review--viewed-p diff-review--files)
          (length diff-review--files)
          (diff-review--counts (diff-review--sum diff-review--files :added)
                               (diff-review--sum diff-review--files :removed))))

(defun diff-review--stats (files &optional mark)
  "The added and removed counts of FILES, after MARK, against the right edge."
  (let ((stats (concat mark (diff-review--counts (diff-review--sum files :added)
                                                 (diff-review--sum files :removed)))))
    (concat (propertize " " 'display `(space :align-to (- right ,(1+ (string-width stats)))))
            stats)))

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
                        (diff-review--stats (list file) (and viewed (propertize "✓ " 'face 'success)))
                        "\n")
                'diff-review-index i
                'help-echo (if (plist-get file :old)
                               (format "%s → %s" (plist-get file :old) path)
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

(defun diff-review--goto-current ()
  "Put point on the current file's line, or the first file."
  (goto-char (point-min))
  (let ((pos (or (and diff-review--index
                      (text-property-any (point-min) (point-max)
                                         'diff-review-index diff-review--index))
                 (next-single-property-change (point-min) 'diff-review-index))))
    (when pos (goto-char pos)))
  (when-let* ((window (get-buffer-window (current-buffer))))
    (set-window-point window (point))))

(defun diff-review--render ()
  "Redraw the overview.  The review buffer must be current."
  (let ((inhibit-read-only t)
        (n (length diff-review--files)))
    (erase-buffer)
    (magit-insert-section (diff-review-root)
      (insert (propertize (format "%s ← %s" diff-review--head-name diff-review--base-name)
                          'face '(bold (:height 1.2)))
              "\n\n"
              (format "%d file%s changed, " n (if (= n 1) "" "s"))
              (diff-review--summary) "\n"
              (propertize (format "%s\nmerge-base %s\n%s\n"
                                  (abbreviate-file-name diff-review--root)
                                  (substring diff-review--base 0 10)
                                  (abbreviate-file-name buffer-file-name))
                          'face 'shadow)
              "\n")
      (diff-review--insert-files))
    (set-buffer-modified-p nil)
    (diff-review--goto-current)))

(defun diff-review--render-all ()
  "Redraw the overview and the sidebar.  The review buffer must be current."
  (diff-review--render)
  (when (buffer-live-p diff-review--sidebar)
    (with-current-buffer diff-review--sidebar (diff-review-sidebar--render)))
  (force-mode-line-update t))

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
  (diff-review--render-all))

(defun diff-review--revert (&rest _)
  "Reload the review from its file, as `revert-buffer-function'."
  (let ((inhibit-read-only t))
    (set-visited-file-modtime)          ; the disk copy is what we want: no supersession prompt
    (insert-file-contents buffer-file-name nil nil nil t))
  (diff-review--load))

;;;; Commands on a list, in the overview or the sidebar

(defun diff-review--index-at-point ()
  (get-text-property (line-beginning-position) 'diff-review-index))

(defun diff-review-open-at-point ()
  "Show the file on this line; on a directory, fold or unfold it."
  (interactive)
  (if-let* ((i (diff-review--index-at-point)))
      (diff-review--in-review (diff-review--open i))
    (magit-section-toggle (magit-current-section))))

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
  (let ((review (or (diff-review--review-buffer) (user-error "Not in a review"))))
    (diff-review--show-in-main review review)))

(defun diff-review-toggle-sidebar ()
  "Show or hide this review's sidebar."
  (interactive)
  (diff-review--in-review
    (setq diff-review--sidebar-enabled (not diff-review--sidebar-enabled))
    (if diff-review--sidebar-enabled
        (diff-review--show-sidebar (current-buffer))
      (diff-review--hide-sidebar (selected-frame)))))

(defvar-keymap diff-review-mode-map
  :doc "Keymap for `diff-review-mode', shared with the sidebar."
  :parent magit-section-mode-map
  "RET" #'diff-review-open-at-point
  "<mouse-1>" #'diff-review-mouse-open
  "n" #'diff-review-next-file
  "p" #'diff-review-previous-file
  "u" #'diff-review-next-unviewed
  "v" #'diff-review-toggle-viewed
  "g" #'diff-review-refresh
  "t" #'diff-review-toggle-sidebar)

;;;###autoload
(define-derived-mode diff-review-mode magit-section-mode "Review"
  "Major mode for a review file: an overview of what a worktree changed.
\\<diff-review-mode-map>\\[diff-review-open-at-point] shows the file at point in this window,
or folds a directory.  \\[diff-review-next-file] and \\[diff-review-previous-file] walk the files;
\\[diff-review-next-unviewed] skips to the next unviewed one.
\\[diff-review-toggle-viewed] toggles the viewed mark, \\[diff-review-refresh] rescans
and \\[diff-review-toggle-sidebar] shows or hides the sidebar.
\\[quit-window] hides the review; killing the buffer ends it.

\\{diff-review-mode-map}"
  (setq revert-buffer-function #'diff-review--revert
        diff-review--sidebar-enabled diff-review-sidebar)
  (add-hook 'write-contents-functions #'diff-review--write nil t)
  (add-hook 'kill-buffer-hook #'diff-review--close nil t)
  (display-line-numbers-mode -1)
  (hl-line-mode 1)
  (auto-revert-mode 1)
  (diff-review--load))

;;;###autoload
(add-to-list 'magic-mode-alist
             (cons "\\`\\s-*{\\s-*\"format\"\\s-*:\\s-*\"diff-review/" #'diff-review-mode))

(defun diff-review--close ()
  "End the review being killed: its files' decorations and its sidebar."
  (let ((review (current-buffer)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and diff-review-file-mode (eq diff-review--review review))
          (diff-review-file-mode -1))))
    (when (buffer-live-p diff-review--sidebar)
      (kill-buffer diff-review--sidebar))))

;;;; The sidebar

(defvar-keymap diff-review-sidebar-mode-map
  :doc "Keymap for `diff-review-sidebar-mode'."
  :parent diff-review-mode-map
  "q" #'diff-review-toggle-sidebar)

(define-derived-mode diff-review-sidebar-mode magit-section-mode "Review-Sidebar"
  "Major mode for a review's sidebar: its changed files as a tree.
The sidebar shows beside the main window.
The keys are those of `diff-review-mode';
\\<diff-review-sidebar-mode-map>\\[diff-review-toggle-sidebar] hides the sidebar.

\\{diff-review-sidebar-mode-map}"
  (setq cursor-type nil
        window-size-fixed 'width
        mode-line-format nil
        header-line-format '(:eval (diff-review-sidebar--header)))
  (display-line-numbers-mode -1)
  (hl-line-mode 1))

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
        (dolist (var '(diff-review--files diff-review--index diff-review--viewed))
          (set (make-local-variable var) (buffer-local-value var review)))
        (magit-insert-section (diff-review-root)
          (diff-review--insert-files))
        (diff-review--goto-current)
        (when (and moved diff-review--index)   ; show the new file, else keep the folds
          (magit-section-reveal (magit-current-section)))))))

(defun diff-review--sidebar-window (frame)
  "The window of FRAME showing a review sidebar, or nil."
  (seq-find (lambda (w) (with-current-buffer (window-buffer w)
                          (derived-mode-p 'diff-review-sidebar-mode)))
            (window-list frame 'no-minibuffer)))

(defun diff-review--show-sidebar (review)
  "Show REVIEW's sidebar in the selected frame, replacing another review's."
  (let ((sidebar (diff-review--sidebar-buffer review))
        (window (diff-review--sidebar-window (selected-frame))))
    (unless (and window (eq (window-buffer window) sidebar))
      (when window (delete-window window))
      (display-buffer sidebar
                      `((display-buffer-in-side-window)
                        (side . ,diff-review-sidebar-side)
                        (slot . 0)
                        (window-width . ,diff-review-sidebar-width)
                        (dedicated . t)
                        (window-parameters . ((no-delete-other-windows . t)))))
      (with-current-buffer sidebar (diff-review-sidebar--render)))))

(defun diff-review--hide-sidebar (frame)
  "Remove the review sidebar from FRAME, if any."
  (when-let* ((window (diff-review--sidebar-window frame)))
    (delete-window window)))

(defun diff-review--follow (&optional frame)
  "Show the selected window's review sidebar, or hide the sidebar.
Runs after a window changes buffer or selection; side windows and the
minibuffer are ignored, so popups do not disturb the sidebar."
  (let* ((frame (if (framep frame) frame (selected-frame)))
         (window (frame-selected-window frame)))
    (unless (or (window-parameter window 'window-side)
                (window-minibuffer-p window))
      (let ((review (with-current-buffer (window-buffer window)
                      (diff-review--review-buffer))))
        (if (and review (buffer-local-value 'diff-review--sidebar-enabled review))
            (with-selected-frame frame
              (with-current-buffer review (setq diff-review--main-window window))
              (diff-review--show-sidebar review))
          (diff-review--hide-sidebar frame))))))

(add-hook 'window-selection-change-functions #'diff-review--follow)
(add-hook 'window-buffer-change-functions #'diff-review--follow)

;;;; Showing a file

(defun diff-review--main-window (review)
  "REVIEW's main window.
The one it last showed in, else the selected window, else any usable one."
  (with-current-buffer review
    (cond ((and (window-live-p diff-review--main-window)
                (not (window-parameter diff-review--main-window 'window-side)))
           diff-review--main-window)
          ((not (or (window-parameter (selected-window) 'window-side)
                    (window-minibuffer-p (selected-window))))
           (selected-window))
          (t (or (get-mru-window nil nil t)
                 (seq-find (lambda (w) (not (window-parameter w 'window-side)))
                           (window-list nil 'no-minibuffer)))))))

(defun diff-review--show-in-main (review buffer)
  "Show BUFFER in REVIEW's main window and select it."
  (let ((window (diff-review--main-window review)))
    (set-window-dedicated-p window nil)
    (set-window-buffer window buffer)
    (select-window window)
    (with-current-buffer review (setq diff-review--main-window window))
    window))

(defun diff-review--blob-buffer (rev path &optional deleted)
  "A read-only buffer with PATH as it is at REV; DELETED when the file is gone."
  (let ((buffer (get-buffer-create (format "*%s: %s*" (if deleted "deleted" rev) path)))
        (contents (diff-review--git "show" (concat rev ":" path)))
        (root diff-review--root))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert contents)
        (let ((buffer-file-name (expand-file-name path root)))
          (set-auto-mode))
        (setq diff-review--deleted deleted)
        (read-only-mode 1)))
    buffer))

(defun diff-review--open (i)
  "Show file I in the main window, at its first hunk, and sync the lists.
The review buffer must be current."
  (setq diff-review--index i)
  (let* ((review (current-buffer))
         (file (diff-review--file i))
         (path (plist-get file :path))
         (buffer (cond ((equal (plist-get file :status) "D")
                        (diff-review--blob-buffer diff-review--base path t))
                       (diff-review--head (diff-review--blob-buffer diff-review--head path))
                       (t (find-file-noselect (expand-file-name path diff-review--root))))))
    (with-current-buffer buffer
      (setq diff-review--review review)
      (diff-review-file-mode 1))
    (diff-review--render-all)
    (diff-review--show-in-main review buffer)
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
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun diff-review--hunk-line (hunk)
  "The buffer line HUNK starts on; a pure deletion sits before the next line."
  (if (zerop (plist-get hunk :count))
      (1+ (plist-get hunk :line))
    (plist-get hunk :line)))

(define-fringe-bitmap 'diff-review-bar [224] nil nil '(center repeated))

(defun diff-review--tint (face source)
  "FACE as an overlay face, with SOURCE's background when FACE sets none."
  (let ((own (face-attribute face :background nil t))
        (borrowed (face-attribute source :background nil t)))
    (if (or (stringp own) (not (stringp borrowed)))
        face
      (list face `(:background ,borrowed :extend t)))))

(defun diff-review--fringe-mark (face)
  "A string that draws a bar in the left fringe of its line, in FACE."
  (if (display-graphic-p)
      (propertize " " 'display `(left-fringe diff-review-bar ,face))
    ""))

(defun diff-review--fontify (text mode)
  "TEXT with the syntax faces MODE would give it, or TEXT itself."
  (condition-case nil
      (with-temp-buffer
        (insert text)
        (delay-mode-hooks (funcall mode))
        (font-lock-ensure)
        (buffer-string))
    (error text)))

(defun diff-review--removed-string (lines mode)
  "The inline rendering of removed LINES: fontified as MODE, tinted, fringe-marked."
  (let ((removed (diff-review--tint 'diff-review-removed 'diff-removed))
        (mark (diff-review--fringe-mark 'diff-indicator-removed)))
    (mapconcat (lambda (line)
                 (let ((text (if (eq mode 'fundamental-mode) line
                               (diff-review--fontify line mode))))
                   (add-face-text-property 0 (length text) removed t text)
                   (concat mark text (propertize "\n" 'face removed))))
               lines)))

(defun diff-review--decorate ()
  "Add the inline removed lines, the added tint, the fringe bars and the folds."
  (diff-review--undecorate)
  (let ((mode major-mode))
    (if diff-review--deleted
        (let ((overlay (make-overlay (point-min) (point-max))))
          (overlay-put overlay 'face (diff-review--tint 'diff-review-removed 'diff-removed))
          (push overlay diff-review--overlays))
      (dolist (hunk diff-review--hunks)
        (let* ((line (diff-review--hunk-line hunk))
               (start (diff-review--line-pos line))
               (count (plist-get hunk :count))
               (end (if (zerop count) start
                      (diff-review--line-pos (+ (plist-get hunk :line) count))))
               (overlay (make-overlay start end nil t nil)))
          (when (> count 0)
            (overlay-put overlay 'face (diff-review--tint 'diff-review-added 'diff-added))
            (overlay-put overlay 'line-prefix (diff-review--fringe-mark 'diff-indicator-added)))
          (when (and diff-review-inline-removed (plist-get hunk :removed))
            (overlay-put overlay 'before-string
                         (diff-review--removed-string (plist-get hunk :removed) mode)))
          (push overlay diff-review--overlays)))
      (when diff-review--focus (diff-review--fold)))))

(defun diff-review--fold ()
  "Hide unchanged lines beyond `diff-review-context' of any hunk."
  (let ((last (line-number-at-pos (point-max)))
        keep)
    (dolist (hunk diff-review--hunks)
      (push (cons (max 1 (- (diff-review--hunk-line hunk) diff-review-context))
                  (min last (+ (plist-get hunk :line)
                               (max 0 (1- (plist-get hunk :count)))
                               diff-review-context)))
            keep))
    (setq keep (sort keep (lambda (a b) (< (car a) (car b)))))
    (let ((line 1))
      (dolist (range (append keep (list (cons (1+ last) (1+ last)))))
        (when (< line (car range))
          (let ((overlay (make-overlay (diff-review--line-pos line)
                                       (diff-review--line-pos (car range)))))
            (overlay-put overlay 'display
                         (propertize (format "   ⋯ %d unchanged lines ⋯\n" (- (car range) line))
                                     'face 'shadow))
            (push overlay diff-review--overlays)))
        (setq line (max line (1+ (cdr range))))))))

(defun diff-review--undecorate ()
  (mapc #'delete-overlay diff-review--overlays)
  (setq diff-review--overlays nil))

(defun diff-review--hunk-number ()
  "1-based index of the hunk at or before point, or nil."
  (let ((line (line-number-at-pos)) (n 0) found)
    (dolist (hunk diff-review--hunks)
      (cl-incf n)
      (when (<= (diff-review--hunk-line hunk) line) (setq found n)))
    found))

(defun diff-review--file-header ()
  (let* ((hunk (diff-review--hunk-number))
         (hunks (length diff-review--hunks))
         (focus diff-review--focus)
         (keys (substitute-command-keys   ; in this buffer, where the file keymap is active
                "   \\[diff-review-next-hunk] next hunk · \\[diff-review-viewed-and-next] viewed · \\[diff-review-toggle-focus] focus · \\[diff-review-overview] overview")))
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
             (propertize "✓ viewed" 'face 'success)
           (propertize "○ not viewed" 'face 'shadow))
         (propertize (if focus "   focus" "") 'face 'shadow)
         (propertize keys 'face 'shadow))))))

(defun diff-review--goto-hunk (hunk)
  (goto-char (diff-review--line-pos (diff-review--hunk-line hunk)))
  (recenter 6))

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
  "Show or hide the removed lines, in this and later files."
  (interactive)
  (setq diff-review-inline-removed (not diff-review-inline-removed))
  (diff-review--decorate))

(defvar-keymap diff-review-file-mode-map
  :doc "Keymap for `diff-review-file-mode'."
  "C-c C-n" #'diff-review-next-hunk
  "C-c C-p" #'diff-review-previous-hunk
  "C-c C-v" #'diff-review-viewed-and-next
  "C-c C-d" #'diff-review-toggle-inline-removed
  "C-c C-z" #'diff-review-toggle-focus
  "C-c C-t" #'diff-review-toggle-sidebar
  "C-c C-l" #'diff-review-overview)

(define-minor-mode diff-review-file-mode
  "Review decorations and keys in a file of a review.
\\<diff-review-file-mode-map>\\[diff-review-next-hunk] and \\[diff-review-previous-hunk] walk the
hunks, across files.  \\[diff-review-viewed-and-next] marks the file viewed and
moves on, \\[diff-review-toggle-focus] folds the unchanged lines,
\\[diff-review-toggle-inline-removed] hides the removed lines,
\\[diff-review-toggle-sidebar] shows or hides the sidebar and
\\[diff-review-overview] goes back to the overview."
  :lighter " Review"
  (if diff-review-file-mode
      (progn
        (setq diff-review--hunks
              (if diff-review--deleted
                  (list (list :line 1 :count 0 :removed nil))
                (diff-review--in-review (diff-review--parse-hunks (diff-review--file)))))
        (diff-review--decorate)
        (setq header-line-format '(:eval (diff-review--file-header))))
    (diff-review--undecorate)
    (setq header-line-format nil
          diff-review--review nil)))

;;;; Evil

;; Keys in the style of evil-collection, whose magit-section keys fold the
;; tree.  The lists start in normal state, so j and k walk the lines and the
;; leader works; gj and gk, C-j and C-k, ]] and [[ show the next and previous
;; file.  A file is being edited, so its keys
;; stay off the editing ones: ]c and [c walk the hunks, ]] and [[ the files,
;; zv marks viewed and moves on, zf folds, zd hides the removed lines, gs
;; toggles the sidebar and gl goes back to the overview.

(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key* "evil-core")
(declare-function evil-define-minor-mode-key "evil-core")

(with-eval-after-load 'evil
  (dolist (mode '(diff-review-mode diff-review-sidebar-mode))
    (evil-set-initial-state mode 'normal))
  (dolist (map (list diff-review-mode-map diff-review-sidebar-mode-map))
    (evil-define-key* 'normal map
      (kbd "RET") #'diff-review-open-at-point
      "o" #'diff-review-open-at-point
      "gj" #'diff-review-next-file
      "gk" #'diff-review-previous-file
      (kbd "C-j") #'diff-review-next-file
      (kbd "C-k") #'diff-review-previous-file
      "]]" #'diff-review-next-file
      "[[" #'diff-review-previous-file
      "v" #'diff-review-toggle-viewed
      "u" #'diff-review-next-unviewed
      "t" #'diff-review-toggle-sidebar
      "gr" #'diff-review-refresh
      "q" (if (eq map diff-review-sidebar-mode-map)
              #'diff-review-toggle-sidebar
            #'quit-window)))
  (evil-define-minor-mode-key 'normal 'diff-review-file-mode
    "]c" #'diff-review-next-hunk
    "[c" #'diff-review-previous-hunk
    "]]" #'diff-review-next-file
    "[[" #'diff-review-previous-file
    "zv" #'diff-review-viewed-and-next
    "zf" #'diff-review-toggle-focus
    "zd" #'diff-review-toggle-inline-removed
    "gs" #'diff-review-toggle-sidebar
    "gl" #'diff-review-overview))

;;;; Creating a review

;;;###autoload
(defun diff-review-find-file (file &optional root base head)
  "Visit review FILE, creating it first when it does not exist.
A new file reviews the repository at ROOT, by default the one holding
FILE, against BASE, by default the default branch.  It reviews revision
HEAD when given, else the worktree.  Where FILE lives is the caller's
choice."
  (interactive "FReview file: ")
  (unless (file-exists-p file)
    (make-directory (file-name-directory (expand-file-name file)) t)
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
