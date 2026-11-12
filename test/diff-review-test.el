;;; diff-review-test.el --- Tests for diff-review  -*- lexical-binding: t; -*-

;;; Commentary:

;; Each test builds a git repository in a temp folder, with a commit and a
;; dirty worktree, and drives windows in the batch frame.  Batch Emacs never
;; redisplays, so `diff-review--follow', which the redisplay hook runs,
;; is called where a window change would have triggered it.

;;; Code:

(require 'ert)
(require 'diff-review)
(require 'hl-line)

(defun diff-review-test--git (&rest args)
  (with-temp-buffer
    (unless (zerop (apply #'call-process "git" nil t nil
                          "-c" "user.name=t" "-c" "user.email=t@t" args))
      (error "git %s failed: %s" (string-join args " ") (string-trim (buffer-string))))))

(defun diff-review-test--write (path text)
  (when-let* ((dir (file-name-directory path)))
    (make-directory dir t))
  (with-temp-file path (insert text)))

(defmacro diff-review-test--with-repo (&rest body)
  "Run BODY with `repo' bound to a fresh repository, cleaned up after.
The repository has two commits and a dirty worktree touching two files."
  (declare (indent 0))
  `(let* ((repo (file-name-as-directory (make-temp-file "diff-review-test" t)))
          (default-directory repo)
          (diff-review-sidebar-width 20))
     (unwind-protect
         (progn
           (diff-review-test--git "init" "-q" "-b" "main")
           (diff-review-test--write "a.txt" "a\nb\nc\n")
           (diff-review-test--write "src/b.txt" "x\n")
           (diff-review-test--git "add" ".")
           (diff-review-test--git "commit" "-q" "-m" "one")
           (diff-review-test--write "a.txt" "a\nB\nc\nd\n")
           (diff-review-test--write "src/b.txt" "y\n")
           (diff-review-test--git "commit" "-q" "-am" "two")
           (diff-review-test--write "a.txt" "a\nB\nc\nd\ne\n")
           (diff-review-test--write "src/b.txt" "z\n")
           ,@body)
       (dolist (buffer (buffer-list))
         (when (or (buffer-local-value 'diff-review--review buffer)
                   (derived-mode-p 'diff-review-mode)
                   (and (buffer-file-name buffer)   ; the repo's files, and its worktrees'
                        (string-prefix-p (directory-file-name (file-truename repo))
                                         (file-truename (buffer-file-name buffer)))))
           (kill-buffer buffer)))
       (delete-other-windows)
       (switch-to-buffer "*scratch*")
       (delete-directory repo t)
       (dolist (dir (file-expand-wildcards (concat (directory-file-name repo) "-*") t))
         (delete-directory dir t)))))

(defun diff-review-test--worktree (repo name &rest args)
  "Add a worktree of REPO beside it, NAME in its folder's name, with ARGS; its path."
  (let ((dir (file-name-as-directory (concat (directory-file-name repo) "-" name))))
    (apply #'diff-review-test--git "worktree" "add" "-q" dir args)
    dir))

(defun diff-review-test--commit-in (dir path text message)
  "Write TEXT to PATH in worktree DIR and commit it there as MESSAGE."
  (let ((default-directory dir))
    (diff-review-test--write path text)
    (diff-review-test--git "add" path)
    (diff-review-test--git "commit" "-q" "-m" message)))

(defun diff-review-test--same-file-p (a b)
  "Whether paths A and B name one file, through symlinks."
  (equal (directory-file-name (file-truename a)) (directory-file-name (file-truename b))))

(defun diff-review-test--open (repo &optional name base head)
  "Visit review NAME in REPO in the selected window, and settle the sidebar."
  (diff-review-find-file (expand-file-name (or name "review.json") repo) repo
                         (or base "main") head)
  (diff-review--follow)
  (current-buffer))

(defun diff-review-test--sidebar (window)
  "The sidebar window of WINDOW, when it shows that review's sidebar buffer."
  (let ((sidebar (diff-review--sidebar-window window)))
    (and sidebar
         (eq (window-buffer sidebar)
             (buffer-local-value 'diff-review--sidebar
                                 (with-current-buffer (window-buffer window)
                                   (diff-review--review-buffer))))
         sidebar)))

;;;; The sidebar is the review window's atomic partner

(ert-deftest diff-review-test-sidebar-inside-the-cell ()
  "In a 2x2 layout the sidebar is carved out of the review's own cell."
  (diff-review-test--with-repo
    (delete-other-windows)
    (split-window-right) (split-window-below) (other-window 2) (split-window-below) (other-window 1)
    (diff-review-test--open repo)
    (let* ((main (selected-window)) (sidebar (diff-review-test--sidebar main)))
      (should (eq major-mode 'diff-review-mode))
      (should sidebar)
      (should (eq (window-next-sibling sidebar) main))
      (should (eq (window-atom-root main) (window-parent main)))
      (should (= (window-total-width sidebar) 20))
      (should (= (length (window-list)) 5)))))

(ert-deftest diff-review-test-walking-keeps-the-sidebar ()
  "Files show in the review's window, from the overview and from the sidebar."
  (diff-review-test--with-repo
    (delete-other-windows)
    (diff-review-test--open repo)
    (let* ((main (selected-window)) (sidebar (diff-review-test--sidebar main)))
      (diff-review-next-file)
      (diff-review--follow)
      (should (eq (selected-window) main))
      (should (equal (buffer-name (window-buffer main)) "b.txt"))   ; src/ first
      (should (eq (diff-review-test--sidebar main) sidebar))
      (select-window sidebar)
      (diff-review-next-file)
      (diff-review--follow)
      (should (eq (selected-window) main))
      (should (equal (buffer-name (window-buffer main)) "a.txt"))
      (should (eq (diff-review-test--sidebar main) sidebar))
      (diff-review-overview)
      (should (eq (selected-window) main))
      (should (eq (buffer-local-value 'major-mode (window-buffer main)) 'diff-review-mode)))))

(ert-deftest diff-review-test-toggle-from-the-sidebar ()
  (diff-review-test--with-repo
    (delete-other-windows)
    (diff-review-test--open repo)
    (let* ((main (selected-window)) (sidebar (diff-review-test--sidebar main)))
      (select-window sidebar)
      (diff-review-toggle-sidebar)
      (should-not (window-live-p sidebar))
      (should (eq (selected-window) main))
      (should-not (window-parameter main 'window-atom))
      (should (= (length (window-list)) 1))
      (diff-review-toggle-sidebar)
      (should (diff-review-test--sidebar main)))))

(ert-deftest diff-review-test-window-commands-treat-the-pair-as-one ()
  (diff-review-test--with-repo
    (delete-other-windows)
    (split-window-right) (other-window 1)
    (diff-review-test--open repo)
    (let ((main (selected-window)))
      (delete-other-windows)
      (should (= (length (window-list)) 2))
      (should (diff-review-test--sidebar main))
      (switch-to-buffer "*scratch*")
      (diff-review--follow)
      (should (= (length (window-list)) 1))
      (should-not (window-parameter main 'window-atom))
      (split-window-right) (other-window 1)
      (diff-review-test--open repo)
      (should (diff-review-test--sidebar (selected-window)))
      (delete-window)
      (should (= (length (window-list)) 1)))))

(ert-deftest diff-review-test-sidebar-resizes-and-keeps-its-width ()
  "The user resizes the sidebar; balancing and a hide and show keep the width."
  (diff-review-test--with-repo
    (delete-other-windows)
    (diff-review-test--open repo)
    (let* ((main (selected-window))
           (review (window-buffer main))
           (sidebar (diff-review-test--sidebar main)))
      (should-not (buffer-local-value 'window-size-fixed (window-buffer sidebar)))
      (should (window-preserved-size sidebar t))
      (should (= (window-total-width sidebar) 20))
      (adjust-window-trailing-edge sidebar 6 t)       ; a drag on its edge
      (should (= (window-total-width sidebar) 26))
      (diff-review-sidebar--resized sidebar)          ; what redisplay would run
      (should (= (buffer-local-value 'diff-review--sidebar-width review) 26))
      (balance-windows)
      (should (= (window-total-width sidebar) 26))
      (with-selected-window sidebar (enlarge-window 4 t))   ; C-x }
      (should (= (window-total-width sidebar) 30))
      (diff-review-sidebar--resized sidebar)
      (balance-windows)
      (should (= (window-total-width sidebar) 30))
      (diff-review-toggle-sidebar)
      (diff-review-toggle-sidebar)
      (let ((again (diff-review-test--sidebar main)))
        (should (= (window-total-width again) 30))
        (should (window-preserved-size again t))))))

;;;; Ending a review gives the window back

(ert-deftest diff-review-test-kill-shows-what-the-window-showed-before ()
  (diff-review-test--with-repo
    (delete-other-windows)
    (split-window-right) (other-window 1)
    (let ((before (get-buffer-create "before")) (w (selected-window)))
      (switch-to-buffer before)
      (diff-review-test--open repo)
      (diff-review-next-file) (diff-review-next-file) (diff-review-overview)
      (should (eq (caar (window-prev-buffers w)) before))
      (should-not (seq-some (lambda (e) (buffer-local-value 'diff-review--review (car e)))
                            (window-prev-buffers w)))
      (kill-current-buffer)
      (diff-review--follow)
      (should (eq (window-buffer w) before))
      (should (= (length (window-list)) 2)))))

(ert-deftest diff-review-test-quit-from-a-file ()
  (diff-review-test--with-repo
    (delete-other-windows)
    (let ((before (get-buffer-create "before")) (w (selected-window)))
      (switch-to-buffer before)
      (diff-review-test--open repo)
      (diff-review-next-file)
      (set-buffer (window-buffer w))
      (should diff-review-file-mode)
      (diff-review-quit)
      (diff-review--follow)
      (should (eq (window-buffer w) before))
      (should (= (length (window-list)) 1))
      (should (buffer-live-p (get-buffer "b.txt")))
      (should-not (buffer-local-value 'diff-review-file-mode (get-buffer "b.txt"))))))

(ert-deftest diff-review-test-commit-review-kills-its-blobs ()
  (diff-review-test--with-repo
    (delete-other-windows)
    (let ((before (get-buffer-create "before")) (w (selected-window)))
      (switch-to-buffer before)
      (diff-review-test--open repo "head.json" "HEAD^" "HEAD")
      (diff-review-next-file)
      (set-buffer (window-buffer w))
      (should diff-review-file-mode)
      (should-not buffer-file-name)
      (let ((blob (current-buffer)))
        (diff-review-quit)
        (diff-review--follow)
        (should-not (buffer-live-p blob))
        (should (eq (window-buffer w) before))))))

;;;; Nothing outside a review is touched

(ert-deftest diff-review-test-revisiting-a-blob-reuses-it ()
  "A second visit to a revision buffer keeps it, and leaves no stray overlay."
  (diff-review-test--with-repo
    (delete-other-windows)
    (diff-review-test--open repo "head.json" "HEAD^" "HEAD")
    (let ((review (current-buffer)) first)
      (with-current-buffer review (diff-review--open 1))
      (setq first (window-buffer))
      (with-current-buffer review (diff-review--open 0))
      (with-current-buffer review (diff-review--open 1))
      (should (eq (window-buffer) first))
      (with-current-buffer first
        (should diff-review--overlays)
        (should (= (length (overlays-in (point-min) (point-max)))
                   (length diff-review--overlays)))
        (should-not (seq-some (lambda (o) (= (overlay-start o) (overlay-end o)))
                              (overlays-in (point-min) (point-max))))))))

(ert-deftest diff-review-test-previous-hunk-lands-on-the-last-hunk ()
  "Walking back into a file lands on its last hunk, not its first."
  (diff-review-test--with-repo
    (diff-review-test--write "a.txt" "a\nB\nc\nd\ne\nf\ng\n")   ; hunks at 2 and 4, end at 7
    (diff-review-test--write "c.txt" "c\n")
    (diff-review-test--git "add" "c.txt")
    (diff-review-test--git "commit" "-q" "-am" "three")
    (delete-other-windows)
    (diff-review-test--open repo "range.json" "HEAD~2" "HEAD")
    (diff-review--open 2)                 ; c.txt, after src/b.txt and a.txt
    (with-current-buffer (window-buffer)
      (should-not (seq-some (lambda (h) (< (diff-review--hunk-line h) (line-number-at-pos)))
                            diff-review--hunks))
      (diff-review-previous-hunk))
    (with-current-buffer (window-buffer)
      (should (equal (plist-get (diff-review--in-review (diff-review--file)) :path) "a.txt"))
      (should (= (line-number-at-pos) 4)))))

(ert-deftest diff-review-test-hook-lives-with-the-reviews ()
  (diff-review-test--with-repo
    (delete-other-windows)
    (should-not (memq #'diff-review--follow (default-value 'window-buffer-change-functions)))
    (let ((one (diff-review-test--open repo))
          (two (diff-review-test--open repo "head.json" "HEAD^" "HEAD")))
      (should (memq #'diff-review--follow (default-value 'window-buffer-change-functions)))
      (with-current-buffer one (diff-review-quit))
      (should (memq #'diff-review--follow (default-value 'window-buffer-change-functions)))
      (with-current-buffer two (diff-review-quit))
      (should-not (memq #'diff-review--follow (default-value 'window-buffer-change-functions))))))

(ert-deftest diff-review-test-dedicated-window-stays-dedicated ()
  "The review's own window keeps its dedication through the files and after."
  (diff-review-test--with-repo
    (delete-other-windows)
    (let ((before (get-buffer-create "before")) (w (selected-window)))
      (switch-to-buffer before)
      (diff-review-test--open repo)
      (set-window-dedicated-p w t)
      (diff-review-next-file)
      (should (equal (buffer-name (window-buffer w)) "b.txt"))
      (should (eq (window-dedicated-p w) t))
      (set-buffer (window-buffer w))
      (diff-review-quit)
      (should (eq (window-buffer w) before))
      (should (eq (window-dedicated-p w) t)))))

(ert-deftest diff-review-test-dedicated-window-survives-its-blob ()
  "Ending a commit review kills its blobs without deleting a dedicated window."
  (diff-review-test--with-repo
    (delete-other-windows)
    (split-window-right)
    (let ((before (get-buffer-create "before")) (w (selected-window)))
      (switch-to-buffer before)
      (diff-review-test--open repo "head.json" "HEAD^" "HEAD")
      (set-window-dedicated-p w t)
      (diff-review-next-file)
      (set-buffer (window-buffer w))
      (should-not buffer-file-name)
      (diff-review-quit)
      (should (window-live-p w))
      (should (eq (window-buffer w) before))
      (should (eq (window-dedicated-p w) t)))))

(ert-deftest diff-review-test-dedicated-window-survives-the-overview ()
  "Quitting from the overview gives a dedicated window back, still dedicated."
  (diff-review-test--with-repo
    (delete-other-windows)
    (split-window-right)
    (let ((before (get-buffer-create "before")) (w (selected-window)))
      (switch-to-buffer before)
      (diff-review-test--open repo)
      (set-window-dedicated-p w t)
      (diff-review-next-file)
      (diff-review-overview)
      (diff-review-quit)
      (should (window-live-p w))
      (should (eq (window-buffer w) before))
      (should (eq (window-dedicated-p w) t)))))

(ert-deftest diff-review-test-dedicated-window-outside-is-left-alone ()
  "A file does not take a window dedicated to something outside the review."
  (diff-review-test--with-repo
    (delete-other-windows)
    (let ((review (diff-review-test--open repo))
          (other (get-buffer-create "other"))
          (w (split-window-right)))
      (set-window-buffer w other)
      (set-window-dedicated-p w t)
      (select-window w)
      (with-current-buffer review (diff-review-next-file))
      (should (eq (window-buffer w) other))
      (should (eq (window-dedicated-p w) t))
      (should (equal (buffer-name (window-buffer)) "b.txt")))))

(ert-deftest diff-review-test-click-on-file-lines-only ()
  (diff-review-test--with-repo
    (delete-other-windows)
    (let ((review (diff-review-test--open repo)) (w (selected-window)))
      (goto-char (point-min))
      ;; A posn, as a click gives: 29.1's `mouse-on-link-p' fails on a bare
      ;; position without `follow-link'.
      (should-not (mouse-on-link-p `(,w ,(point) (0 . 0) 0)))
      (should-not (eq (key-binding [mouse-1] nil nil (point)) #'diff-review-mouse-open))
      (should-not (eq (key-binding [mouse-2] nil nil (point)) #'diff-review-mouse-open))
      (let ((file (text-property-any (point-min) (point-max) 'diff-review-index 0)))
        (should (mouse-on-link-p `(,w ,file (0 . 0) 0)))
        (should (eq (key-binding [mouse-2] nil nil file) #'diff-review-mouse-open))
        (diff-review-mouse-open `(mouse-2 (,w ,file (0 . 0) 0)))
        (should (eq (buffer-local-value 'diff-review--review (window-buffer w)) review))))))

;;;; The review's state stays inside the review

(ert-deftest diff-review-test-header-line-comes-back ()
  "A file's own header line is back after the review, even when revisited."
  (diff-review-test--with-repo
    (delete-other-windows)
    (with-current-buffer (find-file-noselect (expand-file-name "src/b.txt" repo))
      (setq header-line-format "mine"))
    (diff-review-test--open repo)
    (diff-review-next-file) (diff-review-next-file) (diff-review-previous-file)
    (let ((b (get-buffer "b.txt")))
      (should (eq (window-buffer) b))
      (should-not (equal (buffer-local-value 'header-line-format b) "mine"))
      (diff-review-quit)
      (should (equal (buffer-local-value 'header-line-format b) "mine")))))

(ert-deftest diff-review-test-header-line-left-alone ()
  (diff-review-test--with-repo
    (delete-other-windows)
    (let ((diff-review-file-header nil))
      (with-current-buffer (find-file-noselect (expand-file-name "src/b.txt" repo))
        (setq header-line-format "mine"))
      (diff-review-test--open repo)
      (diff-review-next-file)
      (should (equal (buffer-local-value 'header-line-format (window-buffer)) "mine")))))

(defun diff-review-test--removed-lines (overlay)
  "OVERLAY's inline removed lines, a string, or nil: a bar alone has no newline."
  (when-let* ((s (overlay-get overlay 'before-string)))
    (and (string-search "\n" s) s)))

(defun diff-review-test--inline-shown-p ()
  "Whether the selected window's file shows any removed lines inline."
  (seq-some #'diff-review-test--removed-lines
            (buffer-local-value 'diff-review--overlays (window-buffer))))

(ert-deftest diff-review-test-inline-toggle-is-per-review ()
  "Toggling removed lines leaves the option and other reviews as they were."
  (diff-review-test--with-repo
    (delete-other-windows)
    (let ((diff-review-inline-removed t)
          (other (diff-review-test--open repo "head.json" "HEAD^" "HEAD")))
      (diff-review-test--open repo)
      (diff-review-next-file)                   ; src/b.txt, which lost a line
      (should (diff-review-test--inline-shown-p))
      (with-current-buffer (window-buffer) (diff-review-toggle-inline-removed))
      (should-not (diff-review-test--inline-shown-p))
      (should (eq diff-review-inline-removed t))
      (with-current-buffer other
        (should (eq diff-review--inline-removed t))
        (diff-review-next-file))
      (should (eq (buffer-local-value 'diff-review--review (window-buffer)) other))
      (should (diff-review-test--inline-shown-p)))))

(ert-deftest diff-review-test-empty-review ()
  (diff-review-test--with-repo
    (diff-review-test--git "commit" "-q" "-am" "three")
    (delete-other-windows)
    (diff-review-test--open repo)
    (should (= (length diff-review--files) 0))
    (should-error (diff-review-next-file) :type 'user-error)))

;;;; The file keys leave the major mode's alone

(ert-deftest diff-review-test-file-keys-under-the-prefix ()
  (with-temp-buffer
    (org-mode)
    (setq-local diff-review-file-mode t)  ; its keymap active, without a review
    (should (eq (key-binding (kbd "C-c C-n")) #'org-next-visible-heading))
    (should (eq (keymap-lookup nil (concat diff-review-command-prefix " n"))
                #'diff-review-next-hunk))))

(ert-deftest diff-review-test-nil-prefix-binds-nothing ()
  (let ((prefix diff-review-command-prefix))
    (unwind-protect
        (progn
          (customize-set-variable 'diff-review-command-prefix nil)
          (should (equal diff-review-file-mode-map '(keymap)))
          (customize-set-variable 'diff-review-command-prefix "C-c r")
          (should (eq (keymap-lookup diff-review-file-mode-map "C-c r n")
                      #'diff-review-next-hunk)))
      (customize-set-variable 'diff-review-command-prefix prefix))))

;;;; The prefix once: repeat-mode and next-error

(defun diff-review-test--at-a (repo &optional text)
  "Show a.txt of REPO's review, as TEXT, with point at the top.
TEXT defaults to changes at lines 2 and 4."
  (diff-review-test--write "a.txt" (or text "a\nX\nc\nD\n"))
  (delete-other-windows)
  (diff-review-test--open repo)
  (diff-review--open (seq-position diff-review--files "a.txt"
                                   (lambda (file path) (equal (plist-get file :path) path))))
  (goto-char (point-min)))

(defun diff-review-test--at (path line)
  "Whether the selected window shows the review's PATH with point on LINE."
  (with-current-buffer (window-buffer)
    (and (equal (plist-get (diff-review--in-review (diff-review--file)) :path) path)
         (= (line-number-at-pos) line))))

(ert-deftest diff-review-test-prefix-once-with-repeat-mode ()
  "With `repeat-mode' on, the prefix starts a run of file keys."
  (should (eq (get 'diff-review-next-hunk 'repeat-map) 'diff-review-file-command-map))
  (diff-review-test--with-repo
    (diff-review-test--at-a repo)
    (let ((was repeat-mode))
      (unwind-protect
          (progn (repeat-mode 1)
                 (execute-kbd-macro (kbd (concat diff-review-command-prefix " n n"))))
        (repeat-mode (if was 1 -1))))
    (should (diff-review-test--at "a.txt" 4))))

(ert-deftest diff-review-test-hunks-answer-to-next-error ()
  "M-g n and M-g p walk hunks, across files; the review's end restores the function."
  (diff-review-test--with-repo
    (let ((b (find-file-noselect (expand-file-name "src/b.txt" repo)))
          (next-error-last-buffer nil))
      (with-current-buffer b (setq-local next-error-function #'ignore))
      (diff-review-test--at-a repo)
      (execute-kbd-macro (kbd "M-g n"))
      (should (diff-review-test--at "a.txt" 2))
      (execute-kbd-macro (kbd "M-g n"))
      (should (diff-review-test--at "a.txt" 4))
      (execute-kbd-macro (kbd "M-g p"))
      (should (diff-review-test--at "a.txt" 2))
      (execute-kbd-macro (kbd "M-g p"))   ; back into src/b.txt, which comes first
      (should (diff-review-test--at "src/b.txt" 1))
      ;; `next-error' last walked from a.txt, which no longer shows.
      (execute-kbd-macro (kbd "M-g n"))
      (should (diff-review-test--at "a.txt" 2))
      (let ((a (window-buffer)))
        (diff-review-quit)
        (should (eq (buffer-local-value 'next-error-function b) #'ignore))
        (should-not (local-variable-p 'next-error-function a))))))

(ert-deftest diff-review-test-next-error-reset-and-count ()
  "C-u M-g n goes to the first hunk, on line 1 too; M-2 M-g n counts across files."
  (diff-review-test--with-repo
    (let ((next-error-last-buffer nil))
      (diff-review-test--at-a repo "X\nB\nc\nD\n")   ; hunks at 1 and 4
      (forward-line 3)
      (execute-kbd-macro (kbd "C-u M-g n"))
      (should (diff-review-test--at "a.txt" 1))
      (execute-kbd-macro (kbd "M-g p"))       ; src/b.txt, whose one hunk is line 1
      (should (diff-review-test--at "src/b.txt" 1))
      (execute-kbd-macro (kbd "M-2 M-g n"))
      (should (diff-review-test--at "a.txt" 4)))))

;;;; Key help follows the user's keys

(defun diff-review-test--header-keys ()
  "The key hints of the selected window's file header, from the first key."
  (with-current-buffer (window-buffer)
    (let ((header (diff-review--file-header)))
      (substring header (string-match "\\(C-c\\|\\]\\)" header)))))

(ert-deftest diff-review-test-header-keys-share-the-prefix ()
  "The prefix shows once; keys keep `help-key-binding', labels are `shadow'."
  (diff-review-test--with-repo
    (delete-other-windows)
    (diff-review-test--open repo)
    (diff-review-next-file)
    (let ((keys (diff-review-test--header-keys)))
      (should (equal (substring-no-properties keys)
                     "C-c _  n next hunk · v viewed · c comment · z focus · l overview"))
      (dolist (at '(0 4 7))                     ; C, _ and n
        (should (eq (get-text-property at 'face keys) 'help-key-binding)))
      (should (eq (get-text-property 9 'face keys) 'shadow)))))

(ert-deftest diff-review-test-header-keys-in-full ()
  "Keys with no shared prefix, as the README's Evil snippet binds, show in full."
  (diff-review-test--with-repo
    (delete-other-windows)
    (diff-review-test--open repo)
    (diff-review-next-file)
    (let ((emulation-mode-map-alists     ; where Evil's state maps sit
           `(((t . ,(define-keymap
                      "] c" #'diff-review-next-hunk
                      "z v" #'diff-review-viewed-and-next
                      "g c" #'diff-review-comment
                      "z f" #'diff-review-toggle-focus
                      "g l" #'diff-review-overview))))))
      (should (equal (substring-no-properties (diff-review-test--header-keys))
                     "] c next hunk · z v viewed · g c comment · z f focus · g l overview")))))

(ert-deftest diff-review-test-mode-help-lists-the-keys ()
  "`C-h m' has a table of the prefix keys, and renders with no prefix."
  (let ((prefix diff-review-command-prefix))
    (unwind-protect
        (progn
          (should (string-match-p "C-c _ n\\s-+diff-review-next-hunk"
                                  (documentation 'diff-review-file-mode)))
          (customize-set-variable 'diff-review-command-prefix nil)
          (should (string-match-p "M-x diff-review-next-hunk"
                                  (documentation 'diff-review-file-mode))))
      (customize-set-variable 'diff-review-command-prefix prefix))))

;;;; Appearance is the user's

(ert-deftest diff-review-test-customize-group ()
  (let ((members (mapcar #'car (get 'diff-review 'custom-group))))
    (dolist (symbol '(diff-review-title diff-review-file-added diff-review-file-deleted
                      diff-review-file-changed diff-review-unchanged
                      diff-review-tint-added diff-review-tint-removed
                      diff-review-hunk-recenter))
      (should (memq symbol members)))))

(ert-deftest diff-review-test-display-modes-are-left-alone ()
  "Neither list turns on `hl-line-mode' nor hides the cursor."
  (diff-review-test--with-repo
    (delete-other-windows)
    (let ((review (diff-review-test--open repo)))
      (should-not (buffer-local-value 'hl-line-mode review))
      (with-current-buffer (buffer-local-value 'diff-review--sidebar review)
        (should-not hl-line-mode)
        (should cursor-type)))))

(defun diff-review-test--faces (buffer)
  "The faces of BUFFER's review overlays and of their inline removed lines."
  (with-current-buffer buffer
    (mapcan (lambda (o)
              (cons (overlay-get o 'face)
                    (when-let* ((s (diff-review-test--removed-lines o)))
                      (list (get-text-property (string-search "\n" s) 'face s)))))
            diff-review--overlays)))

(ert-deftest diff-review-test-tints-switch-off ()
  (diff-review-test--with-repo
    (delete-other-windows)
    (diff-review-test--open repo)
    (diff-review-next-file)
    (let ((file (window-buffer)))
      (should (seq-some #'identity (diff-review-test--faces file)))
      (let ((diff-review-tint-added nil) (diff-review-tint-removed nil))
        (with-current-buffer file (diff-review--decorate))
        (should (with-current-buffer file
                  (seq-some #'diff-review-test--removed-lines diff-review--overlays)))
        (should-not (seq-some #'identity (diff-review-test--faces file)))))))

(ert-deftest diff-review-test-tint-leaves-the-hunk-bare ()
  "Tinting the removed lines of a plain file does not touch the hunk's strings."
  (let ((lines (list "old")))
    (diff-review--removed-string (diff-review--fontify lines 'fundamental-mode)
                                 'diff-review-removed)
    (should-not (text-properties-at 0 (car lines)))))

(ert-deftest diff-review-test-fontify-keeps-the-lines ()
  "Fontifying the removed lines together gives them back one by one, with faces."
  (let* ((lines (list "(defun a ()" "" "  \"s\")"))
         (texts (diff-review--fontify lines 'emacs-lisp-mode)))
    (should (equal texts lines))
    (should (get-text-property 1 'face (car texts)))
    (should-not (text-properties-at 0 (car lines)))))

(ert-deftest diff-review-test-glyph-falls-back ()
  (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
            ((symbol-function 'char-displayable-p) #'ignore))
    (should (equal (diff-review--glyph "←" "<-") "<-")))
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
    (should (equal (diff-review--glyph "←" "<-") "←"))))

;;;; Fringe bars

(defmacro diff-review-test--with-bitmap (&rest body)
  "Run BODY as if this Emacs had the bar's bitmap.
A build without a window system, like CI's, has none and draws no bars."
  (declare (indent 0))
  `(if (and (fboundp 'fringe-bitmap-p) (fringe-bitmap-p 'diff-review-bar))
       (progn ,@body)
     (cl-letf (((symbol-function 'fringe-bitmap-p) (lambda (b) (eq b 'diff-review-bar))))
       ,@body)))

(defun diff-review-test--file (name)
  "Walk the selected window's review to file NAME, and return its buffer."
  (dotimes (_ 5)
    (unless (string-suffix-p name (or (buffer-file-name (window-buffer)) ""))
      (diff-review-next-file)))
  (should (string-suffix-p name (buffer-file-name (window-buffer))))
  (window-buffer))

(defun diff-review-test--bars (buffer)
  "The bars BUFFER's review overlays draw, as (LINE . DISPLAY-SPEC), in order."
  (with-current-buffer buffer
    (let (bars)
      (dolist (o diff-review--overlays)
        (when-let* ((s (overlay-get o 'before-string)))
          (dotimes (i (length s))
            (when-let* ((spec (get-text-property i 'display s)))
              (push (cons (line-number-at-pos (overlay-start o)) spec) bars)))))
      (sort (nreverse bars) (lambda (a b) (< (car a) (car b)))))))

(ert-deftest diff-review-test-a-bar-on-every-changed-line ()
  "The removed line's bar comes before the added line it was replaced by."
  (diff-review-test--with-bitmap
    (diff-review-test--with-repo
      (delete-other-windows)
      (diff-review-test--open repo "head.json" "HEAD^")
      (should (equal (diff-review-test--bars (diff-review-test--file "a.txt"))
                     '((2 left-fringe diff-review-bar diff-indicator-removed)
                       (2 left-fringe diff-review-bar diff-indicator-added)
                       (4 left-fringe diff-review-bar diff-indicator-added)
                       (5 left-fringe diff-review-bar diff-indicator-added)))))))

(ert-deftest diff-review-test-bars-in-the-right-fringe ()
  (diff-review-test--with-bitmap
    (diff-review-test--with-repo
      (delete-other-windows)
      (diff-review-test--open repo "head.json" "HEAD^")
      (let ((file (diff-review-test--file "a.txt"))
            (diff-review-fringe 'right))
        (with-current-buffer file (diff-review--decorate))
        (should (equal (delete-dups (mapcar #'cadr (diff-review-test--bars file)))
                       '(right-fringe)))))))

(ert-deftest diff-review-test-no-bars-keeps-the-hunks ()
  "Without bars, or without the bitmap of an Emacs built for terminals only,
the hunks keep their overlays, removed lines and moves."
  (diff-review-test--with-repo
    (delete-other-windows)
    (diff-review-test--open repo "head.json" "HEAD^")
    (let ((file (diff-review-test--file "a.txt")))
      (dolist (setup (list (lambda (body) (let ((diff-review-fringe nil)) (funcall body)))
                           (lambda (body)
                             (cl-letf (((symbol-function 'fringe-bitmap-p) nil))
                               (funcall body)))))
        (funcall setup
                 (lambda ()
                   (with-current-buffer file
                     (diff-review--decorate)
                     (should-not (diff-review-test--bars file))
                     (should (seq-some #'diff-review-test--removed-lines diff-review--overlays))
                     (goto-char (point-min))
                     (diff-review-next-hunk)
                     (should (= (line-number-at-pos) 2))
                     (diff-review-next-hunk)
                     (should (= (line-number-at-pos) 4)))))))))

(ert-deftest diff-review-test-loads-without-fringes ()
  "An Emacs built without a window system has no `define-fringe-bitmap'."
  (cl-letf (((symbol-function 'define-fringe-bitmap) nil))
    (load (locate-library "diff-review.el") nil t)))

(ert-deftest diff-review-test-org-indent-kept-on-added-lines ()
  "`org-indent-mode' indents with `line-prefix', which a review overlay's would hide."
  (diff-review-test--with-bitmap
    (diff-review-test--with-repo
      (delete-other-windows)
      (diff-review-test--write "n.org" "* a\nx\n")
      (diff-review-test--git "add" ".")
      (diff-review-test--git "commit" "-q" "-m" "org")
      (diff-review-test--write "n.org" "* a\nx\ny\n")
      (diff-review-test--open repo)
      (with-current-buffer (diff-review-test--file "n.org")
        (org-indent-mode 1)
        (org-indent-add-properties (point-min) (point-max))
        (let ((y (diff-review--line-pos 3)))
          (should (diff-review-test--bars (current-buffer)))
          (should (get-text-property y 'line-prefix))
          (should (equal (get-char-property y 'line-prefix)
                         (get-text-property y 'line-prefix))))))))

;;;; The default base is the user's

(defmacro diff-review-test--without-git-config (&rest body)
  "Run BODY with git reading no global or system config."
  (declare (indent 0))
  `(let ((process-environment (append '("GIT_CONFIG_GLOBAL=/dev/null"
                                        "GIT_CONFIG_NOSYSTEM=1")
                                      process-environment)))
     ,@body))

(ert-deftest diff-review-test-default-branch-order ()
  "origin/HEAD, then init.defaultBranch, main and master that exist, then main."
  (diff-review-test--with-repo
    (diff-review-test--without-git-config
      (diff-review-test--git "branch" "-m" "main" "work")
      (should (equal (diff-review-default-branch) "main"))
      (diff-review-test--git "branch" "master")
      (should (equal (diff-review-default-branch) "master"))
      (diff-review-test--git "branch" "main")
      (should (equal (diff-review-default-branch) "main"))
      (diff-review-test--git "config" "init.defaultBranch" "trunk")
      (should (equal (diff-review-default-branch) "main"))
      (diff-review-test--git "branch" "trunk")
      (should (equal (diff-review-default-branch) "trunk"))
      (diff-review-test--git "update-ref" "refs/remotes/origin/develop" "HEAD")
      (diff-review-test--git "symbolic-ref" "refs/remotes/origin/HEAD"
                             "refs/remotes/origin/develop")
      (should (equal (diff-review-default-branch) "develop")))))

(ert-deftest diff-review-test-new-review-on-master ()
  "Without a remote, a repository on master is reviewed against master."
  (diff-review-test--with-repo
    (diff-review-test--without-git-config
      (diff-review-test--git "branch" "-m" "main" "master")
      (diff-review-find-file (expand-file-name "review.json" repo))
      (should (equal diff-review--base-name "master"))
      (should (equal (diff-review--git "rev-parse" "master") diff-review--base))
      (should (string-match-p "\"base\": \"master\""
                              (with-temp-buffer
                                (insert-file-contents (expand-file-name "review.json" repo))
                                (buffer-string)))))))

(ert-deftest diff-review-test-default-base-string ()
  (diff-review-test--with-repo
    (let ((diff-review-default-base "HEAD~1"))
      (diff-review-find-file (expand-file-name "review.json" repo))
      (should (equal diff-review--base-name "HEAD~1")))))

(ert-deftest diff-review-test-default-base-function-once ()
  "The function is called once, at the root, and the file keeps its answer."
  (diff-review-test--with-repo
    (let* ((calls nil)
           (diff-review-default-base (lambda () (push default-directory calls) "main"))
           (file (expand-file-name "sub/review.json" repo)))
      (diff-review-find-file file repo)
      (should (equal calls (list repo)))
      (kill-buffer)
      (let ((diff-review-default-base "HEAD~1"))
        (diff-review-find-file file)
        (should (equal diff-review--base-name "main"))))))

(ert-deftest diff-review-test-unknown-base-names-the-setting ()
  (diff-review-test--with-repo
    (with-current-buffer (diff-review-test--open repo)
      (setq diff-review--base-name "nope")
      (let ((err (should-error (diff-review--scan) :type 'user-error)))
        (should (string-match-p "\"nope\".*\"base\".*diff-review-default-base"
                                (cadr err)))))))

;; Evil's keys are set when it loads, once per Emacs, so each case runs in
;; a child Emacs with this one's load path.
(defun diff-review-test--evil (form)
  "Return the value of FORM, evaluated in a fresh batch Emacs."
  (with-temp-buffer
    (unless (zerop (call-process
                    (expand-file-name invocation-name invocation-directory) nil t nil
                    "-Q" "--batch" "--eval"
                    (prin1-to-string
                     `(progn (setq load-path ',load-path)
                             (prin1 ,form)))))
      (error "Child Emacs failed: %s" (buffer-string)))
    (car (read-from-string (buffer-string)))))

(ert-deftest diff-review-test-evil-keys ()
  "With Evil, the lists start in normal state with TAB folding; a file gets no keys."
  (skip-unless (locate-library "evil"))
  (should (equal (diff-review-test--evil
                  '(progn (require 'evil)
                          (require 'diff-review)
                          (list (evil-initial-state 'diff-review-mode)
                                (lookup-key (evil-get-auxiliary-keymap
                                             diff-review-mode-map 'normal)
                                            (kbd "TAB"))
                                (assq 'diff-review-file-mode
                                      (cdr (assq 'normal evil-minor-mode-keymaps-alist))))))
                 '(normal magit-section-toggle nil))))

(ert-deftest diff-review-test-evil-keys-off ()
  "`diff-review-evil-keys' nil leaves Evil untouched."
  (skip-unless (locate-library "evil"))
  (should (equal (diff-review-test--evil
                  '(progn (setq diff-review-evil-keys nil)
                          (require 'diff-review)
                          (require 'evil)
                          (list (evil-initial-state 'diff-review-mode)
                                (evil-get-auxiliary-keymap
                                 diff-review-mode-map 'normal nil t))))
                 '(nil nil))))

;;;; Comments

(defun diff-review-test--review-json (repo)
  "The review file of REPO, parsed."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "review.json" repo))
    (json-parse-string (buffer-string) :null-object nil :false-object nil)))

(defun diff-review-test--thread-overlay ()
  "The overlay of the one thread drawn in this buffer."
  (car (diff-review--thread-overlays)))

(defun diff-review-test--threads ()
  "This buffer's threads as (BODY FIRST LAST STALE), by position."
  (mapcar (lambda (o)
            (list (gethash "body" (overlay-get o 'diff-review-comment))
                  (car (diff-review--thread-span o)) (cdr (diff-review--thread-span o))
                  (overlay-get o 'diff-review-stale)))
          (diff-review--thread-overlays)))

(ert-deftest diff-review-test-comment-round-trip ()
  "A comment on a region, a reply, a resolve and a delete go through the file."
  (diff-review-test--with-repo
    (diff-review-test--at-a repo)
    (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (_prompt _string) "Why?\n"))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (let ((transient-mark-mode t))
        (forward-line 1) (push-mark (point) t t) (forward-line 2)   ; lines 2 and 3
        (diff-review-comment))
      (should (= 1 (length (diff-review--thread-overlays))))
      (let ((c (overlay-get (diff-review-test--thread-overlay) 'diff-review-comment)))
        (should (equal (gethash "path" c) "a.txt"))
        (should (equal (gethash "lines" c) [2 3]))
        (should (equal (gethash "blob" c)
                       (diff-review--in-review (plist-get (diff-review--file) :hash))))
        (should (equal (gethash "body" c) "Why?"))
        (should (and (gethash "id" c) (gethash "author" c) (gethash "at" c))))
      (goto-char (point-min)) (forward-line 3)   ; line 4, just under the thread
      (diff-review-reply)
      (diff-review-toggle-resolved)
      (let ((c (aref (gethash "comments" (diff-review-test--review-json repo)) 0)))
        (should (equal (gethash "body" (aref (gethash "replies" c) 0)) "Why?"))
        (should (eq (gethash "resolved" c) t)))
      (should (string-match-p "resolved"
                              (overlay-get (diff-review-test--thread-overlay) 'after-string)))
      (should (string-match-p "1 comment, 0 open"
                              (with-current-buffer diff-review--review (buffer-string))))
      (diff-review-delete-comment)
      (should (null (diff-review--thread-overlays)))
      (should (equal (gethash "comments" (diff-review-test--review-json repo)) [])))))

(ert-deftest diff-review-test-only-the-thread-at-point-is-open ()
  "A thread draws open while point is on its lines or just under, else as one line."
  (diff-review-test--with-repo
    (diff-review-test--at-a repo)
    (diff-review--in-review
      (diff-review--add-comment "a.txt" '(2 . 2) nil "first line\nsecond line"))
    (let ((text (lambda () (overlay-get (diff-review-test--thread-overlay) 'after-string))))
      (goto-char (point-min)) (diff-review--track-threads)
      (should-not (string-match-p "second line" (funcall text)))
      (forward-line 1) (diff-review--track-threads)
      (should (string-match-p "second line" (funcall text)))
      (should (string-match-p "line 2" (funcall text)))
      (forward-line 1) (diff-review--track-threads)   ; line 3, under the thread
      (should (string-match-p "second line" (funcall text)))
      (forward-line 1) (diff-review--track-threads)
      (should-not (string-match-p "second line" (funcall text)))
      (diff-review-toggle-threads)
      (should (string-match-p "second line" (funcall text))))))

(ert-deftest diff-review-test-threads-on-the-same-lines-open-together ()
  "Two threads on one line both open at point, tint their lines, and r asks which."
  (diff-review-test--with-repo
    (diff-review-test--at-a repo)
    (diff-review--in-review
      (diff-review--add-comment "a.txt" '(2 . 2) nil "one")
      (diff-review--add-comment "a.txt" '(2 . 3) nil "two"))
    (let ((threads (lambda ()
                     (seq-filter (lambda (o) (overlay-get o 'diff-review-comment))
                                 (overlays-in (point-min) (point-max))))))
      (goto-char (point-min)) (diff-review--track-threads)
      (should (seq-every-p (lambda (o) (null (overlay-get o 'face))) (funcall threads)))
      (forward-line 1) (diff-review--track-threads)
      (should (= 2 (seq-count (lambda (o) (overlay-get o 'diff-review-open)) (funcall threads))))
      (should (seq-every-p (lambda (o) (and (overlay-get o 'face)
                                            (string-match-p "reply" (overlay-get o 'after-string))))
                           (funcall threads)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt names &rest _) (seq-find (lambda (n) (string-search ": two" n)) names)))
                ((symbol-function 'read-string-from-buffer) (lambda (_p _s) "ok")))
        (diff-review-reply))
      (let ((c (seq-find (lambda (c) (equal (gethash "body" c) "two"))
                         (diff-review--in-review diff-review--comments))))
        (should (= 1 (length (gethash "replies" c))))))))

;;;; The JSON

(ert-deftest diff-review-test-toggle-raw ()
  "C-c C-c shows the file as its JSON in the same window, the review ended, and back."
  (diff-review-test--with-repo
    (let* ((review (diff-review-test--open repo))
           (window (selected-window)))
      (should (diff-review-test--sidebar window))
      (diff-review-toggle-raw)
      (should (eq (window-buffer window) review))
      (with-current-buffer review
        (should-not (derived-mode-p 'diff-review-mode))
        (should diff-review-raw-mode)
        (should-not buffer-read-only)
        (should-not (buffer-modified-p))
        (should (equal (buffer-string)
                       (with-temp-buffer
                         (insert-file-contents (expand-file-name "review.json" repo))
                         (buffer-string)))))
      (should-not (diff-review--sidebar-window window))
      (should-not (memq #'diff-review--follow window-buffer-change-functions))
      (diff-review-toggle-raw)
      (should (eq (window-buffer window) review))
      (with-current-buffer review
        (should (derived-mode-p 'diff-review-mode))
        (should-not diff-review-raw-mode))
      (diff-review--follow)
      (should (diff-review-test--sidebar window)))))

(ert-deftest diff-review-test-toggle-raw-keeps-broken-json ()
  "JSON that is no longer a review stays as it is, in the raw buffer."
  (diff-review-test--with-repo
    (let ((review (diff-review-test--open repo)))
      (diff-review-toggle-raw)
      (with-current-buffer review
        (goto-char (point-min))
        (insert "oops")
        (save-buffer)
        (should-error (diff-review-toggle-raw) :type 'user-error)
        (should diff-review-raw-mode)
        (should-not (derived-mode-p 'diff-review-mode))
        (should (string-prefix-p "oops" (buffer-string)))))))

(ert-deftest diff-review-test-revert-leaves-nothing-behind ()
  "A plain revert, which re-runs the major mode, takes the review off with it."
  (diff-review-test--with-repo
    (diff-review-test--at-a repo)
    (diff-review--in-review (diff-review--add-comment "a.txt" '(2 . 2) nil "here"))
    (should (overlays-in (point-min) (point-max)))
    (revert-buffer t t)
    (should-not diff-review-file-mode)
    (should-not (overlays-in (point-min) (point-max)))
    (should-not (memq #'diff-review--track-threads post-command-hook))))

(ert-deftest diff-review-test-comment-names-the-buffer-s-file ()
  "A comment from a file that is not the one shown names its own file.
Redrawing after a write keeps each file's threads in that file."
  (diff-review-test--with-repo
    (diff-review-test--at-a repo)
    (let ((a (window-buffer)))
      (diff-review--in-review (diff-review--open (diff-review--file-index "src/b.txt")))
      (with-current-buffer a
        (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (_p _s) "from a")))
          (goto-char (point-min))
          (diff-review-comment))
        (should (equal (mapcar #'car (diff-review-test--threads)) '("from a")))
        (should (equal (gethash "path" (aref (diff-review--in-review diff-review--comments) 0))
                       "a.txt")))
      (with-current-buffer (window-buffer)   ; src/b.txt
        (should (null (diff-review--thread-overlays)))
        (should (string-match-p " 1/2 M src/b.txt" (diff-review--file-header)))))))

(ert-deftest diff-review-test-comment-on-unsaved-text ()
  "A comment's lines and blob describe the buffer's text, saved or not."
  (diff-review-test--with-repo
    (diff-review-test--at-a repo)
    (goto-char (point-min))
    (insert "new\n")                    ; unsaved: a is on line 2 now
    (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (_p _s) "on a")))
      (diff-review-comment))
    (set-buffer-modified-p nil)
    (let ((c (aref (diff-review--in-review diff-review--comments) 0)))
      (should (equal (gethash "lines" c) [2 2]))
      (should-not (equal (gethash "blob" c)
                         (plist-get (diff-review--this-file) :hash)))   ; not the saved file's
      (should (equal (mapcar #'cdr (diff-review-test--threads)) '((2 2 nil)))))))

(ert-deftest diff-review-test-thread-on-the-last-line ()
  "A comment on the empty line a file ends with hangs under the last line of text."
  (diff-review-test--with-repo
    (diff-review-test--at-a repo)
    (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (_p _s) "at the end")))
      (goto-char (point-max))
      (diff-review-comment))
    (let ((c (aref (diff-review--in-review diff-review--comments) 0)))
      (should (equal (gethash "lines" c) [5 5]))
      (should (equal (mapcar #'cdr (diff-review-test--threads)) '((4 4 nil)))))))

(ert-deftest diff-review-test-lines-count-the-whole-file ()
  "In a narrowed buffer a comment's lines, and its drawing, count from the file's top."
  (diff-review-test--with-repo
    (diff-review-test--at-a repo)
    (narrow-to-region (diff-review--line-pos 3) (point-max))
    (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (_p _s) "narrow")))
      (goto-char (point-min))
      (diff-review-comment))
    (should (equal (gethash "lines" (aref (diff-review--in-review diff-review--comments) 0))
                   [3 3]))
    (should (equal (mapcar #'cdr (diff-review-test--threads)) '((3 3 nil))))
    (widen)))

(ert-deftest diff-review-test-threads-that-read-alike-stay-apart ()
  "Two threads at point with the same author and opening line are offered apart."
  (diff-review-test--with-repo
    (diff-review-test--at-a repo)
    (let (second offered)
      (diff-review--in-review
        (diff-review--add-comment "a.txt" '(2 . 2) nil "same\nfirst")
        (setq second (diff-review--add-comment "a.txt" '(2 . 2) nil "same\nsecond")))
      (goto-char (point-min)) (forward-line 1)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt names &rest _)
                   (setq offered names)
                   (seq-find (lambda (n) (string-search (gethash "id" second) n)) names)))
                ((symbol-function 'read-string-from-buffer) (lambda (_p _s) "to the second")))
        (diff-review-reply))
      (should (= 2 (length (seq-uniq offered))))
      (let ((c (diff-review--in-review (diff-review--comment (gethash "id" second)))))
        (should (= 1 (length (gethash "replies" c))))))))

(ert-deftest diff-review-test-thread-under-an-unterminated-last-line ()
  "A thread on the last line of a file with no final newline draws under it."
  (diff-review-test--with-repo
    (diff-review-test--at-a repo "a\nX\nc\nD")
    (diff-review--in-review (diff-review--add-comment "a.txt" '(4 . 4) nil "last"))
    (should (string-prefix-p "\n" (overlay-get (diff-review-test--thread-overlay) 'after-string)))
    (goto-char (point-min))
    (diff-review--in-review (diff-review--add-comment "a.txt" '(1 . 1) nil "first"))
    (should-not (string-prefix-p "\n" (overlay-get (car (diff-review--thread-overlays))
                                                   'after-string)))))

(ert-deftest diff-review-test-thread-follows-its-lines ()
  "A thread written against an older blob maps its lines; one inside a change is outdated."
  (diff-review-test--with-repo
    ;; HEAD has a B c d; the worktree gets a line on top, and d changed.
    (diff-review-test--at-a repo "0\na\nB\nc\nD\n")
    (let ((old (diff-review--in-review (diff-review--git "rev-parse" "HEAD:a.txt"))))
      (diff-review--in-review
        (diff-review--add-comment "a.txt" '(2 . 2) old "on B")
        (diff-review--add-comment "a.txt" '(4 . 4) old "on d")
        (diff-review--add-comment "a.txt" '(5 . 5) nil "no blob")))
    (should (equal (sort (diff-review-test--threads) (lambda (a b) (string< (car a) (car b))))
                   '(("no blob" 5 5 nil) ("on B" 3 3 nil) ("on d" 4 4 t))))))

(ert-deftest diff-review-test-agent-comment-as-written ()
  "An agent's comment, with false and a key of its own, comes back as it went in."
  (diff-review-test--with-repo
    (diff-review-test--write
     "review.json"
     (concat "{\"format\": \"diff-review/1\", \"base\": \"main\", \"comments\": ["
             "{\"id\": \"x1\", \"path\": \"a.txt\", \"lines\": [4, 4], \"author\": \"agent\", "
             "\"at\": \"2026-11-06T10:00:00+13:00\", \"body\": \"hm\", \"severity\": \"nit\", "
             "\"resolved\": false}]}"))
    (diff-review-test--at-a repo)
    (should (equal (mapcar #'cdr (diff-review-test--threads)) '((4 4 nil))))
    (diff-review--in-review (diff-review--set-viewed (diff-review--file) t))   ; a write
    (let ((text (with-temp-buffer
                  (insert-file-contents (expand-file-name "review.json" repo))
                  (buffer-string))))
      (should (string-match-p "\"severity\": \"nit\"" text))
      (should (string-match-p "\"resolved\": null" text)))))

(ert-deftest diff-review-test-overview-lists-the-threads ()
  "The overview lists every thread, RET reaching its lines, in or outside the diff."
  (diff-review-test--with-repo
    (delete-other-windows)
    (diff-review-test--write "notes.md" "n1\nn2\n")   ; not in the diff
    (let ((review (diff-review-test--open repo)))
      (let ((c (diff-review--add-comment nil nil nil "Overall fine.\nSecond line.")))
        (puthash "replies" (vector (diff-review--message "claude" "Thanks.")) c)
        (puthash "comments" diff-review--comments diff-review--data))
      (diff-review--add-comment "src/b.txt" '(1 . 1) nil "b?")
      (diff-review--add-comment "notes.md" '(2 . 2) nil "outside")
      (should (string-match-p "3 comments, 3 open" (buffer-string)))
      (goto-char (point-min))
      (search-forward "\nComments\n")
      (should (looking-at "  review  "))
      (should (string-match-p "Second line\.\n.*claude.*\n.*Thanks\." (buffer-string)))
      (diff-review-open-at-point)             ; folds the review-wide thread
      (should (eq (window-buffer) review))
      (should (invisible-p (save-excursion (forward-line 1) (point))))
      (search-forward "  src/b.txt:1  ")
      (beginning-of-line)
      (diff-review-open-at-point)
      (should (diff-review-test--at "src/b.txt" 1))
      (should (with-current-buffer (window-buffer) (diff-review--thread-overlays)))
      (diff-review-overview)
      (with-current-buffer review
        (goto-char (point-min))
        (search-forward "  notes.md:2  ")
        (diff-review-open-at-point))
      (should (diff-review-test--at "notes.md" 2))   ; in the review, unchanged
      (with-current-buffer (window-buffer)
        (should diff-review-file-mode)
        (should (null diff-review--hunks))
        (should (equal (mapcar #'cdr (diff-review-test--threads)) '((2 2 nil)))))
      (with-current-buffer review
        (should (string-match-p "2 files changed" (buffer-string)))
        (should (string-match-p "^  notes.md" (buffer-string)))))))

(ert-deftest diff-review-test-walk-the-threads ()
  "N and P walk the threads, across files; src/b.txt comes first, directories first."
  (diff-review-test--with-repo
    (diff-review-test--at-a repo)
    (diff-review--in-review
      (diff-review--add-comment "a.txt" '(4 . 4) nil "one")
      (diff-review--add-comment "src/b.txt" '(1 . 1) nil "two"))
    (goto-char (point-min))
    (diff-review-next-comment)
    (should (diff-review-test--at "a.txt" 4))
    (with-current-buffer (window-buffer) (diff-review-previous-comment))
    (should (diff-review-test--at "src/b.txt" 1))
    (with-current-buffer (window-buffer) (diff-review-next-comment))
    (should (diff-review-test--at "a.txt" 4))
    (with-current-buffer (window-buffer) (diff-review-next-comment))
    (should (diff-review-test--at "a.txt" 4))))

(ert-deftest diff-review-test-focus-keeps-a-thread ()
  "Folding the unchanged lines leaves a thread and its context in view."
  (diff-review-test--with-repo
    (let ((lines (lambda (from) (mapconcat (lambda (i) (format "l%d\n" i)) (number-sequence from 30) ""))))
      (diff-review-test--write "long.txt" (funcall lines 1))
      (diff-review-test--git "add" ".")
      (diff-review-test--git "commit" "-q" "-m" "three")
      (diff-review-test--write "long.txt" (concat "L1\n" (funcall lines 2))))
    (delete-other-windows)
    (diff-review-test--open repo)
    (diff-review--add-comment "long.txt" '(20 . 20) nil "deep")
    (diff-review--open (seq-position diff-review--files "long.txt"
                                     (lambda (f p) (equal (plist-get f :path) p))))
    (diff-review-toggle-focus)
    (let ((hidden (lambda (line)
                    (let ((pos (diff-review--line-pos line)))
                      (seq-some (lambda (o) (and (overlay-get o 'display)
                                                 (<= (overlay-start o) pos) (> (overlay-end o) pos)))
                                diff-review--overlays)))))
      (should (funcall hidden 10))
      (should-not (funcall hidden 20))
      (should (funcall hidden 28)))))

;;;; A branch's review

(ert-deftest diff-review-test-branch-review-follows-the-branch ()
  "From main's clone the branch's review is under the common git dir, its
files the worktree's own; a commit on the branch is noticed, and a rescan follows it."
  (diff-review-test--with-repo
    (let ((wt (diff-review-test--worktree repo "wt" "-b" "feature")))
      (diff-review-test--commit-in wt "a.txt" "a\nB\nc\nd\nE\n" "feature one")
      (delete-other-windows)
      (diff-review-branch "feature")
      (diff-review--follow)
      (let ((review (current-buffer)) real)
        (should (diff-review-test--same-file-p
                 buffer-file-name (expand-file-name ".git/review/feature.json" repo)))
        (should (equal diff-review--head "feature"))
        (should (equal (mapcar (lambda (f) (plist-get f :path)) diff-review--files) '("a.txt")))
        (should (equal diff-review--tip (diff-review--git "rev-parse" "feature")))
        ;; The file is the worktree's: a real file in its tree, for xref and eglot.
        (diff-review--open 0)
        (setq real (window-buffer))
        (with-current-buffer real
          (should diff-review-file-mode)
          (should-not buffer-read-only)
          (should (diff-review-test--same-file-p buffer-file-name (expand-file-name "a.txt" wt))))
        (with-current-buffer review (diff-review--set-viewed (diff-review--file 0) t))
        ;; The branch moves: the overview says so when shown, and a rescan follows.
        (diff-review-test--commit-in wt "a.txt" "a\nB\nc\nd\nE\nf\n" "feature two")
        (diff-review-test--commit-in wt "src/b.txt" "w\n" "feature three")
        (diff-review-overview)
        (should-not (string-match-p "moved to" (buffer-string)))
        (diff-review--check-tip (selected-window))
        (should (string-match-p "feature moved to [0-9a-f]\\{7\\}," (buffer-string)))
        (should (string-match-p "rescan" (buffer-string)))
        ;; Until the rescan the review is of the tip it scanned: the worktree has
        ;; moved on, so the file comes from a throwaway of that tip, with its hunks.
        (diff-review--open 0)
        (with-current-buffer (window-buffer)
          (should buffer-read-only)
          (should (equal (buffer-string) "a\nB\nc\nd\nE\n"))
          (should (equal (mapcar (lambda (h) (plist-get h :line)) diff-review--hunks) '(5))))
        (with-current-buffer review (diff-review-refresh))
        (should (eq (window-buffer) real))        ; the worktree's file again
        (set-buffer review)
        (should-not diff-review--moved)
        (should-not (string-match-p "moved to" (buffer-string)))
        (should (equal diff-review--tip (diff-review--git "rev-parse" "feature")))
        (should (equal (mapcar (lambda (f) (plist-get f :path)) diff-review--files)
                       '("src/b.txt" "a.txt")))
        (should-not (diff-review--viewed-p (diff-review--file 1)))   ; a.txt changed again
        (with-current-buffer real              ; reread under the review, still a real file
          (should diff-review-file-mode)
          (should (equal (buffer-string) "a\nB\nc\nd\nE\nf\n")))
        ;; Unsaved edits in the worktree's buffer: the branch's blob shows, the edits stay.
        (with-current-buffer real (goto-char (point-max)) (insert "edit\n"))
        (with-current-buffer review (diff-review--open 1))
        (with-current-buffer (window-buffer)
          (should diff-review-file-mode)
          (should-not buffer-file-name)
          (should (equal (buffer-string) "a\nB\nc\nd\nE\nf")))   ; a blob, without its last newline
        (should-not (buffer-local-value 'diff-review-file-mode real))
        (should (buffer-modified-p real))
        (with-current-buffer real (revert-buffer t t))
        (with-current-buffer review (diff-review--open 1))
        (should (eq (window-buffer) real))
        ;; The file dirtied on disk: a rescan gives the window the blob instead.
        (with-temp-file (expand-file-name "a.txt" wt) (insert "dirty\n"))
        (with-current-buffer review (diff-review-refresh))
        (with-current-buffer (window-buffer)
          (should diff-review-file-mode)
          (should-not buffer-file-name)
          (should (equal (buffer-string) "a\nB\nc\nd\nE\nf")))
        (should-not (buffer-local-value 'diff-review-file-mode real))
        ;; The same file, with its marks, from inside the worktree.
        (with-current-buffer review (diff-review--set-viewed (diff-review--file 0) t))
        (let ((file buffer-file-name))
          (kill-buffer review)
          (let ((default-directory wt))
            (diff-review-branch "feature"))
          (should (diff-review-test--same-file-p buffer-file-name file))
          (should (diff-review--viewed-p (diff-review--file 0)))
          (should-not (diff-review--viewed-p (diff-review--file 1))))))))

(ert-deftest diff-review-test-branch-file-and-candidates ()
  "A slash in the branch makes folders; the picker offers the other worktrees' branches."
  (diff-review-test--with-repo
    (let ((wt (diff-review-test--worktree repo "wt" "-b" "feat/x")) seen)
      (should (diff-review-test--same-file-p (diff-review--branch-file repo "feat/x")
                                             (expand-file-name ".git/review/feat/x.json" repo)))
      (let ((case-fold-search nil))      ; git's HEAD is read whatever the user's setting
        (should (plist-get (car (diff-review--worktrees repo)) :head)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _) (setq seen collection) (car collection))))
        (should (equal (diff-review--read-branch) "feat/x"))
        (should (equal seen '("feat/x")))
        (let ((default-directory wt))
          (should (equal (diff-review--read-branch) "main"))))
      (delete-other-windows)
      (diff-review-branch "feat/x")
      (should (file-exists-p (expand-file-name ".git/review/feat/x.json" repo)))
      (should (equal diff-review--head "feat/x"))
      (should (equal diff-review--head-name "feat/x")))))

(ert-deftest diff-review-test-branch-review-outlives-its-worktree ()
  "A review made from a worktree opens from main after the worktree is gone, marks kept."
  (diff-review-test--with-repo
    (let ((wt (diff-review-test--worktree repo "wt" "-b" "feature")))
      (diff-review-test--commit-in wt "a.txt" "a\nB\nc\nd\nE\n" "feature one")
      (delete-other-windows)
      (diff-review-find-file (diff-review--branch-file repo "feature") wt nil "feature")
      (diff-review--set-viewed (diff-review--file 0) t)
      (kill-buffer)                     ; the current buffer, and its default-directory, change
      (let ((default-directory repo))
        (diff-review-test--git "worktree" "remove" "--force" wt)
        (diff-review-branch "feature"))
      (should (diff-review-test--same-file-p diff-review--root repo))
      (should (diff-review--viewed-p (diff-review--file 0))))))

(ert-deftest diff-review-test-changed-throwaway-is-kept ()
  "A throwaway worktree with changes in it is not removed on quit."
  (diff-review-test--with-repo
    (diff-review-test--git "branch" "stale")
    (diff-review-test--git "commit" "-q" "-am" "three")
    (delete-other-windows)
    (diff-review-test--open repo "stale.json" "HEAD~2" "stale")
    (diff-review--open 1)
    (let* ((review (diff-review--review-buffer))
           (dir (buffer-local-value 'diff-review--worktree review))
           (edited (find-file-noselect (expand-file-name "src/b.txt" dir))))
      (with-current-buffer edited           ; unsaved edits, past the read-only
        (let ((inhibit-read-only t)) (insert "x")))
      (with-current-buffer review (diff-review-quit))
      (should (buffer-live-p edited))
      (should (file-directory-p dir))
      ;; Changes on disk keep it as well.
      (set-buffer-modified-p nil)
      (kill-buffer edited)
      (diff-review-test--open repo "stale2.json" "HEAD~2" "stale")
      (diff-review--open 1)
      (with-temp-file (expand-file-name "note.txt" dir) (insert "mine\n"))
      (diff-review-quit)
      (should (file-exists-p (expand-file-name "note.txt" dir))))))

(ert-deftest diff-review-test-symlink-is-a-revision-buffer ()
  "A tracked symlink shows its blob, the link, not the file it points at."
  (diff-review-test--with-repo
    (let ((wt (diff-review-test--worktree repo "wt" "-b" "feature")))
      (let ((default-directory wt))
        (make-symbolic-link "a.txt" "link")
        (diff-review-test--git "add" "link")
        (diff-review-test--git "commit" "-q" "-m" "a link"))
      (delete-other-windows)
      (diff-review-branch "feature")
      (should (equal (plist-get (diff-review--file 0) :path) "link"))
      (diff-review--open 0)
      (with-current-buffer (window-buffer)
        (should diff-review-file-mode)
        (should-not buffer-file-name)
        (should (equal (buffer-string) "a.txt"))))))

(ert-deftest diff-review-test-real-file-stays-with-its-review ()
  "Two reviews at one checked-out commit: the second does not take the first's buffer."
  (diff-review-test--with-repo
    (diff-review-test--git "commit" "-q" "-am" "three")   ; main clean, at HEAD
    (delete-other-windows)
    (let ((one (diff-review-test--open repo "one.json" "HEAD^" "HEAD")))
      (diff-review--open 1)
      (let ((real (window-buffer))
            (two (diff-review-test--open repo "two.json" "HEAD~2" "HEAD")))
        (should (buffer-file-name real))
        (diff-review--open 1)
        (with-current-buffer (window-buffer)
          (should-not (eq (current-buffer) real))
          (should-not buffer-file-name)
          (should (eq diff-review--review two)))
        (should (eq (buffer-local-value 'diff-review--review real) one))
        (with-current-buffer one (diff-review-quit))
        (with-current-buffer two (diff-review--open 1))   ; free now: the real file
        (should (eq (window-buffer) real))))))

(ert-deftest diff-review-test-revision-buffers-are-the-reviews-own ()
  "Two reviews of the same head name and path keep separate revision buffers."
  (diff-review-test--with-repo
    (delete-other-windows)
    (let ((one (diff-review-test--open repo "one.json" "HEAD^" "HEAD")))
      (diff-review--open 1)
      (let ((first (window-buffer))
            (two (diff-review-test--open repo "two.json" "HEAD^" "HEAD")))
        (diff-review--open 1)
        (should-not (eq (window-buffer) first))
        (should (eq (buffer-local-value 'diff-review--review first) one))
        (should (eq (buffer-local-value 'diff-review--review (window-buffer)) two))
        (with-current-buffer two (diff-review-quit))
        (should (buffer-live-p first))
        (with-current-buffer one (diff-review--open 1))
        (should (eq (window-buffer) first))))))

(ert-deftest diff-review-test-file-absent-from-a-clean-checkout ()
  "A file the matching worktree has not on disk, as a sparse checkout omits, is a blob."
  (diff-review-test--with-repo
    (let ((wt (diff-review-test--worktree repo "wt" "-b" "feature")))
      (diff-review-test--commit-in wt "a.txt" "a\nB\nc\nd\nE\n" "feature one")
      (let ((default-directory wt))
        (diff-review-test--git "update-index" "--skip-worktree" "a.txt")
        (delete-file "a.txt"))
      (delete-other-windows)
      (diff-review-branch "feature")
      (diff-review--open 0)
      (with-current-buffer (window-buffer)
        (should diff-review-file-mode)
        (should-not buffer-file-name)
        (should (equal (buffer-string) "a\nB\nc\nd\nE"))))))

(defun diff-review-test--worktree-entries (repo)
  "The paths `git worktree list' gives for REPO."
  (let ((default-directory repo))
    (mapcar (lambda (w) (plist-get w :path)) (diff-review--worktrees repo))))

(ert-deftest diff-review-test-throwaway-worktree ()
  "Commits no worktree has are checked out under review/worktrees, read-only;
a moved tip gets a new worktree, the old one going; the last goes on quit."
  (diff-review-test--with-repo
    (diff-review-test--git "branch" "stale")               ; at two
    (diff-review-test--git "commit" "-q" "-am" "three")    ; main moves on
    (delete-other-windows)
    (diff-review-test--open repo "stale.json" "HEAD~2" "stale")
    (let* ((review (current-buffer))
           (hash (diff-review--git "rev-parse" "stale"))
           (dir (expand-file-name (concat ".git/review/worktrees/" hash) repo))
           file)
      (should-not (file-directory-p dir))
      (diff-review--open 1)                                 ; a.txt, after src/b.txt
      (setq file (window-buffer))
      (should (diff-review-test--same-file-p (buffer-local-value 'diff-review--worktree review) dir))
      (should (seq-some (lambda (p) (diff-review-test--same-file-p p dir))
                        (diff-review-test--worktree-entries repo)))
      (with-current-buffer file
        (should diff-review-file-mode)
        (should buffer-read-only)
        (should (diff-review-test--same-file-p buffer-file-name (expand-file-name "a.txt" dir)))
        (should (equal (buffer-string) "a\nB\nc\nd\n")))
      (with-current-buffer review (diff-review--open 0))   ; the next file: the same worktree, read-only
      (with-current-buffer (window-buffer)
        (should buffer-read-only)
        (should (diff-review-test--same-file-p buffer-file-name (expand-file-name "src/b.txt" dir))))
      (with-current-buffer review (diff-review--open 1))
      (should (eq (window-buffer) file))
      ;; A second review of the commit gets no share of the worktree: blobs, and no removal.
      (let ((second (diff-review-test--open repo "second.json" "HEAD~2" "stale")))
        (diff-review--open 1)
        (with-current-buffer (window-buffer)
          (should diff-review-file-mode)
          (should-not buffer-file-name))
        (should-not (buffer-local-value 'diff-review--worktree second))
        (with-current-buffer second (diff-review-quit))
        (should (file-directory-p dir))
        (with-current-buffer review (diff-review--open 1))
        (should (eq (window-buffer) file)))
      ;; stale moves, by a commit in a worktree that then goes: still nobody has it.
      (let ((tmp (diff-review-test--worktree repo "tmp" "stale")))
        (diff-review-test--commit-in tmp "a.txt" "a\nB\nc\nd\nE\n" "stale two")
        (diff-review-test--git "worktree" "remove" tmp))
      (let ((other (find-file-noselect (expand-file-name "src/b.txt" dir))))   ; as xref would
        (should (buffer-local-value 'buffer-read-only other))
        (with-current-buffer review (diff-review-refresh))
        (should-not (buffer-live-p other)))   ; gone with the old commit's worktree
      (let* ((new (with-current-buffer review (diff-review--git "rev-parse" "stale")))
             (new-dir (expand-file-name (concat ".git/review/worktrees/" new) repo)))
        (should-not (equal new hash))
        (should (equal (buffer-local-value 'diff-review--tip review) new))
        (should-not (file-directory-p dir))
        (should (file-directory-p new-dir))
        (should-not (buffer-live-p file))
        (setq file (window-buffer))
        (with-current-buffer file
          (should diff-review-file-mode)
          (should buffer-read-only)
          (should (diff-review-test--same-file-p buffer-file-name (expand-file-name "a.txt" new-dir)))
          (should (equal (buffer-string) "a\nB\nc\nd\nE\n"))
          ;; Edited past the read-only, the file is shown as the commit has it.
          (setq buffer-read-only nil)
          (goto-char (point-max))
          (insert "edit\n"))
        (with-current-buffer review (diff-review--open 1))
        (with-current-buffer (window-buffer)
          (should-not buffer-file-name)
          (should (equal (buffer-string) "a\nB\nc\nd\nE")))
        (with-current-buffer file (revert-buffer t t))
        (with-current-buffer review (diff-review--open 1))
        (should (eq (window-buffer) file))
        (setq dir new-dir))
      (let ((other (find-file-noselect (expand-file-name "src/b.txt" dir))))   ; as xref would
        (should (buffer-local-value 'buffer-read-only other))
        (with-current-buffer review (diff-review-quit))
        (should-not (buffer-live-p file))
        (should-not (buffer-live-p other)))
      (should-not (file-directory-p dir))
      (should-not (seq-some (lambda (p) (string-match-p "review/worktrees" p))
                            (diff-review-test--worktree-entries repo))))))

(defun diff-review-test--dead-pid ()
  "The pid of a process that has exited."
  (let ((process (start-process "diff-review-test" nil "true")))
    (while (process-live-p process) (sleep-for 0.02))
    (process-id process)))

(ert-deftest diff-review-test-leftover-worktree-is-pruned ()
  "A throwaway worktree a killed Emacs left goes when the next review adds one;
one a running Emacs holds stays."
  (diff-review-test--with-repo
    (diff-review-test--git "branch" "stale")
    (diff-review-test--git "commit" "-q" "-am" "three")
    (diff-review-test--write "c.txt" "c\n")
    (diff-review-test--git "add" "c.txt")
    (diff-review-test--git "commit" "-q" "-m" "four")
    (delete-other-windows)
    (diff-review-test--open repo "stale.json" "HEAD~3" "stale")
    (let* ((review (current-buffer))
           (old (expand-file-name (concat ".git/review/worktrees/" (diff-review--git "rev-parse" "stale"))
                                  repo))
           (new (expand-file-name (concat ".git/review/worktrees/" (diff-review--git "rev-parse" "HEAD~1"))
                                  repo)))
      (let ((dead (diff-review-test--dead-pid)))
        (cl-letf (((symbol-function 'emacs-pid) (lambda () dead)))   ; an Emacs that is gone
          (diff-review--open 1))
        (should (file-directory-p old))
        (should (equal (plist-get (seq-find (lambda (w) (diff-review-test--same-file-p (plist-get w :path) old))
                                            (diff-review--worktrees repo))
                                  :locked)
                       (format "diff-review %d" dead))))
      (with-current-buffer review               ; Emacs dies: no close runs
        (remove-hook 'kill-buffer-hook #'diff-review--close t)
        (kill-buffer))
      (remove-hook 'window-buffer-change-functions #'diff-review--follow)
      (should (file-directory-p old))
      (diff-review-test--open repo "mid.json" "HEAD~3" "HEAD~1")
      (diff-review--open 1)
      (should-not (file-directory-p old))
      (should (file-directory-p new))
      (let ((entries (diff-review-test--worktree-entries repo)))
        (should (seq-some (lambda (p) (diff-review-test--same-file-p p new)) entries))
        (should-not (seq-some (lambda (p) (diff-review-test--same-file-p p old)) entries)))
      ;; Another review needing a throwaway leaves this Emacs's in place.
      (diff-review-test--open repo "again.json" "HEAD~3" "stale")
      (diff-review--open 1)
      (should (file-directory-p old))
      (should (file-directory-p new)))))

;;;; The verdict

(defun diff-review-test--verdict (review)
  "The verdict in REVIEW's file on disk, as an alist."
  (with-temp-buffer
    (insert-file-contents (buffer-file-name review))
    (alist-get 'verdict (json-parse-string (buffer-string) :object-type 'alist))))

(ert-deftest diff-review-test-verdict-turns ()
  "Approve, a commit on the branch, request changes: the three turns, in the
file, the overview, the sidebar's and the file's headers."
  (diff-review-test--with-repo
    (let ((wt (diff-review-test--worktree repo "wt" "-b" "feature"))
          (diff-review-author "rev"))
      (diff-review-test--commit-in wt "a.txt" "a\nB\nc\nd\nE\n" "feature one")
      (delete-other-windows)
      (diff-review-branch "feature")
      (diff-review--follow)
      (let ((review (current-buffer))
            (summary (lambda (review)
                       (with-current-buffer review
                         (substring-no-properties (diff-review--summary)))))
            (header (lambda ()
                      (with-current-buffer (window-buffer)
                        (substring-no-properties (diff-review--file-header))))))
        (should (eq (diff-review--turn) 'reviewer))
        (should-not (string-match-p "approved\\|changes" (funcall summary review)))
        ;; Approved at the tip: the merge's turn.
        (execute-kbd-macro "A")
        (let ((verdict (diff-review-test--verdict review)))
          (should (equal (alist-get 'state verdict) "approved"))
          (should (equal (alist-get 'head verdict) (diff-review--git "rev-parse" "feature")))
          (should (equal (alist-get 'by verdict) "rev"))
          (should (alist-get 'at verdict)))
        (should (eq (diff-review--turn) 'merge))
        (should (string-match-p "approved" (buffer-string)))
        (should (string-match-p "approved\\'" (with-current-buffer diff-review--sidebar
                                                (substring-no-properties
                                                 (diff-review-sidebar--header)))))
        (diff-review--open 0)
        (should (string-match-p "approved" (funcall header)))
        ;; A commit on the branch: the reviewer's turn, noticed before the rescan.
        (diff-review-test--commit-in wt "a.txt" "a\nB\nc\nd\nE\nf\n" "feature two")
        (diff-review-overview)
        (diff-review--check-tip (selected-window))
        (should (eq (diff-review--turn) 'reviewer))
        (should (string-match-p "approved, stale" (funcall summary review)))
        (diff-review-refresh)
        (should (eq (diff-review--turn) 'reviewer))
        (diff-review--open 0)
        (should (string-match-p "approved, stale" (funcall header)))
        ;; Changes at the new tip: the author's turn.
        (diff-review-overview)
        (execute-kbd-macro "C")
        (should (equal (alist-get 'state (diff-review-test--verdict review)) "changes"))
        (should (equal (alist-get 'head (diff-review-test--verdict review))
                       (diff-review--git "rev-parse" "feature")))
        (should (eq (diff-review--turn) 'author))
        (diff-review--open 0)
        (should (string-match-p "changes requested" (funcall header)))
        (should-not (string-match-p "stale" (funcall header)))))))

(ert-deftest diff-review-test-verdict-written-beside ()
  "An agent's verdict shows after the reread, and Emacs's own keeps its keys."
  (diff-review-test--with-repo
    (let ((wt (diff-review-test--worktree repo "wt" "-b" "feature")))
      (diff-review-test--commit-in wt "a.txt" "a\nB\nc\nd\nE\n" "feature one")
      (delete-other-windows)
      (diff-review-branch "feature")
      (let* ((review (current-buffer))
             (data (with-temp-buffer
                     (insert-file-contents (buffer-file-name review))
                     (json-parse-string (buffer-string)))))
        (puthash "verdict" (map-into `(("state" . "changes")
                                       ("head" . ,(diff-review--git "rev-parse" "feature"))
                                       ("by" . "agent") ("at" . "2026-11-07T10:02:00+13:00")
                                       ("round" . 2))
                                     '(hash-table :test equal))
                 data)
        (with-temp-file (buffer-file-name review) (insert (json-serialize data)))
        (revert-buffer)                 ; as auto-revert does
        (should (eq (diff-review--turn) 'author))
        (should (string-match-p "changes requested" (buffer-string)))
        (diff-review-approve)
        (let ((verdict (diff-review-test--verdict review)))
          (should (equal (alist-get 'state verdict) "approved"))
          (should (equal (alist-get 'round verdict) 2)))))))

(ert-deftest diff-review-test-verdict-needs-a-commit ()
  "A review of the worktree has no commit to give a verdict on."
  (diff-review-test--with-repo
    (diff-review-test--open repo)
    (should-error (diff-review-approve) :type 'user-error)
    (should-not (diff-review-test--verdict (current-buffer)))))

(ert-deftest diff-review-test-summary-functions ()
  "What `diff-review-summary-functions' return joins the summary, nil adding nothing."
  (diff-review-test--with-repo
    (let ((diff-review-summary-functions
           (list (lambda () nil) (lambda () (format "%d files" (length diff-review--files))))))
      (diff-review-test--open repo)
      (should (string-match-p "  2 files$" (diff-review--summary))))))

(provide 'diff-review-test)
;;; diff-review-test.el ends here
