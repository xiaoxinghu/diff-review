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
  (unless (zerop (apply #'call-process "git" nil nil nil
                        "-c" "user.name=t" "-c" "user.email=t@t" args))
    (error "git %s failed" (string-join args " "))))

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
                   (and (buffer-file-name buffer)
                        (string-prefix-p repo (buffer-file-name buffer))))
           (kill-buffer buffer)))
       (delete-other-windows)
       (switch-to-buffer "*scratch*")
       (delete-directory repo t))))

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
    (should-not (memq #'diff-review--follow window-buffer-change-functions))
    (let ((one (diff-review-test--open repo))
          (two (diff-review-test--open repo "head.json" "HEAD^" "HEAD")))
      (should (memq #'diff-review--follow window-buffer-change-functions))
      (with-current-buffer one (diff-review-quit))
      (should (memq #'diff-review--follow window-buffer-change-functions))
      (with-current-buffer two (diff-review-quit))
      (should-not (memq #'diff-review--follow window-buffer-change-functions)))))

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
  (diff-review-test--with-repo
    (delete-other-windows)
    (diff-review-test--open repo "head.json" "HEAD^")
    (should (equal (diff-review-test--bars (diff-review-test--file "a.txt"))
                   '((2 left-fringe diff-review-bar diff-indicator-removed)
                     (2 left-fringe diff-review-bar diff-indicator-added)
                     (4 left-fringe diff-review-bar diff-indicator-added)
                     (5 left-fringe diff-review-bar diff-indicator-added))))))

(ert-deftest diff-review-test-bars-in-the-right-fringe ()
  (diff-review-test--with-repo
    (delete-other-windows)
    (diff-review-test--open repo "head.json" "HEAD^")
    (let ((file (diff-review-test--file "a.txt"))
          (diff-review-fringe 'right))
      (with-current-buffer file (diff-review--decorate))
      (should (equal (delete-dups (mapcar #'cadr (diff-review-test--bars file)))
                     '(right-fringe))))))

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
                       (get-text-property y 'line-prefix)))))))

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
  (seq-find (lambda (o) (overlay-get o 'diff-review-comment)) diff-review--overlays))

(ert-deftest diff-review-test-comment-round-trip ()
  "A comment on a region, a reply, a resolve and a delete go through the file."
  (diff-review-test--with-repo
    (diff-review-test--at-a repo)
    (cl-letf (((symbol-function 'read-string-from-buffer) (lambda (_prompt _string) "Why?\n"))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (let ((transient-mark-mode t))
        (forward-line 1) (push-mark (point) t t) (forward-line 2)   ; lines 2 and 3
        (diff-review-comment))
      (should (= 1 (length diff-review--threads)))
      (let ((c (car (car diff-review--threads))))
        (should (equal (gethash "path" c) "a.txt"))
        (should (equal (gethash "lines" c) [2 3]))
        (should (equal (gethash "blob" c)
                       (diff-review--in-review (plist-get (diff-review--file) :hash))))
        (should (equal (gethash "body" c) "Why?"))
        (should (and (gethash "id" c) (gethash "author" c) (gethash "at" c))))
      (goto-char (point-min)) (forward-line 2)   ; line 3, inside the thread
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
      (should (null diff-review--threads))
      (should (equal (gethash "comments" (diff-review-test--review-json repo)) [])))))

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
    (should (equal (sort (mapcar (lambda (th) (cons (gethash "body" (car th)) (cdr th)))
                                 diff-review--threads)
                         (lambda (a b) (string< (car a) (car b))))
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
    (should (equal (mapcar #'cdr diff-review--threads) '((4 4 nil))))
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
      (diff-review--add-comment nil nil nil "Overall fine")
      (diff-review--add-comment "src/b.txt" '(1 . 1) nil "b?")
      (diff-review--add-comment "notes.md" '(2 . 2) nil "outside")
      (should (string-match-p "3 comments, 3 open" (buffer-string)))
      (goto-char (point-min))
      (search-forward "\nComments\n")
      (should (looking-at "  review  "))
      (diff-review-open-at-point)
      (should (eq (window-buffer) review))
      (forward-line 1)
      (should (looking-at "  src/b.txt:1  "))
      (diff-review-open-at-point)
      (should (diff-review-test--at "src/b.txt" 1))
      (should (with-current-buffer (window-buffer) diff-review--threads))
      (diff-review-overview)
      (with-current-buffer review
        (goto-char (point-min))
        (search-forward "  notes.md:2  ")
        (diff-review-open-at-point))
      (should (string-suffix-p "notes.md" (buffer-file-name (window-buffer))))
      (should (= 2 (with-current-buffer (window-buffer) (line-number-at-pos)))))))

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

(provide 'diff-review-test)
;;; diff-review-test.el ends here
