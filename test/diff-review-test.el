;;; diff-review-test.el --- Tests for diff-review  -*- lexical-binding: t; -*-

;;; Commentary:

;; Each test builds a git repository in a temp folder, with a commit and a
;; dirty worktree, and drives windows in the batch frame.  Batch Emacs never
;; redisplays, so `diff-review--follow', which the redisplay hook runs,
;; is called where a window change would have triggered it.

;;; Code:

(require 'ert)
(require 'diff-review)

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

(provide 'diff-review-test)
;;; diff-review-test.el ends here
