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

(defun diff-review-test--inline-shown-p ()
  "Whether the selected window's file shows any removed lines inline."
  (seq-some (lambda (o) (overlay-get o 'before-string))
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
                    (when-let* ((s (overlay-get o 'before-string)))
                      (list (get-text-property 0 'face s)))))
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
                  (seq-some (lambda (o) (overlay-get o 'before-string)) diff-review--overlays)))
        (should-not (seq-some #'identity (diff-review-test--faces file)))))))

(ert-deftest diff-review-test-tint-leaves-the-hunk-bare ()
  "Tinting the removed lines of a plain file does not touch the hunk's strings."
  (let ((lines (list "old")))
    (diff-review--removed-string lines 'fundamental-mode 'diff-review-removed)
    (should-not (text-properties-at 0 (car lines)))))

(ert-deftest diff-review-test-glyph-falls-back ()
  (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
            ((symbol-function 'char-displayable-p) #'ignore))
    (should (equal (diff-review--glyph "←" "<-") "<-")))
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
    (should (equal (diff-review--glyph "←" "<-") "←"))))

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

(provide 'diff-review-test)
;;; diff-review-test.el ends here
