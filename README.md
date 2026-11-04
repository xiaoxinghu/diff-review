# diff-review

Review a git diff from Emacs the way a pull request page does. A review is a
small JSON file: visiting it shows the counts and the changed files as a tree;
picking a file shows the real file in its own major mode, so every editing
command, `xref` and the language server work. Overlays add what a diff shows:
removed lines inline above where they were, a tint on added lines, and a focus
mode that folds the unchanged lines. Files you mark viewed stay marked until
they change again.

A review compares the worktree with the merge base of a base branch, or two
commits. A sidebar shows the file tree beside the window the review shows in,
as its atomic partner, so the review can sit in any split and the tree stays
next to it.

## Install

```elisp
(use-package diff-review
  :vc (:url "https://github.com/xiaoxinghu/diff-review" :rev :newest))
```

Needs Emacs 29.1 and `magit-section`.

## Use

`M-x diff-review-find-file` visits a review file, creating it when it does not
exist: the repository holding the file, against its default branch. From Lisp,
`(diff-review-find-file FILE ROOT BASE HEAD)` reviews HEAD against BASE at ROOT,
for a commit range. Where the file lives is the caller's choice.

A review file:

```json
{"format": "diff-review/1",
 "root": "/path/to/worktree/",
 "base": "main",
 "head": "abc1234",
 "viewed": {"src/a.ts": "<blob hash>"}}
```

In the overview and the sidebar:

| Key   | Command                                  |
|-------|------------------------------------------|
| `RET` | show the file at point, or fold a folder |
| `n` `p` | next, previous file                    |
| `u`   | next unviewed file                       |
| `v`   | toggle the viewed mark                   |
| `g`   | rescan the diff                          |
| `t`   | show or hide the sidebar                 |
| `q`   | end the review                           |

In a file, `diff-review-file-mode` puts its commands under one prefix,
`diff-review-command-prefix`, `C-c _` by default, so the file's major mode
keeps its own `C-c` keys:

| Key         | Command                                |
|-------------|----------------------------------------|
| `C-c _ n` `C-c _ p` | next, previous hunk, across files |
| `C-c _ v`   | mark viewed and show the next unviewed |
| `C-c _ z`   | fold the unchanged lines               |
| `C-c _ d`   | show or hide the removed lines         |
| `C-c _ t`   | show or hide the sidebar               |
| `C-c _ l`   | back to the overview                   |
| `C-c _ q`   | end the review                         |

Set the prefix with `setopt` or Customize, or `setq` before loading; nil binds no keys at
all, for binding `diff-review-file-command-map` or the commands yourself.

With Evil, the lists start in normal state and `]]` `[[` walk the files; in a
file `]c` `[c` walk the hunks, `zv` marks viewed, `zf` folds, `zd` hides the
removed lines, `gl` goes to the overview and `gq` ends the review.

A file under review shows the review's header line, ending with key hints;
`diff-review-file-header` can leave the buffer's own header line alone or name
a function that makes one, and `diff-review-file-header-keys` drops the hints.
The buffer's header line comes back when the review ends. Hiding the removed
lines lasts for that review; `diff-review-inline-removed` is where each review
starts.

## Look

Everything drawn has a face in `M-x customize-group diff-review`, for a
theme to set. Added and removed lines take the background of `diff-added`
and `diff-removed` unless `diff-review-added` and `diff-review-removed` set
their own; `diff-review-tint-added` and `diff-review-tint-removed` turn the
tint off and leave the fringe bar. On a terminal that cannot draw `←`, `✓`
or `⋯`, ASCII stands in.

The lists leave `hl-line-mode` and the cursor as you have them. For a
highlighted line and no cursor:

```elisp
(add-hook 'diff-review-mode-hook #'hl-line-mode)
(add-hook 'diff-review-sidebar-mode-hook (lambda () (hl-line-mode) (setq cursor-type nil)))
```

The sidebar has no mode line, as its header line carries the summary;
`(kill-local-variable 'mode-line-format)` in `diff-review-sidebar-mode-hook`
gives it back.

## Check

`make check` byte-compiles without warnings, runs checkdoc and the ERT tests.
