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

In a file, `diff-review-file-mode` adds:

| Key       | Command                                  |
|-----------|------------------------------------------|
| `C-c C-n` `C-c C-p` | next, previous hunk, across files |
| `C-c C-v` | mark viewed and show the next unviewed   |
| `C-c C-z` | fold the unchanged lines                 |
| `C-c C-d` | show or hide the removed lines           |
| `C-c C-l` | back to the overview                     |
| `C-c C-q` | end the review                           |

With Evil, the lists start in normal state and `]]` `[[` walk the files; in a
file `]c` `[c` walk the hunks, `zv` marks viewed, `zf` folds, `zd` hides the
removed lines, `gl` goes to the overview and `gq` ends the review.

## Check

`make check` byte-compiles without warnings, runs checkdoc and the ERT tests.
