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
next to it. Drag its edge or use `C-x {` `C-x }` to resize it; the review
keeps that width, through `balance-windows` and hiding the sidebar.
`diff-review-sidebar-width` is where each review starts.

## Install

```elisp
(use-package diff-review
  :vc (:url "https://github.com/xiaoxinghu/diff-review" :rev :newest))
```

Needs Emacs 29.1 and `magit-section`.

## Use

`M-x diff-review-find-file` visits a review file, creating it when it does not
exist: the repository holding the file, against `diff-review-default-base`. That
is a revision, or a function called at the repository's root; the default,
`diff-review-default-branch`, takes what `origin/HEAD` points at, else the first
of `init.defaultBranch`, `main` and `master` that exists. The file records the
base it got, so changing the setting leaves existing reviews alone. From Lisp,
`(diff-review-find-file FILE ROOT BASE HEAD)` reviews HEAD against BASE at ROOT,
for a commit range. Where the file lives is the caller's choice.

`M-x diff-review-branch` reviews a branch the way a pull request page does:
pick one of the branches the repository's other worktrees have checked out,
or type any, and the review of its commits against the base opens. Its file
is `review/<branch>.json` under the common git directory, the `.git` every
worktree shares, a slash in the name a folder, so main's clone and the
branch's worktree open the same file and git never commits it: one file per
branch, kept as the branch moves. `head` is the branch name, so the review
follows it: after a commit on the branch, `g` rescans, viewed marks clear
for the files that changed, and threads follow their lines or read
outdated. The overview says when the branch has moved past what it shows.
The review is of the branch's commits, never a worktree's uncommitted
edits.

A review file:

```json
{"format": "diff-review/1",
 "root": "/path/to/worktree/",
 "base": "main",
 "head": "feature/login",
 "viewed": {"src/a.ts": "<blob hash>"}}
```

`head` is a branch, which the review follows, or a commit for a fixed
range; without it the review is of the worktree. The files of a review
with a `head` are real files too. One that a worktree has checked out at
that commit, the main checkout included, and has not edited opens there,
so `xref`, eglot and project.el see an ordinary file in its tree; an edited
one shows the commit's version in a read-only buffer, as its hunks would
not line up. Commits no worktree has are checked out under
`review/worktrees/<hash>` in the common git directory, a throwaway worktree
the review removes when it ends, and their files open there read-only. It
is one review's, locked for the Emacs that added it; a second review of
the same commit meanwhile shows read-only buffers of the files instead,
and one a killed Emacs left goes with the next review that needs one.
Ignored files are not in it, so dependencies such as `node_modules` are
absent: relative imports resolve, package imports read as unresolved. A
deleted file shows its base version.

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
| `C-c _ c`   | comment on the region's lines, or the line at point |
| `C-c _ r`   | reply to the thread at point           |
| `C-c _ x`   | resolve the thread at point, or reopen it |
| `C-c _ D`   | delete the thread at point             |
| `C-c _ o`   | open every thread, or only the one at point again |
| `C-c _ N` `C-c _ P` | next, previous thread, across files |

`C-c _` is the default because Emacs leaves a package nothing handier: `C-c`
and a letter, and F5 to F9, are yours, and org takes the rest of `C-c` and
punctuation. So move it to one of yours, `(setopt diff-review-command-prefix
"C-c r")` or `"<f6>"`, with Customize, or `setq` before loading; nil binds no
keys at all, for binding `diff-review-file-command-map` or the commands
yourself. With `repeat-mode` on, the prefix is typed once: `C-c _ n n v`
walks two hunks and marks the file viewed. Hunks also answer to `next-error`,
so `M-g n` and `M-g p` walk them, across files, with no prefix.

The full list is Emacs's own help: `C-c _ C-h` lists the file commands, `C-h m`
the whole mode, and outside Evil `?` does the same in the overview and the sidebar.
With `which-key-mode`, built in since Emacs 30, pausing after `C-c _` shows
its keys.

With Evil, the lists start in normal state. Normal state takes `n`, `p`, `g`
and `TAB`, so there `gj` `gk` walk the files, `gr` rescans and `TAB` folds a
folder; `RET`, `u`, `v`, `t` and `q` are as above. In a file the prefix keys
work in normal state, and nothing else is bound. Set `diff-review-evil-keys`
to nil before loading to leave Evil untouched: no initial state, no keys.

For vim keys in a file, paste this; it shadows `gq` (`evil-fill-and-move`), and
`zf` `zd` `gl` where evil-lion, vimish-fold or origami bind them:

```elisp
(with-eval-after-load 'evil
  (evil-define-minor-mode-key 'normal 'diff-review-file-mode
    "]c" #'diff-review-next-hunk
    "[c" #'diff-review-previous-hunk
    "zv" #'diff-review-viewed-and-next
    "zf" #'diff-review-toggle-focus
    "zd" #'diff-review-toggle-inline-removed
    "gl" #'diff-review-overview
    "gq" #'diff-review-quit))
```

A file under review shows the review's header line, ending with key hints.
They are the keys you have bound, so with the snippet above they read `] c`
and `z v`; a prefix all the keys share shows once. `diff-review-file-header`
can leave the buffer's own header line alone or name
a function that makes one, and `diff-review-file-header-keys` drops the hints.
The buffer's header line comes back when the review ends. Hiding the removed
lines lasts for that review; `diff-review-inline-removed` is where each review
starts.

## Comments

A review takes comments the way a pull request page does: a thread on some
lines of a file, with replies, resolved or not. `C-c _ c` on a region, or on a
line, opens a buffer for the body; `C-c C-c` posts it and the thread draws
under the lines, in the file's comment colour with a tint behind it, a bar
in the fringe joining it to its lines. The thread point is on, or just
under, is open, its lines tinted; the others are one line each, so a file
with many long comments still reads, and `C-c _ o` opens them all. Several
threads on the same lines open together, and a key that needs one asks
which. The lines need not be
changed ones. Threads live in the
review file, so they persist, and the overview lists them under the tree,
`RET` going to each. A comment with no location is on the review as a whole.

```json
{"format": "diff-review/1",
 "base": "main",
 "viewed": {},
 "comments": [
   {"id": "k3f9a1",
    "path": "src/a.ts",
    "lines": [12, 15],
    "blob": "<blob hash of src/a.ts when this was written>",
    "author": "xiaoxing",
    "at": "2026-11-06T12:40:11+13:00",
    "body": "This retries forever when the socket is gone.",
    "replies": [
      {"author": "claude", "at": "2026-11-06T12:58:02+13:00", "body": "Capped at 5 in 3b1c0e2."}],
    "resolved": true},
   {"id": "77c02d", "author": "review", "at": "2026-11-06T13:01:40+13:00",
    "body": "Overall: correct, one nit above."}]}
```

The file is for agents as much as for Emacs, so it is plain: one array, one
object per thread, the first message carrying the location and `replies` a
flat list of `{author, at, body}`. `id` is any string unique among the
comments. `lines` is first and last, inclusive, counted in the file content
`blob` names, so a thread follows its lines when the file changes, through
`git diff` of the two blobs, and says `outdated` when a change landed inside
them. Without `blob` the lines count in the file as it is now; Emacs always
writes it. `path` is relative to the root and may name a file outside the
diff: that file joins the review's tree as unchanged, so its threads are
reached like any other.
Keys Emacs does not know are kept as they are, in a comment too, which is
where an agent puts a severity or a confidence.

To write a comment as an agent: the blob is `git hash-object -w <path>` in a
worktree review, `git rev-parse <head>:<path>` for a review with a `head`, a
branch or a commit; append one object. The `-w` stores the blob, which a worktree file's is not until it
is committed, so git can diff it once the file moves on; without it the thread
reads as outdated the moment the file changes. To reply, push to the thread's `replies`; to mark it fixed, add
`"resolved": true`. Emacs rereads the file before each of its own writes and
whenever it changes on disk, so writing beside an open review loses nothing.
With `jq`:

```sh
jq --arg blob "$(git hash-object -w src/a.ts)" --arg at "$(date -Iseconds)" \
  '.comments += [{id: "a1b2c3", path: "src/a.ts", lines: [12, 15], blob: $blob,
                  author: "claude", at: $at, body: "Retries forever.", replies: []}]' \
  review.json > review.json.new && mv review.json.new review.json
```

The author is `diff-review-author`, by default the repository's `user.name`.
The time is RFC 3339 with the local offset. The body is plain text; nothing
in it is rendered as markup.

## Look

Everything drawn has a face in `M-x customize-group diff-review`, for a
theme to set: a thread is `diff-review-comment`, its lines while it is open
`diff-review-comment-range`, its authors `diff-review-comment-author`. Added and removed lines take the background of `diff-added`
and `diff-removed` unless `diff-review-added` and `diff-review-removed` set
their own; `diff-review-tint-added` and `diff-review-tint-removed` turn the
tint off and leave the fringe bar. The bars go in the left fringe;
`diff-review-fringe` moves them to the right, or to nowhere with nil. They
are overlay strings, not `line-prefix`, so `org-indent-mode` keeps its
indentation. On a terminal that cannot draw `←`, `✓` or `⋯`, ASCII stands in.

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
It reads no Emacs config: the first run fetches magit-section from MELPA into
`.deps/` (`make deps`), and `rm -rf .deps` starts over. `EMACS=...` picks the
Emacs.
