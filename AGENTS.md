# AGENTS.md

## Keep it small

- One file, `diff-review.el`. Build what the task in hand asks for; leave out
  the option nobody set. YAGNI.
- A review is an ordinary buffer: where it opens is the user's display rules'
  business, and `q` or killing it gives every window back what it showed before.
  Keep it that way.
- Git is the source of truth. The review file records only what git cannot
  derive: the base, and the viewed marks with their blob hashes.
- Prefer the diff that removes a path over the one that adds a branch.

## Checks

- `make check`: byte-compile without warnings, checkdoc, and the ERT tests in
  `test/`. The tests build a git repository in a temp folder and drive windows
  in batch Emacs; a window change that the redisplay hook would notice is
  settled by calling `diff-review--follow` directly.

## Writing

Short. Comments say why; the code says what. A commit that closes a ws task
starts with the task id.
