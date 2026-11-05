# AGENTS.md

## Keep it small

- One file, `diff-review.el`. Build what the task in hand asks for.
- No assumptions about the user's keys, display modes, faces or git setup:
  where the package would decide one of those, a defcustom or defface decides,
  with a conventional default. Everything else stays minimal: no option for a
  behaviour nobody has asked to change.
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
- `make deps`, which `check` runs first, fetches magit-section from MELPA into
  `.deps/`; no Emacs config is read. CI runs `make check` on Emacs 29.1 and
  the latest release.

## Writing

Short. Comments say why; the code says what. A commit that closes a ws task
starts with the task id.
