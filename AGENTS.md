# AGENTS.md

## Keep it small

- One file, `diff-review.el`, plus optional integration files, such as
  `diff-review-magit.el`, for what they integrate with. Only their entry
  points carry autoload cookies, and nothing in `diff-review.el` loads them.
  Forges share `diff-review-forge.el`, pull and submit, and each is a
  backend file, such as `diff-review-github.el`, of its two methods and an
  entry point; the forge file loads the backend the review file names.
  `diff-review-magit-mode` belongs in an init before magit loads, so that
  file requires only magit-section and edits magit once magit loads. Build
  what the task in hand asks for.
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
  `test/`. `diff-review.el` and its tests run with magit off the load path;
  `test/diff-review-magit-test.el` runs on its own, with magit, and the
  forge tests, `test/diff-review-{forge,github,gitlab}-test.el`, on theirs,
  each forge's CLI faked in Lisp. The tests build a git repository in a temp
  folder and drive windows in batch Emacs; a window change that the
  redisplay hook would notice is settled by calling `diff-review--follow`
  directly.
- `make deps`, which `check` runs first, fetches magit-section and magit from
  MELPA into `.deps/`; no Emacs config is read. CI runs `make check` on
  Emacs 29.1 and the latest release.

## Writing

Short. Comments say why; the code says what. A commit that closes a ws task
starts with the task id.
