---
description: Run bin/sync-emacs-lisp.sh — Emacs-Lisp-host Emacs-Lisp sync. One of four Lisp-dialect entry points; shares the hydra-lisp coder.
allowed-tools:
  - Bash(bin/sync-emacs-lisp.sh*)
  - Read
  - Bash(git status*)
---

# Emacs-Lisp-host Emacs-Lisp sync

## Procedure

```bash
bin/sync-emacs-lisp.sh
```

Note: the Emacs Lisp regex matcher needs `case-fold-search` bound to
nil — see `claude/pitfalls.md` for the gotcha.
