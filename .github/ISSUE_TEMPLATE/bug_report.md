---
name: Bug report
about: Something in Hydra doesn't behave as documented or expected
title: ''
labels: bug
assignees: ''
---

## Summary

A clear, one-or-two-sentence statement of the problem.

## Affected host / target language(s)

Hydra spans several implementations. Which apply?
(e.g. Haskell, Java, Python, Scala, TypeScript, Clojure, Common Lisp, Emacs Lisp, Scheme, Go —
or "translingual" if the problem is in the kernel/DSL itself.)

## Steps to reproduce

1. ...
2. ...

Include a minimal code sample or failing command where possible.
If the problem involves code generation, note whether you ran `bin/sync.sh` (or a scoped
wrapper) and from which directory.

## Expected behavior

What you expected to happen.

## Actual behavior

What happened instead. Include the relevant output or stack trace (trimmed to the interesting part).

## Environment

- Hydra version (release, or commit hash if building from source):
- OS:
- Toolchain versions if relevant (GHC/Stack, JDK/Gradle, Python, sbt, Node, ...):

## Notes

Anything else that might help: workarounds you found, related issues, whether the
problem also occurs in another host language.
