# Hydra cross-generation bootstrap matrix

This page records, for every ordered pair of Hydra host languages, whether the **host** (left) can generate
the hydra-core kernel *into* the **target** (top) and pass the full hydra-core test suite there.

It is the durable, versioned answer to [#409](https://github.com/CategoricalData/hydra/issues/409) ("is Hydra
truly mutually self-hosting across every host pair?"): rather than a one-off full-matrix run, each cell
carries the date and git commit it was **last actually verified against**. Cells refresh opportunistically —
whenever a release's bootstrap-coverage pass happens to exercise a cell (the triad, plus whatever else that
release checks) — so a stale date is honest information, not a gap to hide. The data is produced by
`bin/run-bootstrapping-demo.sh` and merged in by `bin/bootstrap-matrix.py`; see [docs/demos.md](demos.md) and
[docs/release-workflow.md § Bootstrap coverage](release-workflow.md#bootstrap-coverage).

**Legend:** 🟢 generates + full test suite passes · 🔴 anything less · ⚪ never verified (no data yet).

## Status grid

| Host \ Target | Haskell | Java | Python | Scala | TypeScript | Clojure | Common Lisp | Scheme | Emacs Lisp |
|---|---|---|---|---|---|---|---|---|---|
| **Haskell** | 🟢 | 🟢 | 🟢 | ⚪ | ⚪ | ⚪ | ⚪ | 🟢 | ⚪ |
| **Java** | 🟢 | 🟢 | 🟢 | 🟢 | 🟢 | 🟢 | 🟢 | 🟢 | 🟢 |
| **Python** | 🟢 | 🟢 | 🟢 | 🟢 | 🟢 | 🟢 | 🟢 | 🟢 | 🟢 |
| **Scala** | ⚪ | 🔴 | 🟢 | ⚪ | ⚪ | ⚪ | ⚪ | ⚪ | ⚪ |
| **TypeScript** | ⚪ | ⚪ | ⚪ | ⚪ | ⚪ | ⚪ | ⚪ | ⚪ | ⚪ |
| **Clojure** | ⚪ | 🔴 | 🟢 | ⚪ | ⚪ | ⚪ | ⚪ | ⚪ | ⚪ |
| **Common Lisp** | ⚪ | 🔴 | 🟢 | ⚪ | ⚪ | ⚪ | ⚪ | ⚪ | ⚪ |
| **Scheme** | ⚪ | ⚪ | 🟢 | ⚪ | ⚪ | ⚪ | ⚪ | ⚪ | ⚪ |
| **Emacs Lisp** | ⚪ | ⚪ | 🟢 | ⚪ | ⚪ | ⚪ | ⚪ | ⚪ | ⚪ |

## Cell details

Each entry: codegen (files/time) · test-gen (files/time) · test-run time · pass count · last verified date @
commit. Red or unverified cells link the issue tracking them. Cells with no data and no tracking issue are
omitted.

| Cell | Status | Detail | Last verified |
|---|---|---|---|
| Haskell → Haskell | 🟢 | gen ?f/—<br>test-gen ?f/—<br>run —<br>3427 pass | 2026-09-30 @ `d0cbc9f634` |
| Haskell → Java | 🟢 | gen ?f/—<br>test-gen ?f/—<br>run —<br>3427 pass | 2026-09-30 @ `d0cbc9f634` |
| Haskell → Python | 🟢 | gen ?f/—<br>test-gen ?f/—<br>run —<br>3427 pass | 2026-09-30 @ `d0cbc9f634` |
| Haskell → Scheme | 🟢 | gen 173f/9.8s<br>test-gen 73f/9.8s<br>run 5.7s<br>3427 pass | 2026-09-30 @ `d0cbc9f634` |
| Java → Haskell | 🟢 | gen ?f/—<br>test-gen ?f/—<br>run —<br>3427 pass | 2026-09-30 @ `d0cbc9f634` |
| Java → Java | 🟢 | gen 3031f/1m5s<br>test-gen 79f/1m5s<br>run 8.8s<br>3656 pass | 2026-11-13 @ `fa1ff0fbb0` |
| Java → Python | 🟢 | gen 450f/54.4s<br>test-gen 82f/54.4s<br>run 15.7s<br>3656 pass | 2026-11-13 @ `fa1ff0fbb0` |
| Java → Scala | 🟢 | gen 254f/8.7s<br>test-gen 78f/6.6s<br>run 1m33s<br>5908 pass | 2026-10-30 @ `673631ab0d` |
| Java → TypeScript | 🟢 | gen 246f/46.9s<br>test-gen 77f/48.4s<br>run 24.5s<br>3539 pass | 2026-10-30 @ `673631ab0d` |
| Java → Clojure | 🟢 | gen 242f/5.4s<br>test-gen 75f/5.4s<br>run 7.7s<br>3427 pass | 2026-10-02 @ `d90d372a45` |
| Java → Common Lisp | 🟢 | gen 247f/5.2s<br>test-gen 77f/5.2s<br>run 1.1s<br>3427 pass | 2026-10-02 @ `d90d372a45` |
| Java → Scheme | 🟢 | gen 250f/5.1s<br>test-gen 80f/5.1s<br>run 5.8s<br>3427 pass | 2026-10-02 @ `d90d372a45` |
| Java → Emacs Lisp | 🟢 | gen 247f/12.4s<br>test-gen 76f/5.5s<br>run 3m1s<br>3539 pass | 2026-10-30 @ `2a7475a326` |
| Python → Haskell | 🟢 | gen ?f/—<br>test-gen ?f/—<br>run —<br>3427 pass | 2026-09-30 @ `d0cbc9f634` |
| Python → Java | 🟢 | gen 3031f/15m24s<br>test-gen 79f/15m24s<br>run 7.6s<br>3656 pass | 2026-11-13 @ `fa1ff0fbb0` |
| Python → Python | 🟢 | gen 450f/20m51s<br>test-gen 82f/20m51s<br>run 15.5s<br>3656 pass | 2026-11-13 @ `fa1ff0fbb0` |
| Python → Scala | 🟢 | gen 254f/2m37s<br>test-gen 78f/2m59s<br>run 1m30s<br>5913 pass | 2026-10-31 @ `923e9c83f1` |
| Python → TypeScript | 🟢 | gen 246f/7m41s<br>test-gen 77f/8m51s<br>run 25.2s<br>3544 pass | 2026-10-31 @ `923e9c83f1` |
| Python → Clojure | 🟢 | gen 278f/2m34s<br>test-gen 76f/2m54s<br>run 9.1s<br>3539 pass | 2026-10-30 @ `673631ab0d` |
| Python → Common Lisp | 🟢 | gen 283f/2m45s<br>test-gen 78f/2m57s<br>run 1.2s<br>3539 pass | 2026-10-30 @ `673631ab0d` |
| Python → Scheme | 🟢 | gen 286f/2m56s<br>test-gen 81f/3m7s<br>run 5.9s<br>3539 pass | 2026-10-30 @ `673631ab0d` |
| Python → Emacs Lisp | 🟢 | gen 247f/2m43s<br>test-gen 76f/2m52s<br>run 3m4s<br>3539 pass | 2026-10-30 @ `c2f7cf12e2` |
| Scala → Java | 🔴 [#788](https://github.com/CategoricalData/hydra/issues/788) | gen ?f/—<br>test-gen ?f/—<br>run —<br>— | 2026-10-03 @ `4476619c74` |
| Scala → Python | 🟢 | gen ?f/—<br>test-gen ?f/—<br>run —<br>3427 pass | 2026-09-30 @ `d0cbc9f634` |
| TypeScript → Java | ⚪ [#820](https://github.com/CategoricalData/hydra/issues/820) | never verified | — |
| Clojure → Java | 🔴 [#814](https://github.com/CategoricalData/hydra/issues/814) | gen ?f/—<br>test-gen ?f/—<br>run —<br>— | 2026-10-30 @ `18fd25bcbc` |
| Clojure → Python | 🟢 | gen 176f/2m50s<br>test-gen 74f/3m41s<br>run 11.6s<br>3544 pass | 2026-10-31 @ `923e9c83f1` |
| Common Lisp → Java | 🔴 [#814](https://github.com/CategoricalData/hydra/issues/814) | gen ?f/—<br>test-gen ?f/—<br>run —<br>— | 2026-10-30 @ `18fd25bcbc` |
| Common Lisp → Python | 🟢 | gen 176f/1m3s<br>test-gen 74f/1m1s<br>run 11.6s<br>3544 pass | 2026-10-31 @ `923e9c83f1` |
| Scheme → Python | 🟢 | gen 168f/10m38s<br>test-gen 74f/25m50s<br>run 12.1s<br>3544 pass | 2026-10-31 @ `923e9c83f1` |
| Emacs Lisp → Python | 🟢 | gen 168f/—<br>test-gen 69f/—<br>run 12.1s<br>3544 pass | 2026-10-31 @ `923e9c83f1` |

---

_This page is generated. To refresh a cell, run a bootstrap cell exercising it (`bin/run-bootstrapping-demo.sh
--hosts H --targets T`) and then `bin/bootstrap-matrix.py update --run <run_dir>`. Do not hand-edit the data
block below._

<!-- BOOTSTRAP-MATRIX-DATA
{
  "cells": {
    "clojure-to-java": {
      "mainFiles": null,
      "mainGenSeconds": null,
      "passed": null,
      "rawStatus": "fail",
      "skipped": null,
      "status": "red",
      "testFiles": null,
      "testGenSeconds": null,
      "testRunMs": null,
      "verifiedCommit": "18fd25bcbc",
      "verifiedDate": "2026-10-30"
    },
    "clojure-to-python": {
      "mainFiles": 176,
      "mainGenSeconds": 170.1,
      "passed": 3544,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 74,
      "testGenSeconds": 221.3,
      "testRunMs": 11643.4,
      "verifiedCommit": "923e9c83f1",
      "verifiedDate": "2026-10-31"
    },
    "common-lisp-to-java": {
      "mainFiles": null,
      "mainGenSeconds": null,
      "passed": null,
      "rawStatus": "fail",
      "skipped": null,
      "status": "red",
      "testFiles": null,
      "testGenSeconds": null,
      "testRunMs": null,
      "verifiedCommit": "18fd25bcbc",
      "verifiedDate": "2026-10-30"
    },
    "common-lisp-to-python": {
      "mainFiles": 176,
      "mainGenSeconds": 62.7,
      "passed": 3544,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 74,
      "testGenSeconds": 61.0,
      "testRunMs": 11608.5,
      "verifiedCommit": "923e9c83f1",
      "verifiedDate": "2026-10-31"
    },
    "emacs-lisp-to-python": {
      "mainFiles": 168,
      "mainGenSeconds": null,
      "passed": 3544,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 69,
      "testGenSeconds": null,
      "testRunMs": 12061.1,
      "verifiedCommit": "923e9c83f1",
      "verifiedDate": "2026-10-31"
    },
    "haskell-to-haskell": {
      "mainFiles": null,
      "mainGenSeconds": null,
      "passed": 3427,
      "rawStatus": "ok",
      "skipped": null,
      "status": "green",
      "testFiles": null,
      "testGenSeconds": null,
      "testRunMs": null,
      "verifiedCommit": "d0cbc9f634",
      "verifiedDate": "2026-09-30"
    },
    "haskell-to-java": {
      "mainFiles": null,
      "mainGenSeconds": null,
      "passed": 3427,
      "rawStatus": "ok",
      "skipped": null,
      "status": "green",
      "testFiles": null,
      "testGenSeconds": null,
      "testRunMs": null,
      "verifiedCommit": "d0cbc9f634",
      "verifiedDate": "2026-09-30"
    },
    "haskell-to-python": {
      "mainFiles": null,
      "mainGenSeconds": null,
      "passed": 3427,
      "rawStatus": "ok",
      "skipped": null,
      "status": "green",
      "testFiles": null,
      "testGenSeconds": null,
      "testRunMs": null,
      "verifiedCommit": "d0cbc9f634",
      "verifiedDate": "2026-09-30"
    },
    "haskell-to-scheme": {
      "mainFiles": 173,
      "mainGenSeconds": 9.8,
      "passed": 3427,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 73,
      "testGenSeconds": 9.8,
      "testRunMs": 5660,
      "verifiedCommit": "d0cbc9f634",
      "verifiedDate": "2026-09-30"
    },
    "java-to-clojure": {
      "mainFiles": 242,
      "mainGenSeconds": 5.4,
      "passed": 3427,
      "rawStatus": "diff",
      "skipped": 0,
      "status": "green",
      "testFiles": 75,
      "testGenSeconds": 5.4,
      "testRunMs": 7684,
      "verifiedCommit": "d90d372a45",
      "verifiedDate": "2026-10-02"
    },
    "java-to-common-lisp": {
      "mainFiles": 247,
      "mainGenSeconds": 5.2,
      "passed": 3427,
      "rawStatus": "diff",
      "skipped": 0,
      "status": "green",
      "testFiles": 77,
      "testGenSeconds": 5.2,
      "testRunMs": 1072,
      "verifiedCommit": "d90d372a45",
      "verifiedDate": "2026-10-02"
    },
    "java-to-emacs-lisp": {
      "mainFiles": 247,
      "mainGenSeconds": 12.4,
      "passed": 3539,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 76,
      "testGenSeconds": 5.5,
      "testRunMs": 180517,
      "verifiedCommit": "2a7475a326",
      "verifiedDate": "2026-10-30"
    },
    "java-to-haskell": {
      "mainFiles": null,
      "mainGenSeconds": null,
      "passed": 3427,
      "rawStatus": "ok",
      "skipped": null,
      "status": "green",
      "testFiles": null,
      "testGenSeconds": null,
      "testRunMs": null,
      "verifiedCommit": "d0cbc9f634",
      "verifiedDate": "2026-09-30"
    },
    "java-to-java": {
      "mainFiles": 3031,
      "mainGenSeconds": 64.8,
      "passed": 3656,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 79,
      "testGenSeconds": 64.8,
      "testRunMs": 8813.2,
      "verifiedCommit": "fa1ff0fbb0",
      "verifiedDate": "2026-11-13"
    },
    "java-to-python": {
      "mainFiles": 450,
      "mainGenSeconds": 54.4,
      "passed": 3656,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 82,
      "testGenSeconds": 54.4,
      "testRunMs": 15692.5,
      "verifiedCommit": "fa1ff0fbb0",
      "verifiedDate": "2026-11-13"
    },
    "java-to-scala": {
      "mainFiles": 254,
      "mainGenSeconds": 8.7,
      "passed": 5908,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 78,
      "testGenSeconds": 6.6,
      "testRunMs": 92711,
      "verifiedCommit": "673631ab0d",
      "verifiedDate": "2026-10-30"
    },
    "java-to-scheme": {
      "mainFiles": 250,
      "mainGenSeconds": 5.1,
      "passed": 3427,
      "rawStatus": "diff",
      "skipped": 0,
      "status": "green",
      "testFiles": 80,
      "testGenSeconds": 5.1,
      "testRunMs": 5850,
      "verifiedCommit": "d90d372a45",
      "verifiedDate": "2026-10-02"
    },
    "java-to-typescript": {
      "mainFiles": 246,
      "mainGenSeconds": 46.9,
      "passed": 3539,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 77,
      "testGenSeconds": 48.4,
      "testRunMs": 24472,
      "verifiedCommit": "673631ab0d",
      "verifiedDate": "2026-10-30"
    },
    "python-to-clojure": {
      "mainFiles": 278,
      "mainGenSeconds": 153.7,
      "passed": 3539,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 76,
      "testGenSeconds": 174.5,
      "testRunMs": 9089,
      "verifiedCommit": "673631ab0d",
      "verifiedDate": "2026-10-30"
    },
    "python-to-common-lisp": {
      "mainFiles": 283,
      "mainGenSeconds": 164.8,
      "passed": 3539,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 78,
      "testGenSeconds": 177.2,
      "testRunMs": 1216,
      "verifiedCommit": "673631ab0d",
      "verifiedDate": "2026-10-30"
    },
    "python-to-emacs-lisp": {
      "mainFiles": 247,
      "mainGenSeconds": 162.8,
      "passed": 3539,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 76,
      "testGenSeconds": 171.7,
      "testRunMs": 184110,
      "verifiedCommit": "c2f7cf12e2",
      "verifiedDate": "2026-10-30"
    },
    "python-to-haskell": {
      "mainFiles": null,
      "mainGenSeconds": null,
      "passed": 3427,
      "rawStatus": "ok",
      "skipped": null,
      "status": "green",
      "testFiles": null,
      "testGenSeconds": null,
      "testRunMs": null,
      "verifiedCommit": "d0cbc9f634",
      "verifiedDate": "2026-09-30"
    },
    "python-to-java": {
      "mainFiles": 3031,
      "mainGenSeconds": 924.3,
      "passed": 3656,
      "rawStatus": "diff",
      "skipped": 0,
      "status": "green",
      "testFiles": 79,
      "testGenSeconds": 924.3,
      "testRunMs": 7641.2,
      "verifiedCommit": "fa1ff0fbb0",
      "verifiedDate": "2026-11-13"
    },
    "python-to-python": {
      "mainFiles": 450,
      "mainGenSeconds": 1251.4,
      "passed": 3656,
      "rawStatus": "diff",
      "skipped": 0,
      "status": "green",
      "testFiles": 82,
      "testGenSeconds": 1251.4,
      "testRunMs": 15463.6,
      "verifiedCommit": "fa1ff0fbb0",
      "verifiedDate": "2026-11-13"
    },
    "python-to-scala": {
      "mainFiles": 254,
      "mainGenSeconds": 156.8,
      "passed": 5913,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 78,
      "testGenSeconds": 179.0,
      "testRunMs": 90382,
      "verifiedCommit": "923e9c83f1",
      "verifiedDate": "2026-10-31"
    },
    "python-to-scheme": {
      "mainFiles": 286,
      "mainGenSeconds": 175.6,
      "passed": 3539,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 81,
      "testGenSeconds": 186.6,
      "testRunMs": 5901,
      "verifiedCommit": "673631ab0d",
      "verifiedDate": "2026-10-30"
    },
    "python-to-typescript": {
      "mainFiles": 246,
      "mainGenSeconds": 461.4,
      "passed": 3544,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 77,
      "testGenSeconds": 531.4,
      "testRunMs": 25218,
      "verifiedCommit": "923e9c83f1",
      "verifiedDate": "2026-10-31"
    },
    "scala-to-java": {
      "mainFiles": null,
      "mainGenSeconds": null,
      "passed": null,
      "rawStatus": "fail",
      "skipped": null,
      "status": "red",
      "testFiles": null,
      "testGenSeconds": null,
      "testRunMs": null,
      "verifiedCommit": "4476619c74",
      "verifiedDate": "2026-10-03"
    },
    "scala-to-python": {
      "mainFiles": null,
      "mainGenSeconds": null,
      "passed": 3427,
      "rawStatus": "ok",
      "skipped": null,
      "status": "green",
      "testFiles": null,
      "testGenSeconds": null,
      "testRunMs": null,
      "verifiedCommit": "d0cbc9f634",
      "verifiedDate": "2026-09-30"
    },
    "scheme-to-python": {
      "mainFiles": 168,
      "mainGenSeconds": 638.2,
      "passed": 3544,
      "rawStatus": "pass",
      "skipped": 0,
      "status": "green",
      "testFiles": 74,
      "testGenSeconds": 1549.8,
      "testRunMs": 12058.2,
      "verifiedCommit": "923e9c83f1",
      "verifiedDate": "2026-10-31"
    }
  },
  "issues": {
    "clojure-to-java": 814,
    "common-lisp-to-java": 814,
    "scala-to-java": 788,
    "typescript-to-java": 820
  }
}
BOOTSTRAP-MATRIX-DATA-END -->
