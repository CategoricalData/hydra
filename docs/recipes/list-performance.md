# List representation and indexed-access performance

`hydra.lib.lists.map` (and most other `hydra.lib.lists.*` primitives) return the kernel's abstract
`list<x>` type. The DSL signature does not mandate a concrete backing structure — each host's coder
independently chooses how to represent it. As of this writing, that choice is not uniform across
hosts, and on some hosts repeated indexed access (`at(i, xs)`, or a host-native `get`/`nth`/
`list-ref` called in a loop) is quadratic rather than linear. This recipe documents which hosts are
affected and the workaround.

## Why this matters

A loop like "for `i` from 0 to `length(xs)`, look up `xs[i]`" is a natural, type-correct way to
traverse a list in every host. On hosts where the list is backed by a linked (cons) structure,
each lookup is O(i), so the loop as a whole is O(n²) — with nothing at the type level to signal
the cost. This is easy to hit by accident after a `map`/`filter`/`sort` (or any other primitive
that returns a fresh list) followed by indexed access, and the slowdown only becomes visible at
realistic data sizes.

## Which hosts are affected

| Host | List representation | Indexed access |
|------|---------------------|-----------------|
| Java | `hydra.overlay.java.util.ConsList` — a singly-linked cons-list (implements `java.util.List`, so it type-checks as one, but `get(i)` walks `i` cells) | O(i) — **quadratic in a loop** |
| Clojure | native Clojure seq / lazy-seq; `at` uses `nth` | O(i) — **quadratic in a loop** |
| Common Lisp | native cons-list; `at` uses `nth` | O(i) — **quadratic in a loop** |
| Emacs Lisp | native cons-list; `at` uses `nth` | O(i) — **quadratic in a loop** |
| Scheme | native cons-list (R7RS pairs); `at` uses `list-ref` | O(i) — **quadratic in a loop** |
| Python | `hydra.overlay.python.util.ConsList` — backed internally by a native `tuple` | O(1) |
| Scala | native `Seq[A]` | O(1) |
| TypeScript | native JS array | O(1) |
| Haskell | native `[a]` | O(i) by construction — idiomatic for the language; Haskell code that needs random access already reaches for `Data.Sequence`/`Data.Vector` instead of indexing `[a]` |

The Lisp-dialect hosts (Clojure, Common Lisp, Emacs Lisp, Scheme) return a *native* language list,
not a distinguishable Hydra wrapper type — so there is no separate class to notice or convert away
from. This makes the cost less discoverable there than in Java, where `ConsList` at least names the
representation.

## Workaround

On an affected host, materialize the list into a random-access structure once before an indexed
loop, rather than indexing the list repeatedly:

- **Java**: `new ArrayList<>(consList)` (or call `.toArrayList()` on a `ConsList`), then index the
  copy.
- **Clojure**: `(vec xs)`, then index with `nth`/`get` on the vector (O(1) there).
- **Common Lisp / Emacs Lisp**: `(coerce xs 'vector)`, then `(aref v i)`.
- **Scheme**: `(list->vector xs)`, then `(vector-ref v i)`.

Python's `ConsList` needs no such workaround — indexed access is already O(1) by construction (see
its docstring in `overlay/python/hydra-kernel/.../util/cons_list.py` for the rationale: tuple-backed
storage trades an O(n) `cons`/prepend for O(1) indexing, judged the better tradeoff for
codegen-heavy workloads). Scala and TypeScript are unaffected for the same reason (native
array-backed sequences).

If you only need a single lookup rather than a loop, `hydra.lib.lists.at(i, xs)` is fine as-is on
every host — the cost only compounds when called repeatedly over an increasing (or any) range of
indices against the same list.

## Related

- [Generating code from your own Hydra modules](downstream-codegen.md) — downstream-project codegen
  entry points; this recipe covers a runtime characteristic of the generated code's output rather
  than the codegen pipeline itself.
- [GitHub issue #651](https://github.com/CategoricalData/hydra/issues/651) — the report that
  surfaced this; also tracks the open question of whether to change any host's list representation
  to close the gap (a larger, per-host, breaking-change design decision, separate from this
  documentation fix).
