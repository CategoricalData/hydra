<!-- [PENDING #579]: spec-standard version/status header, per index.md §4 format (not yet defined). -->
<!-- NOTE: this file will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/*.hs by a generator script
     (sibling of bin/regenerate-lexicon.sh; not yet built).
     This draft is hand-authored under #417 to fix the format the generator must produce.
     Until the generator exists, treat content as provisional and the format as the deliverable.
     The specifications on this page are the normative TARGET: they may (and often deliberately
     do) diverge from the currently-frozen primitive definitions; the definitions will be
     rewritten to match this page when #417's implementation phase begins. -->

# Hydra primitives

This is the complete catalog of Hydra's primitive functions: the `hydra.lib.*` namespace.
Every primitive is listed here, from every module, regardless of maturity; a primitive's lifecycle
status is carried by its badge (see [Lifecycle and badges](#lifecycle-and-badges)), never by its
presence or absence on this page.

A primitive is a named, typed function that every conforming Hydra implementation provides natively.
Each primitive is defined once, translingually, with a name, a type signature, a prose specification,
purity/totality/laziness metadata, and (where applicable) a default implementation in terms of other
primitives.
Each entry fully defines its primitive's semantics on its own terms; no external language or
library is referenced normatively.
Where shared semantic ground is needed (structural equality, the total order over values,
floating-point behavior), it is defined once in a supplementary specification and linked.

For the flat names-and-signatures view of the same definitions, see [lexicon.txt](../lexicon.txt).
For the conformance meaning of this catalog, see the specification index.
<!-- [PENDING #579]: link target for the conformance statement (index.md §3). -->

## Conventions

### Signature notation

Signatures are written in Hydra's normative textual type syntax (`hydra.show.core`, becoming
`hydra.print.core`; the syntax may evolve, and this page tracks it):
type schemes as `∀t1,t2. (class t1) ⇒ <body>`; function types arrow-joined,
`t1 → t2 → t3`; container types in angle brackets, `list<T>`, `optional<T>`, `either<L, R>`,
`map<K, V>`, `set<T>`, `pair<A, B>`, `effect<T>`; literal types lowercase (`string`, `int32`,
`float64`, `boolean`, `binary`); named types in PascalCase (`Comparison`); nominal type
application as `F @ A`.
Parentheses are used only where needed for grouping (e.g. a function-typed argument,
`(t1 → t2) → list<t1> → list<t2>`).
Type variables are named `t`, or `t1`, `t2`, ... when several are needed (`k`/`v` for map keys
and values).
Each entry also gives a usage form — the primitive applied to named arguments by juxtaposition,
`equal x y` — and the prose refers to arguments by those names.
Within its own module's section a primitive is named bare (`compare`); references to another
module's primitive are fully qualified (`hydra.lib.equality.equal`).
The argument names are part of the specification.
<!-- [PENDING #579]: syntax.md will carry the grammar this section summarizes; keep unified. -->

### Modules and homing

A primitive lives in the module of its semantic subject — usually, but not always, its primary
(dispatched-on) operand.
Name reuse across modules is deliberate: `map`, `bind`, `filter`, `member`, and others appear in
several modules, and the module qualifier is part of the name.
A shared name is backed by a type class only when its cross-constructor semantics is lawful
(see [Type classes and constraints](#type-classes-and-constraints)); otherwise it is an
intentionally overloaded identifier.

### Type classes and constraints

Five type-class constraints appear in primitive signatures: `equality`, `ordering`,
`numeric`, `integral`, and `fractional`.
Constraints are part of the type scheme; Hydra propagates them during inference.
The classes themselves — their members, instances, and laws — are specified in
[classes.md](../classes.md).

Ordering on keys and elements is part of the map and set type contract: Hydra maps and sets are
ordered collections (iteration is in ascending key/element order), so every map- or set-typed
variable in a primitive signature uniformly carries the `ordering` constraint, including in
primitives that perform no comparison themselves.

### Parameter order

1. Function and configuration arguments come first; the principal data argument comes last
   (`lists.map f xs`, `lists.foldl f acc xs`, `maps.insert k v m`).
2. Eliminators take the scrutinee first (`optionals.cases m default f`, `eithers.bind m f`).

Documented exceptions (`math.atan2 y x`, `math.logBase b x`, default-first
`optionals.withDefault default m`) are noted in their entries.

### Totality

Every primitive is total: defined and terminating on every input of its declared type.
Partial operations express their partiality in the codomain, returning `optional` or `either`
values rather than failing.
(If an exception ever exists, its entry carries a `Partial:` marker with an explanation.)

### Sizes and indices

Collection size and index parameters and results default to `int32`.
A container whose size exceeds the `int32` range is an implementation capacity limit — like
memory exhaustion, outside the specified domain — not a semantic case primitives must handle.
Primitives and types whose domains inherently require larger sizes (file byte sizes, for
example) use `int64` explicitly.
The selection principle for new primitives: favor `int32` when it is large enough for any
conceivable real-world application; if there is any chance of overflow, use `int64`.

### Purity and effects

Primitives are pure — referentially transparent, with no observable side effects — unless the
entry says otherwise.
An `Effectful:` line, stating what the effect touches, appears on each impure primitive,
e.g. `Effectful: reads from the file system.`
Effectful primitives describe computations using `effect` types in their signatures; execution
is separate from description.

### Strictness and laziness

Primitives are strict in every argument unless the entry says otherwise.
A `Lazy:` line, naming arguments from the usage form, appears only on primitives with at least
one lazy argument, e.g. ``Lazy: `ifTrue`, `ifFalse` — only the selected branch is evaluated.``

### Arity suffixes

A trailing digit in a primitive name denotes arity, and nothing else: `concat2` is the binary
form of `concat`; `atan2` is the two-argument arctangent.

### Verb categories

- `parse` / `print`: pure conversion between text and structured values.
- `read` / `write`: input/output against an external resource (impure; `files`).
- `get*`: query ambient system state (impure; `system`).

### Floating-point values

Hydra's floating-point value space is the wire format's: IEEE 754 binary32/binary64 values with a
single NaN (payloads are not distinguished) and with negative zero distinct from positive zero.
`hydra.lib.equality.equal` on floats is value identity: `equal NaN NaN` is `true`, and
`equal -0.0 0.0` is `false`.
The `ordering` constraint on floats denotes the extended total order: NaN compares greater than
every other value and equal to itself, and `-0.0` compares less than `0.0`; consequently
`compare x y` is `equalTo` exactly when `equal x y` is `true`
(see [ordering-and-equality.md](../ordering-and-equality.md)).
IEEE 754's own comparison predicates (under which NaN is not equal to itself) are deliberately
not provided.
Warning: the idiom of testing a value against itself with `equal` to detect NaN does not work in
Hydra (the result is always `true`); `math.isNaN` is the designated future addition for
IEEE-style predicates.
Arithmetic follows IEEE 754: required operations are correctly rounded and bit-identical across
hosts; transcendental functions are faithful to the host's math library and may differ across
hosts in the final unit of precision.

### Lifecycle and badges

Each primitive carries one badge:

- **Draft** — implemented and listed, but its specification may still change.
- **Canonical** — specification pinned; the primitive will never be renamed, removed, or changed
  in behavior. Conforming implementations must provide exactly the specified semantics.
- **Deprecated** — retained for compatibility; the entry names its replacement. Deprecated
  primitives remain available until a stated version.

`Since:` gives the version in which the primitive was added; deprecated entries add
`Deprecated since:` and `Use:`.
Canonization changes a badge; it never adds or removes a row.
<!-- [PENDING #579]: final badge word list to be defined once in the spec index and shared. -->

---

## Modules

One page per module, in this directory:

| Module | Contents |
|---|---|
| [hydra.lib.chars](chars.md) | Unicode code-point predicates and case mapping |
| [hydra.lib.effects](effects.md) | the effect monad: sequencing and traversal (impure) |
| [hydra.lib.eithers](eithers.md) | the either type: construction, elimination, traversal |
| [hydra.lib.equality](equality.md) | equality of values |
| [hydra.lib.files](files.md) | filesystem input/output (impure) |
| [hydra.lib.functions](functions.md) | function combinators |
| [hydra.lib.hashing](hashing.md) | cryptographic hashing |
| [hydra.lib.lists](lists.md) | list construction, transformation, and folding |
| [hydra.lib.literals](literals.md) | conversion and parse/print for literal values |
| [hydra.lib.logic](logic.md) | boolean operations and branching |
| [hydra.lib.maps](maps.md) | ordered maps |
| [hydra.lib.math](math.md) | arithmetic and numeric functions |
| [hydra.lib.optionals](optionals.md) | the optional type |
| [hydra.lib.ordering](ordering.md) | ordering of values |
| [hydra.lib.pairs](pairs.md) | pairs |
| [hydra.lib.regex](regex.md) | regular expressions (syntax: [regex.md](../regex.md)) |
| [hydra.lib.sets](sets.md) | ordered sets |
| [hydra.lib.strings](strings.md) | strings as sequences of code points |
| [hydra.lib.system](system.md) | process and environment access (impure) |
| [hydra.lib.text](text.md) | text encoding |

<!-- FORMAT NOTE: equality.md, ordering.md, and logic.md are written as format exemplars;
     remaining module pages follow after format review (#417). -->
