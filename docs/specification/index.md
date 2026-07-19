# The Hydra specification

**Status: Draft** · Version: tracks the repository release (see §5)

This is the specification of Hydra: a functional programming language based on the
LambdaGraph data model, in which programs are graphs and graphs are programs.
It is the normative, implementer-facing reference for the language —
the data model, the textual syntax, the type system, validation, the standard primitive
library, and the interchange formats.
It is distinct from Hydra's explanatory documentation:
the [wiki](https://github.com/CategoricalData/hydra/wiki) explains concepts and design
rationale, the `docs/` tree documents the implementation and its build system,
and this specification states what a conforming implementation MUST do.

The key words MUST, MUST NOT, SHOULD, and MAY are to be interpreted as described in RFC 2119.

## 1. Scope

This specification defines:

- the core data model (`hydra.core`): the complete grammar of Hydra terms and types;
- the textual syntax for terms, types, and their dependencies;
- the constraint classes and the canonical equality and ordering of values;
- the type system: inference, elaboration, checking, and unification;
- validation: the well-formedness constraints on terms, types, modules, and packages;
- the standard primitive library (`hydra.lib.*`);
- the JSON interchange format.

It does not define: the host-language authoring DSLs, the build and code-generation
system, or the internal architecture of any implementation.
The [LambdaGraph paper](https://github.com/CategoricalData/hydra/issues/197) is the
formal treatment of the underlying calculus; this specification is the implementer-facing
counterpart, and the two cite each other.

## 2. Conventions

- **Notation**: terms and types appear in the Hydra textual syntax ([syntax.md](syntax.md)).
  Inference rules use the judgment forms defined in the type-system chapter.
- **Names**: fully qualified, dotted (`hydra.core.Term`).
- **Generated and hand-written pages**: catalog pages (per-module reference
  documentation, including the primitive catalog) are generated from the package's
  post-inference module content; chapters like this one are hand-written and
  cross-link into them. Generated pages carry a generated-file notice.

## 3. Conformance

**A conforming Hydra implementation passes the hydra-kernel test suite**
(`hydra.test.testSuite`) — the translingual test suite that accompanies Hydra's kernel.
Other packages carry their own test suites; hydra-kernel is the definitive package for a
Hydra implementation, and its suite is the conformance criterion.
The suite exercises primitive semantics, formatting and serialization (including the
canonical textual rendering), type-inference results, expected inference failures, and
effectful cases.

- Two conformance modes correspond to the suite's two derivations:
  **kernel (interpreter) conformance** — the implementation evaluates translated kernel
  terms directly — and **generation (compiler) conformance** — the implementation
  compiles Hydra modules to native code which then passes the same tests.
- The parity guarantee is normative: the same tests, the same primitive semantics, and
  the same reduction behavior apply to every implementation.
- An implementation MAY be target-only (no self-hosting); self-hosting is not a
  conformance requirement.
- **Status vocabulary** (used by chapters and by per-primitive badges):
  - **Draft** — MAY change without notice; implement at your own risk.
  - **Canonical** — MUST be implemented as specified; changes only by specification
    revision (renames are add-new + deprecate-old, never in-place).
  - **Deprecated** — MUST remain available until the stated removal version;
    new code SHOULD NOT depend on it.

## 4. Design principles

Cross-cutting decisions that shape multiple chapters are collected here;
each is normative where its instances appear in the chapters, and the design rationale
behind each is recorded on the wiki
([Design](https://github.com/CategoricalData/hydra/wiki/Design)).

- **Inherit JSON syntax.** Where a Hydra surface form has a JSON counterpart
  (numbers, strings, binary payloads), Hydra inherits the JSON syntax rather than
  defining its own variant, and the corresponding kernel primitives are the JSON
  serializers.
  Instances: strings (RFC 8259; one escape production for the wire format, textual
  string literals, and quoted names), decimals (RFC 8259 number grammar in;
  representation-faithful ECMAScript/JCS layout out; the JSON coder calls
  `printDecimal`/`parseDecimal` directly), float digit strings and non-finite
  sentinels, base64 binary.

Further principles (candidates to be collected here as their chapters land include
round-trippable printing, totalized partiality, and functorial-last parameter order).

## 5. Versioning and citation

- There is exactly ONE copy of the specification, at `docs/specification/` on the
  main branch. The specification is versioned by Git tags — release tag `v<X.Y.Z>`
  fixes the specification as of that release — and versioned copies are never
  stored in the source tree.
- Cite a page at a version as
  `https://github.com/CategoricalData/hydra/blob/v<X.Y.Z>/docs/specification/<page>.md`.
- Every specification page carries a status header:
  hand-written chapters begin `**Status: <Draft|Canonical|Deprecated>** · …`;
  generated pages derive their header from the module's own documentation and carry
  the generated-file notice.
- <!-- PROPOSAL, discussion deferred (2026-07-19): --> at 1.0, additionally publish a
  RENDERED snapshot (built from the tag, outside the source tree — e.g. GitHub Pages)
  with a Zenodo DOI as the citation form for papers. Mechanics to be discussed;
  nothing in the tree depends on this.

## 6. Chapters

Part I — The language:

- [The core data model](core.md) — every `hydra.core` type: the grammar of terms and
  types *(drafting)*
- [Textual syntax](syntax.md) — the notation for terms, types, and their dependencies
- [Constraint classes](classes.md) — the five classes and their semantics
- [Ordering and equality](ordering-and-equality.md) — the canonical total order and
  structural equality of values

Part II — The type system:

- Type inference and elaboration *(planned; consumes the rule enumeration of #377)*
- [Validation](validation.md) — well-formedness constraints and profiles *(drafting)*

Part III — The library:

- [Primitives](primitives/index.md) — the complete primitive catalog, by module
- The kernel module reference *(generated pages; drafting)*
- The lexicon (`lexicon.txt`) — the compact kernel signature listing

Part IV — Interchange:

- [JSON format](json-format.md) — the wire format *(moving from docs/)*

Planned (deferred beyond the current release): effects · graphs and modules ·
reduction semantics · standard mappings (property graphs, RDF) ·
appendices (conformance-suite format).
