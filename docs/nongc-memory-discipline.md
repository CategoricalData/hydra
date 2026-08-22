# Translating Hydra's immutable data model into non-tracing-GC languages

Status: design/feasibility investigation (#678). Contributor-facing.

Hydra maps cleanly into languages with **automatic tracing garbage collection** — Java, Python,
TypeScript/JavaScript, Go, Kotlin, C#, the JVM/Lisp hosts, Haskell. Its data model says nothing about
**allocation, freeing, or ownership**, so target languages without tracing GC are not yet viable *runtime*
hosts, even where a syntax coder already exists (Rust, C++). This note settles, once and for the record,
**why** the GC mapping is clean and **what discipline** it takes to reach the non-GC targets — so the
question stops recurring each time someone re-proposes a Rust, C++, or C head.

It is an investigation, not a head implementation. The deliverable is a shared memory-management discipline
grounded in Hydra's data model, a per-language verdict for Swift, Rust, C++, and C, and compiled
proof-of-concept code: Rust (`Box`, drop-counter), C++ (`unique_ptr` and arena, ASan/LSan), and C (arena,
ASan/LSan). Swift is argued exhaustively but not compiled — no `swiftc` on the build box.

## The shared principle: structural tree-ownership

The two properties that make general memory management hard are:

- **Cycles** — a cyclic object graph cannot be freed by reference counting alone; you need tracing to
  discover unreachable cycles.
- **Mutation** — mutable shared state means a node's lifetime is not determined by any single owner, so you
  need ownership/lifetime tracking (or, again, tracing) to know when to free.

**Hydra's runtime data has neither.** It is immutable, acyclic, and tree-shaped. Verified against the
kernel `Core` module (`packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Types/Core.hs`):

- `Term` is a tree. Its recursive variants hold their sub-terms **by structural nesting**, never by a
  back-pointer: `application`, `cases`, `either term term`, `inject`, `let`, `list<term>`,
  `map<term,term>`, `optional<term>`, `pair<term,term>`, `project`, `record`, `set<term>`,
  `typeApplication`, `typeLambda`, `wrap` (Core.hs, the `Term` union, ~line 449).
- **Recursion in the object language is expressed by variable *name*, not by object link.** `Lambda`
  (Core.hs:316) is a record `{ parameter :: Name, domain :: optional Type, body :: Term }` — the bound
  variable is a `Name`, and the body references it via `TermVariable (Name ...)` (Core.hs:495), a
  *name lookup*, not a pointer back to the binding term. `TermUnwrap` likewise carries a `Name`.
- `Let` (Core.hs:330) expresses (possibly recursive) bindings by **name-scoped** references, not by cyclic
  object links between the binding and its uses.
- Values are immutable: constructed once, never mutated in place.

So the in-memory representation of any Hydra value is an **acyclic tree of immutable nodes**. A `Term`
that "refers to itself" recursively does so through the *naming environment*, which the evaluator resolves;
the data structure on the heap is finite and tree-shaped.

For an immutable acyclic tree, **ownership is structural**: each node is owned by its unique parent and is
freed exactly when the parent is freed. There is no shared ownership to reconcile and no cycle to detect.
This is precisely the case every manual or ownership-based memory regime handles most cleanly — unique
ownership (Rust `Box`, C++ `unique_ptr`, Swift value types) or region/arena allocation freed wholesale.

That reframes the whole problem. We are **not** asking "how does Hydra learn general memory management?"
We are asking the narrow, decidable question: **what mechanism realizes structural tree-ownership in each
target language?** The answer differs per target; the principle is the same.

### Why GC hosts are clean (for the record)

A tracing collector handles the general case — cycles and shared mutable state — automatically, so it also
handles the strictly easier immutable-acyclic-tree case for free. The Hydra→GC-host mapping never has to
say anything about ownership because the collector owns everything. That is the entire reason the GC hosts
came first and cost nothing on this axis. The non-GC targets simply have to *state* the ownership that GC
left implicit — and because the data is an immutable tree, that statement is short.

## Per-language verdicts

Ordered easiest → hardest. One shared principle, four mechanisms.

### 1. Swift — value types rule cycles out entirely — **VERDICT: solved (1.0-clean)**

Swift uses **ARC** (automatic reference counting). ARC is automatic — no manual `free` — but it does **not**
collect reference cycles: two `class` instances that strongly reference each other leak. ARC's single gap is
exactly cycles, and Hydra's data is acyclic by construction, so the gap can never trigger — provided the
mapping uses **value types**, which cannot form cycles at all.

Mapping:
- Hydra record → Swift `struct`
- Hydra union → Swift `enum` (with associated values)
- **Recursive** union/record → Swift `indirect enum` / `indirect case`. Swift's `indirect` keyword tells the
  compiler to box the recursive payload behind a hidden allocation while keeping *value semantics* — the
  type is still copied, not referenced, so no cycle can form even through the indirection.

Value types are **copied, not referenced**, so a cycle is structurally impossible regardless of the data:
there is no shared mutable identity for two nodes to point at each other through. ARC still runs (the
`indirect` boxes are reference-counted internally), but since no cycle can exist, refcounting alone reclaims
everything — the ARC cycle gap is unreachable.

#### Ruling out a forced `class` — the airtight version

The only way ARC's cycle gap could become reachable is if the Swift coder were **forced** to emit a
reference type (`class`) rather than a value type. A cycle needs *two* mutable reference-typed nodes that can
be made to point at each other; value types (`struct`/`enum`) are copied on assignment and have no shared
identity, so they cannot participate in a cycle at all. So the claim reduces to: **enumerate every Hydra type
construct a Swift coder must map, and show none forces a `class`.** The constructs are exactly the
data-carrying variants of the kernel `Type` union (Core.hs, the `Type` union ~line 509):

| Hydra construct | Swift value-type mapping | Forces a `class`? |
|-----------------|--------------------------|-------------------|
| `record` | `struct` | No |
| `union` | `enum` with associated values | No |
| recursive `union`/`record` | `indirect enum` / `indirect case` | No — `indirect` boxes, keeps value semantics |
| `wrap` (newtype) | single-field `struct` | No |
| `list<T>` | `Array<T>` | No (value type; COW) |
| `set<T>` | `Set<T>` | No (value type) |
| `map<K,V>` | `Dictionary<K,V>` | No (value type) |
| `optional<T>` | `Optional<T>` (`T?`) | No (value type) |
| `pair`, `either` | `struct` / two-case `enum` | No |
| `literal`, `unit` | scalar / `Void`-like | No |
| `forall` / `variable` (polymorphism) | generic parameter `<T>` | No — generics are erased-lifetime, not references |
| `function` (stored higher-order *data* only) | closure `@escaping (A) -> B` | Reference-like — **see below** |

Every data construct maps to a value type. Two edge cases deserve an explicit ruling rather than a wave:

1. **Large-substructure sharing for size.** A coder *might* be tempted to share a common sub-tree via a
   `class` to save memory. For immutable tree data this is only ever an *optimization*, never a correctness
   requirement — the tree is well-defined without sharing, and the current model (and every existing coder,
   which emits nominal types field-by-field) introduces none. Swift's `Array`/`String` already give
   copy-on-write sharing *with value semantics* for the common cases, so even the optimization does not need
   `class`. If a future coder deliberately introduced `class`-based sharing, the acyclic invariant would
   still forbid a cycle, and the documented policy would be `weak`/`unowned` on any back-edge — but nothing
   requires it today.
2. **Stored function values (closures).** A `function`-typed *field* would map to a Swift closure, which is a
   reference type and can capture and thus retain other objects. This is the one genuine `class`-like case.
   It is not reachable for Hydra's serialized **data** model (which is first-order — `Term`, types, records
   are all data, and closures are not part of the on-the-wire tree), so it does not affect the data-mapping
   verdict. If a runtime head stored live closures, the discipline would be: closures capture **by value**
   (Swift's default for value types) or with an explicit `[weak self]`-style capture list where a back-edge
   could otherwise form. Again, the acyclic data invariant means no such back-edge arises from the data
   itself.

**Verdict:** Swift is clean for 1.0 on this axis. Discipline = the value-type mapping (`struct`/`enum`,
`indirect` for recursion, value-type containers). ARC's cycle gap is provably unreachable for the data model.

**Caveat — the one verdict without a compiled POC.** There is no `swiftc` on the build box, so Swift is the
sole verdict argued but not compiled. This is a **toolchain gap, not a confidence gap**: the argument above
is exhaustive over the type constructs. What *would* confirm it: emit the value-type mapping for a recursive
type (e.g. `indirect enum Term`), construct and drop it under a Swift toolchain, and confirm with the
memory-graph debugger / `swift-leaks` that ARC's retain count returns to zero (no cycle retained). That is
mechanical once a Swift coder + toolchain exist; it does not change the verdict.

### 2. Rust — structural ownership via `Box` — **VERDICT: solved, POC-confirmed**

Rust's ownership model is *designed* for owned trees, which is exactly what a Hydra value is.

Mapping:
- Hydra record → `struct`; Hydra union → `enum`.
- Owned recursive children → **`Box<T>`**. A directly self-recursive Rust `enum`/`struct` is otherwise of
  infinite size and will not compile; `Box<T>` puts the child behind a single owning heap pointer of known
  size. This is the load-bearing element of the discipline.
- Containers: `list<T>` → `Vec<T>`; `map<K,V>` → `BTreeMap<K,V>` (or `HashMap`); `set<T>` → `BTreeSet`/
  `HashSet`; `optional<T>` → `Option<T>`; `pair` → tuple; `either` → `Either`. These containers own their
  elements, so their recursive elements are owned transitively.
- **No `Rc`/`RefCell`, no explicit lifetimes.** `Rc` is for *shared* ownership and `RefCell` for *interior
  mutability* — a unique-ownership immutable tree needs neither. Explicit lifetimes annotate *borrows*; an
  owned tree has no borrows to annotate. Their absence is a positive result, not an omission.

Leak-freedom follows from Rust's own guarantees: under unique ownership, dropping the root recursively drops
every `Box` and container in the tree exactly once. There is no cycle for `Box`'s refcount-free ownership to
mishandle (that failure mode requires `Rc`, which the discipline excludes).

**State of the existing coder.** The Rust syntax coder (`packages/hydra-ext/src/main/haskell/Hydra/Sources/
Rust/`) already models `Box`, `ReferenceType`, and `Lifetime`, so the syntax layer can express the
discipline. But today it emits `Box` **only for function types** (`Box<dyn Fn(..)>`, `Coder.hs` ~line 487);
enum variants and struct fields wrap the child type **directly, un-boxed** (`encodeEnumVariant`,
`encodeStructField`). So a directly self-recursive generated type would currently fail to compile for lack
of a `Box`. The missing piece is exactly the *policy* this investigation supplies: **box a field whose type
is (transitively) recursive back to the enclosing type.** That is a targeted coder change, not new
machinery — the syntax already has `Box`.

**Verdict:** solved, **POC-confirmed**. Discipline = `Box<T>` for recursive fields, plain owning containers
otherwise, no `Rc`/`RefCell`/lifetimes. See the POC below.

**POC (`poc/rust-nongc/`).** A standalone, zero-dependency crate defines a `Term`-shaped recursive `enum`
under the `Box` discipline (recursive children via `Box<Term>`, `list`→`Vec`, `optional`→`Option`,
`pair`→tuple, `name`→`String`), constructs a nested value, and drops it. The `every_node_dropped_exactly_once`
test attaches an atomic drop-counter to a structurally identical tree and asserts the number of `Drop`
invocations equals the node count: a short count would signal a leak, a double-free would abort. Result on
`rustc 1.98.0` (stable): **both tests pass, zero warnings** — no `Rc`, `RefCell`, or lifetime annotation was
needed to compile, which is itself confirmation that unique ownership suffices. Run with `cargo test`.

### 3. C++ — `unique_ptr` tree or arena — **VERDICT: solved (with API implication), POC-confirmed**

Two candidate disciplines, both leak-free for an immutable tree:

- **(a) RAII unique ownership.** Recursive children as `std::unique_ptr<T>`; the compiler-generated
  destructor recursively frees the tree. Containers `std::vector`/`std::map`/`std::set`/`std::optional`.
  *API implication:* transparent — the consumer holds a value/`unique_ptr` and destruction is automatic at
  scope exit. Closest analogue to the Rust `Box` discipline; recommended default.
- **(b) Arena / region.** Allocate the whole tree in a pool (bump allocator); free the pool at once. No
  per-node destructors run. *API implication:* the consumer holds an arena handle and frees the arena, not
  individual nodes — allocation lifetime is decoupled from node scope. Best when building many trees fast or
  when node-by-node `unique_ptr` overhead matters.

Recommendation: **(a) `unique_ptr` RAII** as the default discipline (transparent API, matches the structural
ownership directly), with **(b) arena** documented as an available optimization for bulk/perf-sensitive
consumers. The existing C++ coder (`Sources/Cpp/`) already has the type machinery; as with Rust, the missing
piece is the *policy* to emit owning smart pointers for recursive fields.

**Verdict:** solved, with the stated API implication (RAII transparent; arena moves ownership to the arena
handle).

**POC (`poc/cpp-nongc/`).** Both disciplines compiled and leak-checked with AddressSanitizer +
LeakSanitizer (valgrind is not on the build box; `-fsanitize=address,leak` is the equivalent). On g++ 12.2:
- `term_unique_ptr.cpp` — a `Term`-shaped struct with `std::unique_ptr<Term>` recursive children and
  `std::vector`/`std::optional` containers. A static counter incremented in `~Term()` asserts **12 nodes
  destroyed exactly once**; LSan reports **0 leaks**, ASan would abort on any double-free. Neither occurs.
- `term_arena.cpp` — the arena option: a bump allocator builds the whole tree, byte accounting asserts
  handed-out == reclaimed, and LSan reports **0 leaks** after the region is freed wholesale — backing the
  "arena API implication" with compiled code, not assertion.

### 4. C — arena/region allocation — **VERDICT: solved-via-arena (with API implication), POC-confirmed**

C has no destructors and no ownership types, so RAII is unavailable. The clean answer for an immutable tree
is **arena/region allocation**:

- Allocate every node of a tree from a region (a growable block / bump allocator).
- Free the region **wholesale** when the tree is no longer needed — no per-node `free`, so no leak and no
  double-free, regardless of tree shape or size.
- Construction uses `arena_alloc(arena, sizeof(node))`; the tree is a normal C struct graph *within* the
  arena's lifetime.

*API implication (the honest cost):* the generated data library is no longer "malloc a node / free a node."
The consumer holds and frees an **arena handle**, and every value's lifetime is tied to its arena. This is a
different — but well-established and entirely leak-safe — contract than idiomatic per-object C. It is
acceptable for a generated immutable-data library precisely because the data is immutable and tree-shaped:
there is never a reason to free one node early, so wholesale region free loses nothing.

**Verdict:** solved-via-arena-with-API-implications. Not blocked. The single caveat is that the C consumer
must accept arena-scoped lifetimes; if a future consumer needed per-node freeing (they should not, for
immutable data), that would be the only thing to revisit.

**POC (`poc/c-nongc/`).** `term_arena.c` implements a minimal bump-arena (`arena_alloc` / `arena_free_all`,
growing in blocks) and builds a `Term`-shaped struct graph — nodes, names, and child-pointer arrays all in
the region — then frees it wholesale. Two signals on gcc 12.2 with `-fsanitize=address,leak`: byte
accounting asserts **`bytes_malloced == bytes_freed`** (65560 == 65560 for the sample), and a multi-block
stress (500 nodes forced into tiny 256-byte blocks) exercises the block-growth + wholesale-free path; LSan
reports **0 leaks**. The POC surfaced **no API wart beyond the already-stated arena-handle implication** —
so the verdict stands as *solved-via-arena*, honestly, not retrofitted: the only cost is the consumer holding
and freeing an arena handle, which is exactly what the section predicted.

## Graduation implications (discipline found → runtime head)

For each language, what remains between "discipline stated here" and "runtime head," and 1.0-eligibility on
*this* (memory) axis:

| Language | Memory-axis verdict | Remaining to a head | 1.0-eligible (memory) |
|----------|--------------------|---------------------|---------------------------|
| Swift | solved (value types); no compiled POC | New Swift coder (none yet); runtime; tests | **Yes** — toolchain gap |
| Rust | solved (`Box`), **POC-confirmed** | Coder policy: box recursive fields; runtime; tests | **Yes** |
| C++ | solved (`unique_ptr`/arena), **POC-confirmed** | Coder policy: owning smart ptrs; runtime; tests | **Yes** |
| C | solved-via-arena, **POC-confirmed** | Coder policy + arena runtime; tests | **Yes**; heaviest lift |

The memory-management problem — the one thing that gated all four and recurred in every Rust/C++/C head
proposal — is **solved for all four**: three (Rust, C++, C) with **compiled, leak-checked POCs**, and Swift
with an exhaustive per-construct argument (the only one lacking a compiled POC, purely because no `swiftc` is
on the build box). The honest API implication stands for C++ (arena option) and C (arena required). None is
blocked-defer on memory. The remaining work per language is ordinary host-graduation effort (coder maturity,
runtime, primitives, tests), tracked in separate per-language head issues gated on this verdict.

## Summary of verdicts

- **Swift:** solved — value types; ARC's cycle gap provably unreachable (exhaustive per-construct argument).
  1.0-clean. The one verdict without a compiled POC — a toolchain gap (no `swiftc`), not a confidence gap.
- **Rust:** solved — `Box` for recursive fields, no `Rc`/`RefCell`/lifetimes. **POC-confirmed** (drop-counter).
- **C++:** solved — `unique_ptr` RAII (default) or arena (optional); stated API implications.
  **POC-confirmed** (both disciplines, ASan/LSan clean).
- **C:** solved-via-arena — wholesale region free; consumer holds an arena handle. **POC-confirmed**
  (byte accounting + multi-block stress, LSan clean); no API wart beyond the stated arena handle.

The enabling fact behind all four: Hydra runtime data is an **immutable acyclic tree**, so ownership is
structural — each node owned by its unique parent — and neither cycle-tracing nor lifetime tracking is
required.
