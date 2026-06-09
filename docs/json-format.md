# Hydra JSON encoding (format reference)

This document specifies Hydra's JSON encoding for terms and types.
It covers the rules of the JSON coder itself, independent of the shape of the `Module` type
or any other particular kernel type being encoded.
It also specifies the three JSON **sidecar and metadata files** that travel alongside the
encoded modules — `manifest.json`, `package.json`, and `digest.json` — in
[Sidecar and metadata files](#sidecar-and-metadata-files).

The encoding is intended to be stable for the lifetime of the v1 series.
A `moduleFormatVersion` field on the per-package `build/<set>/digest.json` records the encoding version,
but that digest is gitignored and not shipped, so external consumers cannot read it today;
see [Format versioning](#format-versioning) below.

## Status

This document describes **`moduleFormatVersion: 1`**.

The encoding is implemented by the JSON coder in
`packages/hydra-kernel/src/main/haskell/Hydra/Sources/Json/{Encode,Decode}.hs`.
Every implementation that reads or writes Hydra JSON modules must conform to the rules below.

## Why JSON?

The exchange format's primary audience is the Hydra toolchain (the per-target coders,
`bootstrap-from-json`, the per-package digest checks), not humans.
At 380+ modules with deeply nested term trees, no encoded module is meaningfully
readable by hand in any text format; debugging uses `jq` or scripts.

That tilts the trade firmly toward JSON over YAML or other text formats:

- **Universal tooling.** Every target language has a first-class JSON parser in its
  standard library or a single canonical choice. YAML has multiple incompatible dialects
  (1.1 vs 1.2, flow vs block, anchor semantics) and a long history of parser CVEs.
  Multiplying that exposure across nine host implementations is not a trade we want to make.
- **Deterministic serialization.** The byte-equivalence guarantee in
  [Top-level shape](#top-level-shape) and the digest-driven cache layer depend on a
  single canonical encoding. YAML admits multiple valid serializations of the same logical
  document; enforcing a canonical form across nine implementations would be additional work.
- **Schema fit.** Hydra terms map cleanly onto JSON's primitive set: literals, records as
  objects, [variants](#tagged-unions) as one-key objects, lists as arrays. None of YAML's
  distinguishing features — anchors, multi-document streams, custom tags, comments — would
  add value; Hydra's own [annotation type](#annotations) is the principled place for
  in-band metadata.
- **Smaller surface.** The JSON coder is a small, well-tested module sitting on the
  critical bootstrapping path. Replacing it carries cost.

The one dimension where YAML would win is file size: typically 2-3× smaller for deeply
nested terms with short keys. That isn't load-bearing — `dist/json/` compresses well in
git packs, and the dominant time costs in sync are inference and code generation, not I/O.
If compactness becomes a real bottleneck, a binary encoding (CBOR, protobuf, or a Hydra-native
schema-driven format) would beat YAML on every dimension that matters.

## Top-level shape

Every JSON file under `dist/json/**/src/<set>/json/**/*.json` (apart from `manifest.json`)
encodes exactly one Hydra `Module`.
The file is JSON-pretty-printed with stable, deterministic output:
the same module always produces byte-identical JSON.

A separate `dist/json/<pkg>/build/<set>/digest.json` artifact carries content hashes
for incremental builds; it lives in the gitignored `build/` subtree and is regenerated
on every sync. See [Format versioning](#format-versioning).

### Why one file per module

Almost every programming language has a notion of file-level modules.
Per-module JSON preserves the 1:1 mapping between source modules and
emitted files across every stage of transformation: a Hydra source
module is one file, its JSON encoding is one file, and each target
emission (Haskell, Java, Python, Scala, Lisp, TypeScript, Go, ...) is
one file. The invariant survives translation rather than being
something targets have to reconstruct.

Two other useful properties follow from that choice:

- **Granular caching.** Freshness, digest, and dirty-set propagation
  all operate at the module level. A monolithic per-package or per-
  universe file would make the smallest unit of cache invalidation
  the entire blob, defeating incremental sync.
- **Diff and merge cleanliness.** A one-module change touches one
  file. Branches that edit disjoint modules merge without conflict;
  reviewers see only the relevant module's diff. A monolithic file
  would surface every unrelated change in every branch.

## Tagged unions

Hydra's sum types (`Term`, `Type`, `Literal`, `LiteralType`, `Json.Value`, etc.) encode as
single-key JSON objects:

```json
{"<variant-tag>": <payload>}
```

The key is the variant name; the value is the variant's payload, encoded by the same rules.
Each tagged-union object has exactly one key.
This applies recursively at every level of the AST.

Examples:

- `Term.literal (string "hello")` → `{"literal": {"string": "hello"}}`
- `Type.list (variable "T")` → `{"list": {"variable": "T"}}`
- `Term.unit` → `{"unit": {}}` (nullary variants take an empty object payload; see [Empty objects](#empty-objects))

The single-key shape is structurally dual to the record encoding for the
opposite reason that records are objects: a record is a conjunction (it has
*all* of its fields, which maps to a JSON object containing all its
attributes), and an injection is a disjunction (it has *exactly one* of its
fields, which maps to a JSON object containing one attribute). The variant
key *is* the discriminator; there is no separate `"tag"` / `"value"`
indirection to maintain.

## Records

A *record type* (`Type.record`) encodes as a JSON **array** of `{"name": ..., "type": ...}`
objects, in the field order declared in the DSL source:

```json
{"record": [
  {"name": "first", "type": {...}},
  {"name": "second", "type": {...}}]}
```

**Field order is significant and stable.**
The order matches the field order in the DSL source.
Generators in any host language must preserve declaration order when emitting record types.
Consumers may rely on this order (e.g., for stable digests, for deterministic codegen).

A *record term* (`Term.record`, i.e., a runtime record value) encodes as a **JSON object**
keyed by field name.
**Key order in a record-term object preserves the field order of the record** — the same
declaration order that the record type uses, not an alphabetical sort.
This is a property of the encoder: the encoder walks the record's fields in order and emits
them in that order.

JSON objects are order-agnostic *by spec* — `{"a": 1, "b": 2}` and `{"b": 2, "a": 1}` denote
the same JSON value — but every tool that emits JSON nonetheless chooses *some* order. Hydra
chooses to preserve declaration order rather than alphabetize. This makes Hydra-generated JSON
both **deterministic** (the same value always serializes to the same bytes) and **intuitive**
(fields read in the order the author declared them — e.g. a `PrimitiveDefinition`'s
`defaultImplementation` stays last, where it was written, instead of floating to the front
under an alphabetical sort).

The two rules are now aligned on purpose:
both record types and record terms preserve declaration order.
A consumer that reproduces the encoding for a digest must emit record-term keys in field order.

> **Order-sensitive equality.** Because the `Value.object` payload is an *ordered list of
> key/value pairs* (`[(string, Value)]`), not an unordered map, **equality of `Value` objects
> is order-sensitive.** Two objects with the same pairs in a different order are equal *as JSON*
> (per the spec) but **not equal as Hydra `Value`s**. Anything that compares, hashes, or
> deduplicates `Value`s — round-trip tests, digest computation, structural diffs — must account
> for this: a re-serialization that reorders fields will not compare equal to the original
> `Value`, even though both are valid encodings of the same JSON. Decoders that only need to
> *look fields up by name* (the common case) are unaffected — they collapse the pair list into a
> name-keyed map at the boundary and never observe order.

## Optional fields

`Optional`-typed fields use one of three shapes, depending on context:

1. **Standalone `Optional T` value** (T is not itself `Optional`):
   - `none` → `null`
   - `given v` → encoded value of `v`
2. **Standalone `Optional (Optional T)` value** (nested `Optional`):
   - `none` → `null`
   - `given v` → `[<encoded v>]` (array wrapper to disambiguate from the inner `none`)
3. **Record field of simple `Optional T` type**:
   - `none` → field is **omitted entirely** from the record-term object
   - `given v` → `<field-name>: <encoded v>` (no wrapper)

`null` only ever encodes an `Optional.none`. It is never used as a generic sentinel,
never used to mean "missing value" in any other context, and never appears for a non-`Optional` type.

Each rule exists to eliminate an ambiguity the previous one would create if
extended naively. Rule 1 works because no Hydra value other than `Optional.none`
encodes to bare `null`. Rule 2 needs the array wrapper because without it the
outer `none` and the inner `none` would both be bare `null` and a
consumer couldn't tell them apart. Rule 3 omits the field in records because
the absent key is unambiguous (record-term keys are never bound to `null`
elsewhere), and the omission is more compact than encoding `null`.

## Empty values

- `Type.unit`, and any nullary variant payload, encodes as `{}`.
- An empty list, set, or array of records encodes as `[]`.
- An empty map encodes as `[]` (a map is encoded as an array of entries; see [Maps](#maps)).

## Maps

`Type.map` and `Term.map` encode as JSON arrays of two-key objects:

```json
[{"key": <encoded k>, "value": <encoded v>}, ...]
```

The `key` / `value` envelope is required because JSON object keys must be strings,
and Hydra map keys may be any term (`Name`, structured key, etc.).
Entry order is the iteration order of the source `Map` (by `Ord` on the key type for the
reference Haskell encoder; equivalent total order for other encoders).

## Pairs

`Type.pair` and `Term.pair` encode as a two-key JSON object:

```json
{"first": <encoded a>, "second": <encoded b>}
```

At the JSON layer, the encoding follows the [record rule](#records), using
the special field names `first` and `second`.

## Eithers

`Type.either` and `Term.either` encode as a single-key JSON object:

```json
{"left":  <encoded a>}    // for Left a
{"right": <encoded b>}    // for Right b
```

Decoders treat presence of `left` and absence of `right` (or vice versa) as the discriminator.
A well-formed `Either` value never carries both keys.

At the JSON layer, the encoding follows the [tagged-union rule](#tagged-unions),
with `left` and `right` as the variant names.

## Wrapped types

`Type.wrap` and `Term.wrap` (newtype-style wrappers) encode transparently in the untyped
fallback, and via the wrapped type's encoding in the typed path.
The wrapper's `typeName` is preserved on the term as `"typeName": "<qualified.Name>"`
inside the `wrap` payload.

## Annotations

`Term.annotated` and `Type.annotated` encode as:

```json
{"annotated": {
  "body": <encoded inner>,
  "annotation": <encoded annotation Term>}}
```

The `annotation` field is a `Term` (post-#386 schema flip; previously a
`Map<Name, Term>`). The canonical shape for an annotation Term is a
`TermMap` keyed by `TermVariable`s, so a one-entry annotation
`{"k1": <v>}` serializes as:

```json
{"annotated": {
  "body": <encoded inner>,
  "annotation": {"map": [
    {"key":   {"variable": "k1"},
     "value": <encoded v>}]}}}
```

Producers should use the `wrapAnnotationMap` kernel helper (or the
language-specific `annots` / `annotated` DSL functions) so the encoded
key shape is `{"variable": "k1"}` consistently across hosts.
Consumers should call `getAnnotationMap` to project back to
`Map Name Term`; that function also accepts the transitional
`{"wrap": {"typeName": "hydra.core.Name", "body": …}}` key shape so
older fixtures continue to load.

## Literals

`Literal` is a tagged union over the primitive literal types:

```json
{"string":   "abc"}
{"boolean":  true}
{"integer":  {"int32":   42}}
{"float":    {"float64": 3.14}}
{"binary":   "<base64>"}
```

Integer and float literals are themselves tagged with their precision class
(`int8`/`int16`/`int32`/`int64`/`uint8`/.../`bigint` and `float32`/`float64`).

### Integer formatting

`Literal.integer` values encode according to whether the precision class can
exceed JavaScript's `Number.MAX_SAFE_INTEGER` (`2^53 - 1`):

- **As JSON numbers:** `int8`, `int16`, `int32`, `uint8`, `uint16`, `uint32`.
  Every value of these types fits safely in a JS `Number`.
- **As JSON strings:** `int64`, `uint64`, `bigint`.
  Values of these types can exceed `2^53 - 1` and would lose precision if
  read by a JavaScript consumer via `JSON.parse`. Strings preserve precision
  on the wire.

The threshold is the IEEE 754 double's integer-precision boundary, not the
64-bit signed range. Typed-language consumers (Haskell, Java, Python, etc.)
have arbitrary-precision integer types and don't need the string protection,
but JavaScript's `Number` is the only integer type its `JSON.parse` produces,
and the format protects against silent corruption on the JS side.

`uint32` moved from the string group to the number group; earlier output quoted
it as a string. For backward compatibility the decoder accepts `uint32` as
*either* a JSON number or a JSON string, so JSON written before the change still
reads. This is a clarification of the existing format and does **not** bump
`formatVersion`.

### Float formatting

`Literal.float` values — including both `float32` and `float64` precisions — encode symmetrically:

- **Finite values** encode as JSON numbers, using shortest round-trip decimal representation:
  the shortest decimal string that, when parsed back at the value's precision, returns the
  original IEEE 754 bit pattern exactly.
  The encoder does not "tidy" the input;
  if the input is the float64 bit pattern `0.30000000000000004`, the encoder emits exactly that.
- **Non-finite values and `-0.0`** encode as JSON strings — `"Infinity"`, `"-Infinity"`,
  `"NaN"`, `"-0.0"` — because JSON's number grammar cannot represent these.

The four sentinels are the complete set of IEEE 754 `float32` / `float64` values that
JSON's number grammar can't represent: `+Infinity` and `-Infinity` (overflow),
`NaN` (any NaN bit pattern collapses to a single sentinel), and `-0.0` (because
JSON's `-0` parses as `0` in many parsers, losing the sign of zero). Any other
finite float survives the number grammar.

The sentinel form is a string rather than an object wrapper (e.g.
`{"nan": null}`) to keep the wire shape uniform: a `float32` or `float64`
literal always appears as a single JSON scalar (number or string), never
sometimes a scalar and sometimes an object. A consumer that wants to recognize
a float literal only has to look at one position in the AST, regardless of the
value.

Decoders accept both shapes for either precision.
The schema disambiguates `float32` from `float64`, just as it disambiguates `int8` from `int64`;
a JSON number `0.5` decodes to a `float32` value under one schema and a `float64` value under another.

### String escapes

A `Literal.string` value encodes as a JSON string. Within the string body:

- `"` (U+0022) is always escaped as `\"`.
- `\` (U+005C) is always escaped as `\\`.
- The five JSON shortcut escapes are used where applicable: `\b` (U+0008), `\f` (U+000C),
  `\n` (U+000A), `\r` (U+000D), `\t` (U+0009).
- Other control characters (U+0000 through U+001F not covered by the shortcuts) are
  escaped as `\u00XX` with lowercase hex digits.
- All other characters are emitted as their literal UTF-8 byte sequences. This includes:
  - Forward slash `/` (no `\/` escape produced; decoders must accept either form).
  - Non-ASCII characters such as `é`, `中`, `🎉` — emitted as multi-byte UTF-8.
- Code points above U+FFFF (supplementary plane) are emitted as their literal 4-byte UTF-8
  sequence, not as `\uXXXX\uXXXX` surrogate pairs. Decoders must accept either form.

Hydra strings are assumed to be well-formed Unicode.
Behavior on malformed input (lone surrogates, invalid byte sequences) is a decode error.

## Stability of byte order

For consumers that hash file bytes (digests, content-addressed caches, byte-equality assertions in tests),
the encoder must produce identical bytes for identical input across runs and across host languages.
Most ordering questions are settled by other rules above:

- Record types preserve DSL declaration order ([Records](#records)).
- Record-term object keys preserve the record's field (declaration) order ([Records](#records)).
- `Type.map` / `Term.map` arrays preserve the source map's iteration order ([Maps](#maps)).
- Tagged-union objects have exactly one key.

Two further cases are not covered by those rules and need their own pinning:

### JSON object keys (non-Map cases)

Some files use plain JSON objects whose keys are simple strings — `digest.json`'s `hashes` field is
the canonical example.
For these, writers must emit keys in **lexicographic order** (Unicode code-point order, equivalent to
`LC_ALL=C` sort).
This rule does **not** apply to `Map` values, which use the `[{"key", "value"}]` envelope and preserve
source-map iteration order, nor to record-term JSON objects (which preserve field declaration order — a
separate rule; see [Records](#records)).

### Manifest array values

`manifest.json` files (per package, under `dist/json/<pkg>/src/main/json/manifest.json`) list each module
name owned by the package, grouped by category (`mainModules`, `dslModules`, `defaultLibModules`, etc.;
see [Sidecar and metadata files](#sidecar-and-metadata-files) for the full field set).
Each array's entries must be sorted **lexicographically by module name string**.
This is independent of the source code's enumeration order — the wire-format requirement is
sorted; the runtime code that drives the writer should sort before encoding.

### Trailing whitespace and line endings

Every JSON file under `dist/json/` ends with a single LF (`\n`, U+000A) byte after the final
closing brace.
No other trailing bytes (spaces, tabs, CR, additional newlines) appear at end-of-file or end-of-line.
Line endings inside the file are LF only; CRLF is not produced on any platform.

Indentation inside the file is 2 spaces per nesting level.
Pretty-printing collapses leaf values onto the parent line where doing so does not
exceed an internal soft-wrap budget.

## Format versioning

Encoding changes are gated by a `moduleFormatVersion` integer carried at the top level of each
package's per-source-set `digest.json` (`dist/json/<pkg>/build/<set>/digest.json`):

```json
{
  "digestFormatVersion": 1,
  "moduleFormatVersion": 1,
  "hashes": {...}
}
```

The digest sidecar carries two distinct version descriptors, both reset to `1` for the 0.16.0 release:

- `moduleFormatVersion` versions the JSON encoding of the sibling module files
  (`dist/json/<pkg>/.../*.json`) — the wire format this document specifies.
- `digestFormatVersion` versions the digest file's *own* internal schema
  (the simple hash map vs. the inputs/outputs/generator layout). It is not meant for consumers
  gating on the module-JSON encoding.

Bump rules for `moduleFormatVersion`:

- It **increments by 1** when a parser written for version *N* would mis-parse
  version *N+1*. This is the load-bearing definition: a consumer who keys their parser
  selection off `moduleFormatVersion` is guaranteed correct results.
- Adding a new optional field, a new `Term` or `Type` variant, or a new module is **not** a
  bump: existing parsers will see an unknown variant and can fail loudly, but they will
  not silently mis-parse anything that was valid in the prior version.
- Renaming a key, removing a key, changing a key's value type, or changing the encoding of
  any existing variant **is** a bump.
- Bumps are expected to be rare — measured in years, not releases.

Caveat on consumer visibility: `digest.json` is gitignored and regenerated every sync, so it is
**not** part of a published package. An external consumer reading shipped artifacts therefore cannot
currently read `moduleFormatVersion` at all. Surfacing the format version in shipped data is tracked
under #370 / the post-#370 issue. Until then, `moduleFormatVersion` is an internal build-cache
descriptor, not a consumer contract.

Module files themselves carry no version field;
the package's `build/<set>/digest.json` is the single source of truth for the format
version of all JSON files in the same package's source set.

The two sibling metadata files — `manifest.json` and `package.json` — carry their own
schema-version fields (`manifestFormatVersion`, `packageFormatVersion`), independent of
`moduleFormatVersion` and also reset to `1` for 0.16.0. See
[Sidecar and metadata files](#sidecar-and-metadata-files) for their full schemas.

## Conformance

A conforming encoder must:

- Emit byte-identical output for byte-identical input modules.
- Preserve declared field order in record types (see [Records](#records)).
- Preserve field (declaration) order in record-term object keys (see [Records](#records)).
- Sort plain JSON-object keys lexicographically (e.g., `digest.json`'s `hashes`)
  and `manifest.json` array values lexicographically (see [Stability of byte order](#stability-of-byte-order)).
- Emit `null` only for `Optional.none`; never as a generic sentinel.

A conforming decoder must:

- Accept all of the above shapes and decode them to the corresponding Hydra terms/types.
- Reject any tagged-union object with zero or more than one key.
- Reject any `Either` object carrying both `left` and `right` keys.

## Sidecar and metadata files

Three JSON files describe and track the encoded modules rather than encoding terms themselves.
Unlike module files (which are pure `Module` serializations governed by `moduleFormatVersion`),
each of these carries its own schema-version field, all reset to `1` for the 0.16.0 release.
They are plain JSON objects: 2-space indent, LF line endings, and the same lexicographic key
ordering described in [Stability of byte order](#stability-of-byte-order).

### `manifest.json` — per-package module listing

One per package at `dist/json/<pkg>/src/main/json/manifest.json` (generated; tracked in git).
It lists the modules each package owns, grouped by role, so a host can load a package without
scanning the filesystem. Each array is sorted lexicographically by module name.

| Field | Type | Meaning |
|-------|------|---------|
| `manifestFormatVersion` | integer | Schema version of this manifest file. Currently `1`. |
| `package` | string | The package name (e.g. `hydra-rdf`). |
| `mainModules` | array of string | Module names making up the package's main source set. |
| `testModules` | array of string | Module names in the package's test source set (empty for most packages). |
| `dslModules` | array of string | Generated DSL-wrapper module names (empty when the package defines no DSL-wrapped types). |
| `defaultLibModules` | array of string | Historically: default-implementation library module names. Permanently empty after #437 (defaults are inline in their canonical `Lib/<Sub>.hs` registry now). Field retained for manifest-schema continuity until a follow-up drops it. |

Example (`mainModules` abbreviated):

```json
{
  "defaultLibModules": [],
  "dslModules": [],
  "mainModules": ["hydra.owl.syntax", "hydra.rdf.serde", "hydra.rdf.syntax"],
  "manifestFormatVersion": 1,
  "package": "hydra-rdf",
  "testModules": []
}
```

The monolithic universe manifest (written to `dist/json/manifest.json` rather than per package)
carries one additional `kernelModules` array; the per-package manifests above do not.

### `package.json` — hand-authored package descriptor

One per package at `packages/<pkg>/package.json` (hand-authored; the canonical source of package
metadata). It is the single source of truth for a package's identity and dependency edges, which
drive build ordering and per-language packaging. This is **distinct** from the npm `package.json`
files under `heads/typescript/` and `heads/wasm/`, which follow npm's own schema and are unrelated.

| Field | Type | Required | Meaning |
|-------|------|----------|---------|
| `packageFormatVersion` | integer | yes | Schema version of this descriptor. Currently `1`. |
| `name` | string | yes | The package name (matches the directory, e.g. `hydra-kernel`). |
| `description` | string | yes | One-line human-readable summary; flows into per-language package metadata. |
| `sourceLanguage` | string | yes | The host language the package's DSL sources are authored in (today always `haskell`). |
| `dependencies` | array of string | yes | Other package names this package depends on; build order is a topological sort over these edges. May be empty. |
| `targetLanguages` | array of string | no | Restricts which target languages the package is regenerated to. Omitted means every target. |

```json
{
  "packageFormatVersion": 1,
  "name": "hydra-rdf",
  "description": "RDF ecosystem support for Hydra: RDF, OWL, SHACL, ShEx, XML Schema",
  "sourceLanguage": "haskell",
  "targetLanguages": ["haskell", "java", "python"],
  "dependencies": ["hydra-kernel"]
}
```

### `digest.json` — build-cache sidecar

The digest tracks content hashes so the build can skip regenerating unchanged work. It lives under
`dist/json/<pkg>/build/<set>/digest.json` (and a universe digest at `dist/json/build/digest.json`).
It is **gitignored and regenerated every sync**, so it is not shipped in a published package. Parsing
is deliberately tolerant: unknown keys are ignored and a malformed digest degrades to an empty cache,
so renaming or adding fields never breaks an older reader, and no migration step is needed.

Two layouts exist, both carrying the two version fields and a content-hash core:

- **Hashes layout** — a flat per-namespace hash map. Used for source-set digests.
- **Inputs/outputs layout** — records each input and output file with its `kind` and `hash`,
  plus a `generator` stamp. Used by the `digest-check` executable.

| Field | Type | Layout | Meaning |
|-------|------|--------|---------|
| `digestFormatVersion` | integer | both | Schema version of the digest file's own format. Currently `1`. |
| `moduleFormatVersion` | integer | both | The [module wire-format version](#format-versioning) the sibling `*.json` files were written at. Currently `1`. |
| `hashes` | object | hashes | Map of module namespace → content hash (hex), sorted by namespace. |
| `selfHash` | string | both | Hash over this package's own `hashes`; empty on legacy digests. |
| `depHash:<pkg>` | string | both | Recorded `selfHash` of dependency `<pkg>` at write time. The `depHash:` prefix keeps these distinct from namespace keys; transitive invalidation compares them against deps' current `selfHash`. |
| `generator` | string | inputs/outputs | Per-target generator-stamp identity (see [build-system.md](build-system.md)). |
| `inputs` | object | inputs/outputs | Map of input path → `{ "kind": ..., "hash": ... }`. |
| `outputs` | object | inputs/outputs | Map of output path → `{ "kind": ..., "hash": ... }`. |

```json
{
  "digestFormatVersion": 1,
  "moduleFormatVersion": 1,
  "selfHash": "…",
  "depHash:hydra-kernel": "…",
  "hashes": {
    "hydra.rdf.syntax": "…",
    "hydra.rdf.utils": "…"
  }
}
```

See [Format versioning](#format-versioning) for the distinction between the two version fields and the
bump rules, and [build-system.md](build-system.md) for the digest's role in the cache hierarchy.
