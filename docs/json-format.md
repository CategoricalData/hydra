# Hydra JSON encoding (format reference)

This document specifies Hydra's JSON encoding for terms and types.
It covers the rules of the JSON coder itself, independent of the shape of the `Module` type
or any other particular kernel type being encoded.

The encoding is intended to be stable for the lifetime of the v1 series.
A `formatVersion` field on the per-package `digest.json` advertises the version a consumer is reading;
see [Format versioning](#format-versioning) below.

## Status

This document describes **`formatVersion: 1`**.

The encoding is implemented by the JSON coder in
`packages/hydra-kernel/src/main/haskell/Hydra/Sources/Json/{Encode,Decode}.hs`.
Every implementation that reads or writes Hydra JSON modules must conform to the rules below.

## Top-level shape

Every JSON file under `dist/json/**/*.json` (apart from `digest.json` and `manifest.json`)
encodes exactly one Hydra `Module`.
The file is JSON-pretty-printed with stable, deterministic output:
the same module always produces byte-identical JSON.

`digest.json` is a sibling artifact carrying content hashes for incremental builds;
see [Format versioning](#format-versioning).

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
**Key order in a record-term object is alphabetical by field name** (lexicographic sort).
This is a property of the encoder, not of the source type.
Consumers must not depend on key order in a record-term object — JSON objects are unordered
by spec — but if they reproduce the encoding for a digest, they must follow this rule.

The two rules are different on purpose:
record types appear in the AST and need declaration order to round-trip the source;
record terms are runtime data and need a canonical order for byte-stable output.

## Optional fields

`Maybe`-typed fields use one of three shapes, depending on context:

1. **Standalone `Maybe T` value** (T is not itself `Maybe`):
   - `Nothing` → `null`
   - `Just v` → encoded value of `v`
2. **Standalone `Maybe (Maybe T)` value** (nested `Maybe`):
   - `Nothing` → `null`
   - `Just v` → `[<encoded v>]` (array wrapper to disambiguate from the inner `Nothing`)
3. **Record field of simple `Maybe T` type**:
   - `Nothing` → field is **omitted entirely** from the record-term object
   - `Just v` → `<field-name>: <encoded v>` (no wrapper)

`null` only ever encodes a `Maybe.Nothing`. It is never used as a generic sentinel,
never used to mean "missing value" in any other context, and never appears for a non-`Maybe` type.

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

## Eithers

`Type.either` and `Term.either` encode as a single-key JSON object:

```json
{"left":  <encoded a>}    // for Left a
{"right": <encoded b>}    // for Right b
```

Decoders treat presence of `left` and absence of `right` (or vice versa) as the discriminator.
A well-formed `Either` value never carries both keys.

## Wrapped types

`Type.wrap` and `Term.wrap` (newtype-style wrappers) encode transparently in the untyped
fallback, and via the wrapped type's encoding in the typed path.
The wrapper's `typeName` is preserved on the term as `"typeName": "<qualified.Name>"`
inside the `wrap` payload.

## Annotations

`Term.annotated` and `Type.annotated` encode as:

```json
{"annotated": {
  "annotation": [{"key": "<name>", "value": <encoded value>}, ...],
  "body": <encoded inner>}}
```

The annotation is a `Map Name Term`, which serializes per the [Maps](#maps) rule.
Because `Name` is just a string at the wire level, this list-of-pairs shape is a plain
key/value list — duplicate keys would parse but should not be produced.

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
(`int8`/`int16`/`int32`/`int64`/`uint8`/.../`bigint` and `float32`/`float64`/`bigfloat`).

### Float formatting

`Literal.float` values — including both `float32` and `float64` precisions — encode symmetrically:

- **Finite values** encode as JSON numbers, using shortest round-trip decimal representation:
  the shortest decimal string that, when parsed back at the value's precision, returns the
  original IEEE 754 bit pattern exactly.
  The encoder does not "tidy" the input;
  if the input is the float64 bit pattern `0.30000000000000004`, the encoder emits exactly that.
- **Non-finite values and `-0.0`** encode as JSON strings — `"Infinity"`, `"-Infinity"`,
  `"NaN"`, `"-0.0"` — because JSON's number grammar cannot represent these.

Decoders accept both shapes for either precision.
The schema disambiguates `float32` from `float64`, just as it disambiguates `int8` from `int64`;
a JSON number `0.5` decodes to a `float32` value under one schema and a `float64` value under another.

`Literal.float (bigfloat _)` is deprecated and will be retired in a future release;
its current encoding is documented in the Haskell encoder source.

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
- Record-term object keys are alphabetical by field name ([Records](#records)).
- `Type.map` / `Term.map` arrays preserve the source map's iteration order ([Maps](#maps)).
- Tagged-union objects have exactly one key.

Two further cases are not covered by those rules and need their own pinning:

### JSON object keys (non-Map cases)

Some files use plain JSON objects whose keys are simple strings — `digest.json`'s `hashes` field is
the canonical example.
For these, writers must emit keys in **lexicographic order** (Unicode code-point order, equivalent to
`LC_ALL=C` sort).
This rule does **not** apply to `Map` values, which use the `[{"key", "value"}]` envelope and preserve
source-map iteration order, nor to record-term JSON objects (alphabetical by field name, which is the same
rule applied separately).

### Manifest array values

`manifest.json` files (per package, under `dist/json/<pkg>/src/main/json/manifest.json`) list each module
namespace owned by the package, grouped by category (`mainModules`, `dslModules`, `evalLibModules`, etc.).
Each array's entries must be sorted **lexicographically by namespace string**.
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

Encoding changes are gated by a `formatVersion` integer carried at the top level of each
package's checked-in `digest.json`:

```json
{
  "formatVersion": 1,
  "version": 1,
  "hashes": {...}
}
```

Bump rules:

- `formatVersion` **increments by 1** when a parser written for version *N* would mis-parse
  version *N+1*. This is the load-bearing definition: a consumer who keys their parser
  selection off `formatVersion` is guaranteed correct results.
- Adding a new optional field, a new `Term` or `Type` variant, or a new module is **not** a
  bump: existing parsers will see an unknown variant and can fail loudly, but they will
  not silently mis-parse anything that was valid in the prior version.
- Renaming a key, removing a key, changing a key's value type, or changing the encoding of
  any existing variant **is** a bump.
- Bumps are expected to be rare — measured in years, not releases.

The other `version` field in `digest.json` describes the digest file's own internal schema
and is not meant for consumers gating on the encoding format.
Consumers should read `formatVersion` only.

Module files themselves carry no version field;
the package's `digest.json` is the single source of truth for the format version of all
sibling JSON files in the same package.

## Conformance

A conforming encoder must:

- Emit byte-identical output for byte-identical input modules.
- Preserve declared field order in record types (see [Records](#records)).
- Sort record-term object keys alphabetically by field name.
- Sort plain JSON-object keys lexicographically (e.g., `digest.json`'s `hashes`)
  and `manifest.json` array values lexicographically (see [Stability of byte order](#stability-of-byte-order)).
- Emit `null` only for `Maybe.Nothing`; never as a generic sentinel.

A conforming decoder must:

- Accept all of the above shapes and decode them to the corresponding Hydra terms/types.
- Reject any tagged-union object with zero or more than one key.
- Reject any `Either` object carrying both `left` and `right` keys.
