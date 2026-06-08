# Ingesting JSON data into a Hydra schema

A guide to taking a pile of external JSON, encoding it as Hydra `Term`s
against a user-defined schema, and validating it end-to-end via
generated decoders. Worked in Python; the same shape applies to other
hosts.

> **Status: draft.** This recipe is based on a single real-world
> migration (the SmSn knowledge graph → `hydra.kg.smsn`, ~169k atoms).
> Conventions and snippets reflect what worked there; reviewers familiar
> with other hosts should pull what generalizes and flag what doesn't.

## Prerequisites

- A Hydra schema you control, defined as one or more `Module`s. (See
  [Promoting raw code to Hydra modules](promoting-code.md) and
  [Generating code with Hydra](code-generation.md).)
- A working host environment for the language you want to ingest into.
  Python is used throughout this recipe; the structural story is the
  same for Java, Scala, etc.
- A JSON corpus you want to load. The recipe assumes one logical record
  per JSON object (e.g. one row per record, or one JSONL line per
  record); batched documents are a straightforward extension.

## What this recipe covers (and doesn't)

This recipe covers **structural** ingestion: producing typed host
values from JSON and proving they match your schema's shape. A decoder
roundtrip catches every record/union/wrap/optional/list/literal mismatch
between your JSON and your schema.

It does **not** cover **referential** validation — checking that a
`Relationship<Person>` actually points at an atom that is, semantically,
a Person and not (say) an Organization. That requires Hydra inference
and is out of scope here.

## Overview: two entry points

A typed host value sits at the end of either of these chains:

```
   instance JSON       Value           Term         typed host value
  (a Python dict,  →  (hydra.json.  →  (hydra.   →  (an instance of
   parsed from        model.Value,     core.Term,   your schema's
   the file)          a JSON AST)      Hydra's      generated Python
                                       universal    dataclass)
                                       term IR)
```

You can enter at either of two points:

- **JSON → Value → Term → typed value.** Faithful to the full picture.
  The Value and Value-to-Term layers live under `hydra.json.*` (parser
  combinators in `hydra.json.parser`, decoders for `Value` in
  `hydra.json.decoding`). This path needs the schema's `Type` ASTs
  loaded as data in the host, so the Value-to-Term step has something
  to drive it. Best fit for a long-running ingestion service that
  accepts arbitrary JSON.
- **Build the `Term` directly, then decode it.** Skip the JSON / Value
  layers entirely; emit the `hydra.core.Term` AST from your source
  representation and hand it straight to a generated decoder. Doesn't
  require the schema as runtime data — the generated decoder *is* the
  schema knowledge. Best fit for a one-shot migration, a CLI tool, or
  any case where you already understand the source shape and just want
  to project it into the schema. **This recipe takes this path**; the
  JSON / Value layers are listed for context only.

Both paths end with the generated decoder, which is the canonical
gate for "does this conform to my schema?"

## Step 1: generate decoders for your schema

The generated decoders live next to the type modules under
`hydra.decode.<your.namespace>.*`. There's one decoder function per
named type in your schema. Each has signature:

```python
def my_type(cx: hydra.graph.Graph,
            raw: hydra.core.Term) -> Either[hydra.errors.DecodingError, MyType]:
    ...
```

Decoders are not produced by their own writer function — they're
**synthesized** from your schema's type modules and then emitted by the
same Python writer that handles type modules. The pipeline has two
stages:

1. Synthesize `Hydra.Sources.Decode.<your.namespace>.*` source modules
   from your `Hydra.Sources.<YourSchema>.*` type modules.
   Done by `bootstrap-from-json --synthesize-sources`.
2. Emit Python from the union of (type modules) and (decoder source
   modules) via `writePython` (in `Hydra.ExtGeneration`).

See [Generating code with Hydra](code-generation.md) for the broader
context; the relevant detail there is the
[**writeXxx functions**](code-generation.md#the-writexxx-functions)
section and the `bootstrap-from-json` flag table.

### Path A: from JSON-exported modules (the standard pipeline)

If you've already followed
[Synchronizing Hydra-Python](syncing-python.md), your schema's
DSL-defined modules are exported as JSON under
`dist/json/<your-pkg>/src/main/json/`. From there, `bootstrap-from-json`
with `--synthesize-sources` will produce both type modules and decoder
modules under your output directory:

```bash
cd heads/haskell
stack build hydra:exe:bootstrap-from-json   # stack exec never rebuilds; build current source first
stack exec -- bootstrap-from-json \
  --target python \
  --output /abs/path/to/out/python \
  --package <your-pkg> \
  --synthesize-sources
```

The `stack build` matters: `stack exec` runs whatever binary is already in
`.stack-work` and never rebuilds, so without it you may silently run a stale
executable. The sync scripts (`bin/sync-python.sh` and friends) build first
automatically.

This is the same path the kernel itself uses to bootstrap; see
[`heads/haskell/src/exec/bootstrap-from-json/Main.hs`](../../heads/haskell/src/exec/bootstrap-from-json/Main.hs)
for the implementation, and `bin/sync-python.sh` for a worked example.

### Path B: directly from DSL modules (in-tree shortcut)

If you're working inside a Hydra worktree and your schema is already
defined as Haskell DSL modules under `packages/<your-pkg>/src/main/haskell/`,
you can skip the JSON export and call `writePython` from a one-file
Haskell driver. Decoder source modules are synthesized in the host using
`generateDecoderSourceModules` from `Hydra.Generation`:

```haskell
-- Generate.hs
module Main where

import Hydra.ExtGeneration (writePython)
import Hydra.Generation (generateDecoderSourceModules)
import qualified Hydra.Sources.YourSchema.All as Schema

main :: IO ()
main = do
  let universe       = Schema.allModules
  let typeMods       = Schema.typeModules     -- the subset with type defs
  decoderSrcs       <- generateDecoderSourceModules universe typeMods
  _ <- writePython "/abs/path/to/out/python" universe (typeMods ++ decoderSrcs)
  pure ()
```

`writePython` has signature
`FilePath -> [Module] -> [Module] -> IO [FilePath]`: the first
`[Module]` is the universe (dependency context — must include every
transitive dependency, even ones you aren't emitting), the second is
the subset to actually write out. The decoder source modules carry
their own `TypeScheme`, so they don't need to be re-typed by the host.

After either path you'll have:
- `<out>/hydra/your/schema/*.py` — dataclass type modules
- `<out>/hydra/decode/your/schema/*.py` — decoder modules

> Both paths feed the same generated-output shape. Path A is what
> `bin/sync-python.sh` runs in CI; Path B is convenient for one-off
> experiments where you don't want to keep the JSON export in sync.

## Step 2: set up `PYTHONPATH`

To import your decoders you need **three** roots on `PYTHONPATH`:

```
<hydra>/heads/python/src/main/python                  # hydra.dsl.python, hydra.json, etc.
<hydra>/dist/python/hydra-kernel/src/main/python      # hydra.core, hydra.graph, hydra.errors
<your-schema-out>/python                              # hydra.your.schema.*, hydra.decode.your.schema.*
```

All three are required: the first two ship the kernel and DSL; the
third is your generated code. Missing any one of them produces an
`ImportError` on import of `hydra.decode.<your>.<schema>.*`.

A `.envrc` (with [direnv](https://direnv.net)) or a wrapper script
keeps this from biting you on every invocation:

```bash
export PYTHONPATH="<hydra>/heads/python/src/main/python:<hydra>/dist/python/hydra-kernel/src/main/python:<schema-out>/python"
```

## Step 3: build an empty `Graph`

The generated decoders take a `Graph` as their first argument. For
ingestion that doesn't reference bound terms, primitives, or schema
types at decode time (which is the common case), an empty `Graph` is
sufficient. The constructor takes eight keyword arguments. Note the
mix of `FrozenDict` and `frozenset`:

```python
from hydra.dsl.python import FrozenDict
import hydra.graph as G

def empty_graph() -> G.Graph:
    return G.Graph(
        bound_terms=FrozenDict({}),
        bound_types=FrozenDict({}),
        class_constraints=FrozenDict({}),
        lambda_variables=frozenset(),   # set, not FrozenDict
        metadata=FrozenDict({}),
        primitives=FrozenDict({}),
        schema_types=FrozenDict({}),
        type_variables=frozenset(),     # set, not FrozenDict
    )

CX = empty_graph()
```

The `lambda_variables` / `type_variables` fields are typed as `Set`,
not `Mapping`, so a `FrozenDict({})` there will raise — pass
`frozenset()` instead.

## Step 4: how schema constructs serialize as `Term`s

This is the reference table that took me longest to assemble by hand,
so it's worth keeping close while you write encoders. Given a schema
construct on the left, you build the `Term` on the right.

| Schema construct | `Term` shape |
|---|---|
| `wrap Foo X` | `TermWrap(WrappedTerm(type_name=Name("…Foo"), body=<term for X>))` |
| `record Foo {a: A, b: B}` | `TermRecord(Record(type_name=Name("…Foo"), fields=(Field(name=Name("a"), term=<A>), Field(name=Name("b"), term=<B>))))` |
| `union Foo {x: X, y: Y}` | `TermInject(Injection(type_name=Name("…Foo"), field=Field(name=Name("x"), term=<X>)))` |
| `optional X` | `TermOptional(Given(<X>))` or `TermOptional(None_())` |
| `list X` | `TermList((<X1>, <X2>, …))` — **tuple**, not list |
| `set X` | `TermSet(frozenset({<X1>, <X2>, …}))` |
| `map K V` | `TermMap(FrozenDict({<K1>: <V1>, …}))` |
| `string` | `TermLiteral(LiteralString("…"))` |
| `boolean` | `TermLiteral(LiteralBoolean(True))` |
| `int32` | `TermLiteral(LiteralInteger(IntegerValueInt32(n)))` |
| `int64`, `bigint`, `uint16`, … | `TermLiteral(LiteralInteger(IntegerValue<Variant>(n)))` |
| `float64`, `float32` | `TermLiteral(LiteralFloat(FloatValue<Variant>(x)))` |
| `unit` | `TermUnit()` |

A few traps from the first time through:

- **Unions are `TermInject(Injection(…))`**, not `TermUnion` (no such
  constructor). The `Injection` carries the named variant *and* the
  union's `type_name`.
- **Optionals are `TermOptional(Given(...))` / `TermOptional(None_())`**.
  The present case is `Given`, the absent case `None_` (trailing underscore,
  since `None` is a Python keyword).
- **Integers and floats are doubly wrapped**: literal kind, then value
  variant. `LiteralInteger(IntegerValueInt32(n))`, not
  `LiteralInteger(n)`.
- **`Record.fields` is a tuple of `Field`**, not a dict. Field order
  doesn't have to match declaration order for decoding, but every
  declared field must appear (use `TermOptional(None_())` for absent
  optionals).
- **`TermList` wraps a `Sequence[Term]`** in the type signature, but
  pass an immutable `tuple(...)`. The kernel's own constructors and
  decoders are written against the dataclass being effectively frozen,
  and a plain Python `list` will hash-fail in any code path that
  treats the term as a key.

Small helpers go a long way. A useful starting set:

```python
import hydra.core as C
from hydra.dsl.python import FrozenDict, Given, None_

def t_str(s):     return C.TermLiteral(C.LiteralString(s))
def t_bool(b):    return C.TermLiteral(C.LiteralBoolean(b))
def t_i32(n):     return C.TermLiteral(C.LiteralInteger(C.IntegerValueInt32(n)))
def t_f64(x):     return C.TermLiteral(C.LiteralFloat(C.FloatValueFloat64(x)))
def t_optional(v): return C.TermOptional(Given(v) if v is not None else None_())
def t_list(xs):   return C.TermList(tuple(xs))

def t_wrap(type_name, body):
    return C.TermWrap(C.WrappedTerm(type_name=C.Name(type_name), body=body))

def t_record(type_name, fields):  # fields: dict[str, Term]
    return C.TermRecord(C.Record(
        type_name=C.Name(type_name),
        fields=tuple(C.Field(name=C.Name(k), term=v) for k, v in fields.items())))

def t_inject(type_name, variant_name, payload):
    return C.TermInject(C.Injection(
        type_name=C.Name(type_name),
        field=C.Field(name=C.Name(variant_name), term=payload)))
```

## Step 5: the fail-fast loop

The whole point of decoder-roundtrip is to surface a mismatch the
moment you produce one. Structure the ingestion as: encode → decode →
on `Left`, fix and try again.

```python
import hydra.dsl.python as dp
import hydra.decode.your.schema.my_module as dec

def ingest_one(raw_dict: dict) -> "MyType":
    term = encode_my_type(raw_dict)              # your encoder
    result = dec.my_type(CX, term)
    if isinstance(result, dp.Right):
        return result.value
    raise ValueError(f"decode failed: {result.value}")
```

A practical scaling pattern:

1. Start with **one** record from your hardest variant. Watch it decode
   to a `Right`.
2. Add five from each variant. Catch the per-variant boundary cases.
3. Bump to 100 / variant, then 1000, then the full corpus. Per-variant
   pass/fail counts make it obvious which encoder branch is wrong.

You're done when you can ingest the whole corpus with zero `Left`s.

## Step 6: cycles and the `Relationship<AtomId>` pattern

If your schema has mutual recursion through unions (say, `Note → Concept
→ Note`) the natural encoding can produce arbitrarily deep `Term`s for
what should be a reference. Two patterns help:

- **Narrow one end of the cycle to an opaque ID.** Replace
  `Relationship<Concept>` with `Relationship<AtomId>` (or whatever your
  identifier wrap is) on at least one side. The reference becomes a
  cheap wrap-of-string instead of a full record; you trade some
  static-type information for a finite, fast encode/decode. (Inference
  can restore the type information later.)
- **Build minimal placeholder records.** For `Relationship<Person>`,
  build the smallest valid `Person` record that names the target —
  identity plus nothing else — rather than recursively expanding the
  full target. Cheap, and the full record gets decoded when *its own*
  row is ingested.

## Step 7: monomorphic records with a union-typed field

A frequent design choice: should a "kind-of-thing" record be
polymorphic (`Work<Paper | Book | Film | …>`) or monomorphic with a
union-typed field (`Work` with `instance: WorkInstance`)?

In practice, monomorphic-with-a-union-field is much easier to ingest
into. Polymorphism propagates the type parameter through every consumer
of the record (and every encoder, decoder, and dataclass), enlarging
the surface area. A union-typed field localizes the variability to one
place: every consumer sees a single `Work` type, and only the code that
cares about the kind-specific details branches on the union.

This isn't ingestion-specific advice, but the choice is most visible
during ingestion: a polymorphic record forces you to thread the type
parameter through your encoder; a union-typed field doesn't.

## Worked example

The SmSn → `hydra.kg.smsn` migration is a reference implementation of
this recipe at non-trivial scale (~169k records, ~30 variant types).
The schema lives under
[`smsn/repositories/kg/schema/`](https://github.com/joshsh/smsn/tree/main/repositories/kg/schema)
(paths within that tree have shifted over time; browse from there).
Useful pieces to study, if still present at those paths:

- `Generate.hs` — the in-tree Path-B driver (see Step 1).
- `regenerate.sh` — the `PYTHONPATH` and build wiring.
- `migrate.py:empty_graph` — the empty-`Graph` recipe in real use.
- `migrate.py:t_str`/`t_record`/`t_inject`/… — the helper-function set.
- `migrate.py:encode_atom_to_term` — a dispatch-on-variant encoder.
- `migrate.py:main` — the scaling loop (5 → 100 → 1000 → all).

## Common pitfalls

- **`expected a record`** at the leaf of a `Relationship<T>` decode.
  You're passing a wrap (e.g. an `AtomId`) where the schema expects a
  full record. Either build a minimal record, or narrow the schema to
  `Relationship<AtomId>` (see Step 6).
- **`expected a string` / `expected an integer`** on a literal field.
  Off-by-one in the literal wrapping. Make sure you have *both* the
  literal kind (`LiteralInteger`) *and* the value variant
  (`IntegerValueInt32`).
- **`TypeError: missing keyword argument`** building the `Graph`.
  All eight fields are required, even on an empty graph. The
  `lambda_variables` / `type_variables` fields take `frozenset`, not
  `FrozenDict`.
- **`AttributeError: module 'hydra.core' has no attribute 'TermUnion'`**.
  Union variants are `TermInject(Injection(...))`. There is no
  `TermUnion`. Related: optionals are `TermOptional`, not `TermMaybe`
  (renamed in #401).
- **`ImportError: No module named hydra.decode.…`** at runtime.
  All three roots need to be on `PYTHONPATH` (see Step 2).

## Related documentation

- [Generating code with Hydra](code-generation.md) — the upstream step:
  schema → generated Python; covers the `writeXxx` functions and the
  `bootstrap-from-json` CLI used in Step 1.
- [Promoting raw code to Hydra modules](promoting-code.md) — turning
  ad-hoc code into a Hydra-managed schema in the first place.
- [Synchronizing Hydra-Python](syncing-python.md) — how the kernel
  Python distribution and your schema's generated code stay in lockstep
  across `bin/sync-python.sh` runs.
- [The Hydra JSON kernel](json-kernel.md) — the JSON representation
  Hydra uses to interchange modules between hosts. Useful context for
  Path A in Step 1 and for the `hydra.json.*` modules referenced in the
  overview.
- [Hydra implementation overview](../implementation.md) and the
  [Concepts wiki page](https://github.com/CategoricalData/hydra/wiki/Concepts) —
  if you'd like deeper grounding in what `Term`, `Type`, and `Graph`
  *are* before working through this recipe.
