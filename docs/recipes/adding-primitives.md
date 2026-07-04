# Adding new primitives to Hydra

A step-by-step guide for adding new primitive functions and constants to Hydra's standard library
across all implementations.

## Prerequisites

- Familiarity with Hydra's type system (see [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts))
- Understanding of the target language implementations (Haskell, Java, Python, Scala, Lisp)
- Knowledge of which library the primitive belongs to (e.g. `hydra.lib.strings`, `hydra.lib.math`)

**Important:** Every new primitive must include test cases in the common test suite.
Tests ensure consistent behavior across all language implementations and catch regressions.
Adding a primitive without tests is incomplete work.

## Overview

Primitive functions are the standard library of Hydra: bridges to the host language's
capabilities for operations that may not be expressible in Hydra's term language alone.

Each primitive has two faces:

1. **Universal metadata** (name, description, host-independent `comments` prose,
   signature, isPure / isTotal flags, and an optional cross-compilable reference
   implementation in Hydra terms), declared once in the kernel as a `PrimitiveDefinition`.
2. **Per-host implementation** — the actual native code that runs in each target language
   (Haskell, Java, Python, Scala, Lisp), paired with the universal metadata into a
   `Primitive` record at host-side registration time.

The kernel's `Hydra/Sources/Kernel/Lib/<sub>.hs` files **are** the primitive registry:
they enumerate every primitive in each `hydra.lib.<sub>` module name with its full
metadata. Host registrations look up that metadata by name and pair it with their
native implementation.

Most primitives are functions (*primitive functions*), but there are also *primitive
constants* like `hydra.lib.math.pi` and `hydra.lib.sets.empty`.

### When an operation should be a primitive

An operation only needs to be a *primitive* if it requires a native implementation —
for performance, or for access to host- or platform-specific capabilities. Anything
expressible in Hydra's own term language should be a **term definition** instead, not
a primitive.

The decision rule:

- **Make it a primitive** when there is no way to express it in Hydra terms, or no
  acceptable way: effectful operations (most of `hydra.lib.effects`, file and console
  I/O), host-native computation (arithmetic, character predicates, regex matching),
  and fundamental eliminators (`optionals.cases`, `pairs.first`) that bottom out the
  term language.
- **Make it a term definition** when it can be written in Hydra in terms of existing
  primitives and term constructs. For example, `fileName : FilePath -> FileName`
  (extract the last path component) is pure string manipulation — it can be a Hydra
  term definition rather than a primitive, even though it lives conceptually alongside
  the file API. The same goes for most path manipulation (`parentPath`, `withExtension`,
  `normalizePath`).

Performance *can* justify a primitive, but weigh it carefully. The goal is a balance: keep
the primitive set — the part of Hydra that every host must implement natively — as small as
possible, since each primitive is a burden on every current and future implementation; but
also give Hydra the capabilities and performance it needs when its programs are translated
into a wide variety of targets. An operation expressible in Hydra terms whose default
expression is fast enough should stay a term definition. One that is genuinely
performance-critical across hosts is a reasonable candidate for a primitive even though it
*could* be written in Hydra. And a user who needs a faster implementation of a particular
term-defined function in their own application can always override its definition with a
host-native UDF, without that function having to ship as a kernel primitive — so reserve
kernel primitives for operations that are broadly performance-critical, not merely faster
in one application.

A worked example is `copyFile : FilePath -> FilePath -> effect<either<FileError, unit>>`.
It *could* be written in Hydra as `readFile` followed by `writeFile`, so by the
expressibility test alone it would not need to be a primitive. But that composition forces
the entire file's contents to cross from the operating system into the host language's
runtime (marshaled into a string) and back out again, defeating OS-level fast paths that
copy bytes directly. It would also misbehave on binary files and hold the whole file in
memory. Here the performance and capability considerations vastly outweigh the preference
for minimalism, so `copyFile` is a primitive.

### No partial functions

Primitive functions must not encode partial operations that can fail for ordinary,
well-typed inputs.
Do not add primitives like `fromJust`, `head`, `tail`, or integer division as plain
functions whose only possible response to some inputs is a runtime failure.
Use result types that express failure, such as `optional<T>` or `either<E, T>`.

Examples:

- `maybeHead : list<a> -> optional<a>` rather than `head : list<a> -> a`
- `maybeTail : list<a> -> optional<list<a>>` rather than `tail : list<a> -> list<a>`
- `maybeDiv : int32 -> int32 -> optional<int32>` rather than `div : int32 -> int32 -> int32`

Hydra removed `hydra.lib.maybes.fromJust` as a primitive in v0.15.0 for this reason.
If a caller needs to collapse an optional value in a context where absence is impossible,
return `either<Error, T>` and propagate an informative error instead of hiding the
partiality in a primitive.

### Prefer core type constructors in signatures

Built-in primitive signatures should lean on core `Type` constructors, literal types,
and type variables bound by the primitive's own type scheme, rather than on named Hydra
types defined in model modules.
For example, a primitive may have a polymorphic signature such as
`forall a. a -> (a, a)`, but a signature such as `hydra.core.Term -> hydra.core.Type` —
referencing named types defined outside the primitive's own scheme — is something to
avoid where a core constructor will do.

This keeps primitive libraries independent of particular model modules and avoids
special cases where a primitive is only meaningful after resolving named type
definitions from the graph.
When an abstraction is foundational enough to appear in primitive signatures, prefer
adding it as a core type constructor rather than as a named type.
This is why `either<L, R>` replaced the old named `Flow` monad, and the same reasoning
makes effects a core `effect<T>` constructor rather than a named `hydra.effects.Effect<T>`
(see the [Effects](https://github.com/CategoricalData/hydra/wiki/Effects) wiki page).

This is a preference, not an absolute rule, and it applies to kernel primitives — not to
UDFs, which application developers routinely write against named types from their own
models.
A domain library may deliberately use named types in its signatures when they make the
intent clearer than bare core types would.
`hydra.lib.files` is the standard example: its operations take a `FilePath` and return a
`FileError` or `FileStatus` rather than passing raw `string`s and integers around, because
a small, self-describing POSIX vocabulary is worth the named-type dependency.
The judgement to make is whether the named type earns its keep, not whether named types
are forbidden.

### How the registry works

For each library module name, e.g. `hydra.lib.logic`, there is a kernel source module
`packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Logic.hs` that
declares every primitive in that module name as a `PrimitiveDefinition`:

```haskell
ns :: ModuleName
ns = ModuleName "hydra.lib.logic"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.logic module.")}
  where
    definitions = [and, ifElse, not_, or_]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

and :: PrimitiveDefinition
and = defineWithDefault "and" "Compute the logical AND of two boolean values."
  (sig $ TypeScheme [] (Types.boolean Types.~> Types.boolean Types.~> Types.boolean) Nothing)
  ["and(p, q) returns true iff both p and q are true. ..."]
  ("a" ~> "b" ~> Logic.ifElse (var "a") (var "b" :: TypedTerm Bool) false)

ifElse :: PrimitiveDefinition
ifElse = define "ifElse" "Compute a conditional expression."
  ifElseSig
  ["ifElse(p, t, f) returns t if p is true, or f if p is false. ..."]
```

Each primitive is a top-level `PrimitiveDefinition` value produced by one of two module-local helpers:

- **`define localName description signature comments`** — declares a primitive with no default
  implementation, for primitives that are fundamental (e.g. `logic.ifElse`, `pairs.first`)
  or whose meaning is host-native (e.g. arithmetic, char predicates, regex matching).
  `define = primitiveInModule module_` (or `impurePrimitiveInModule module_` for effectful
  primitives; see [Impure primitives](#impure-primitives) below).
- **`defineWithDefault localName description signature comments defaultBody`** — declares a
  primitive whose `defaultImplementation` is a pure Hydra-term expression.
  Used when the primitive can be defined in terms of other primitives.
  `defineWithDefault = primitiveWithDefaultInModule module_`.

The `comments` argument is `[String]` (a list of paragraphs). Pass `[]` for primitives whose
short `description` is self-explanatory, or a non-empty list to attach a longer,
host-independent specification — typically a paragraph citing the authoritative
external source (IEEE 754, Unicode, Haskell `Data.*`, etc.), characterizing
edge cases, and noting when behavior is host-defined. The `comments` field flows
through to the generated JSON kernel and is consumed by downstream documentation
and host bindings.

#### Writing the `comments` field

Conventions established across the 13 `hydra.lib.*` module names (#319):

- **Pick an authoritative source.** IEEE 754-2019 for floating-point operations
  (§5 for arithmetic + rounding, §9.2 for trig / exp / log); Unicode (general
  categories) for character predicates and case mappings; Haskell `Prelude` /
  `Data.Char` / `Data.List` / `Data.Map.Strict` / `Data.Set` / `Data.Either` /
  `Data.Maybe` for primitives without a normative external standard.
- **Hydra type names are lowercase in prose** (`int32`, `float64`, `boolean`,
  `optional`, `set`). Use Haskell type names (`Int32`, `Double`, `Bool`, `Maybe`)
  *only* inside `Corresponds to Haskell's <name> :: <Haskell-sig>` cross-references.
- **Do not mention Haskell typeclasses** (`Num`, `Floating`, `Ord`, `Enum`).
  Hydra does not have typeclasses. The closest Hydra concept is the per-type-var
  constraint set (e.g. `'ordering'`, `'equality'`); name those explicitly when
  relevant.
- **Special-value notation.** IEEE 754 sentinels compact: `±0`, `±∞`, `NaN`.
  Ranges in interval notation: `[0, π]`, `(-π/2, +π/2)`. Use unicode `±`, `∞`,
  `π`, `√`; encode as Haskell escape sequences (`\xB1`, `\x221E`, `\x03C0`,
  `\x221A`) to keep the source file ASCII-clean.
- **Boundary-case discipline.** If a value behaves surprisingly (e.g.
  `abs(minBound) = minBound`, `cos(±∞) = NaN`, `(−0) + (−0) = −0`), state it
  explicitly with the surprising value worked out.
- **Terminate every comment with the Haskell cross-reference**, in the form
  `Corresponds to Haskell's <name> :: <Haskell-sig>.` No trailing prose after
  the cross-reference. No class name (`from the Num class` and similar are
  removed).
- **Flag host-defined behavior** explicitly. Regex syntax, special-value
  literal capitalization (`"NaN"` vs `"nan"`), UTF-8 replacement policy on
  invalid bytes, etc. are typically host-defined; say so per primitive.
- **Populate `comments` for every primitive, even trivial ones.** A one-sentence
  comment for `negate` is still better than `Nothing` — it confirms the spec
  is intentional rather than skipped. Modeled on the all-primitives-covered
  pattern of `Math.hs` (45 primitives, all populated).

The `definitions` list holds `PrimitiveDefinition` values directly; the module wraps them with
`DefinitionPrimitive <$> definitions` so the `Module` record receives `[Definition]`.
This is distinct from modules that mix `Definition.term` and `Definition.type` entries —
primitive-only modules use `DefinitionPrimitive <$>` exclusively.

The generated JSON in `dist/json/hydra-kernel/src/main/json/hydra/lib/<sub>.json`
is the cross-host source of truth — every host language reads this to know what
primitives exist, their signatures, and their default implementations.

### What lives where

| Concern | Location |
|---------|----------|
| Universal primitive metadata (name, description, comments, type signature, purity flags, default implementation) | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/<Sub>.hs` |
| Native Haskell implementation | `overlay/haskell/hydra-kernel/src/main/haskell/Hydra/Overlay/Haskell/Lib/<Sub>.hs` (post-#501 `hydra.overlay.haskell.*` namespace) |
| Native Java implementation | `overlay/java/hydra-kernel/src/main/java/hydra/overlay/java/lib/<sub>/<FunctionName>.java` |
| Native Python implementation | `overlay/python/hydra-kernel/src/main/python/hydra/overlay/python/lib/<sub>.py` (#473 — impls live at `hydra.overlay.python.lib.*`) |
| Host-side primitive registry (binds names to native impls) | `overlay/haskell/hydra-kernel/src/main/haskell/Hydra/Overlay/Haskell/Libraries.hs` (Haskell, #473) and, per host, the registry in that host's overlay: `overlay/{java,python,scala,typescript}/hydra-kernel/.../overlay/<lang>/{lib,}/Libraries.<ext>`, and equivalent overlay paths for the four Lisp dialects |
| Phantom-typed DSL wrappers (for writing Hydra source modules) | **Generated** (#467): `hydra.dsl.lib.<sub>` in every target, e.g. `dist/haskell/.../Hydra/Dsl/Lib/<Sub>.hs` — no hand-written wrapper |
| Common test cases | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Test/Lib/<Sub>.hs` |

**Important:** the canonical metadata for every primitive — *including its name* — is the
`PrimitiveDefinition` in `Hydra/Sources/Kernel/Lib/<Sub>.hs`. All other files
(host registrations, DSL wrappers) **derive** the primitive's name from that definition
(via the generated `hydra.lib.*` def-modules) and do **not** re-declare the name, signature,
or description. There is no separate hand-maintained name index: the old
`Hydra.Sources.Kernel.Lib.Names` module and the `_<namespace>_<localName>` alias layer in
`Libraries.hs` were removed in #473.

### How primitive names flow

A primitive's name is declared once, in its kernel `PrimitiveDefinition`. From there (#473):

1. Code generation emits a per-host `hydra.lib.*` **def-module** for each kernel `Hydra/Sources/Kernel/Lib/<Sub>.hs`,
   carrying the `PrimitiveDefinition` (name + signature + metadata) as data. Defs live at `hydra.lib.<sub>`;
   the native implementations live alongside at `hydra.<lang>.lib.<sub>` (mirroring Haskell's
   `Hydra.Overlay.Haskell.Lib.*`).
2. Every host's primitive registry **derives** each name from that def-module rather than hand-writing a
   string. In Haskell this is fully implicit: the DSL builders (`primitive`, `primN`, `primCase`,
   `standardLibrary`) accept a `PrimitiveDefinition` directly via the `ToPrimName` class, so you write
   `prim1 DefChars.isAlphaNum ...` and `primitive1 DefChars.toLower` with no explicit name accessor. Other
   hosts derive the name with a `.name`/`prim-name` accessor over the loaded def — Python
   `def_chars.is_alpha_num.name`, Scala `hydra.lib.chars.isAlphaNum.name`, Java
   `hydra.lib.Chars.isAlphaNum().name`, and the lisp dialects via `prim-name`.

The upshot: there is exactly one place a primitive name is written down (the kernel `PrimitiveDefinition`),
and a rename there propagates to all hosts through regeneration.

### Naming conventions

Primitive functions conform to the case convention of the implementation language:

- **Haskell**: camelCase (e.g. `isAlphaNum`, `toLower`)
- **Java**: PascalCase for class names (e.g. `IsAlphaNum`, `ToLower`)
- **Python**: snake_case (e.g. `is_alpha_num`, `to_lower`)

The native Hydra name is always camelCase (e.g. `hydra.lib.chars.isAlphaNum`),
regardless of the implementation language.

### Default implementations

A primitive's `defaultImplementation : Maybe Term` is a declarative, cross-compilable
expression in Hydra terms whose type matches the primitive's signature. Two roles:

- **Fallback for minimal interpreters.** A host implementation that doesn't ship
  a native impl for a primitive can fall back to evaluating the default Hydra term.
- **Proof-friendly reference.** Targets that can prove or simulate the default
  body (e.g. Coq) get a verified reference implementation for free.

Default implementations are pure Hydra expressions — they take only the primitive's
declared arguments (no `Context`, no `Graph`) and reduce using only other
primitives and term-level constructs. For example:

```haskell
-- optionals.fromOptional def m = cases m def (\x -> x)
fromOptional :: PrimitiveDefinition
fromOptional = defineWithDefault "fromOptional" "Return the contained value or a default."
  fromOptionalSig
  ["fromOptional(def, m) returns the value inside m if present, or def if m is none."]
  ("def" ~> "m" ~> Optionals.cases (var "m") (var "def") ("x" ~> var "x"))
```

Not every primitive has a meaningful default. Fundamental operations
(`optionals.cases`, `pairs.first`, character predicates, arithmetic) cannot be
expressed in terms of other primitives — those use `define` (no default) and rely on the
host's native implementation.

### Impure primitives

Primitives whose result type is `effect<t>` — i.e. those in `hydra.lib.effects`,
`hydra.lib.files`, and `hydra.lib.system` — are declared with `impurePrimitiveInModule`
instead of `primitiveInModule`:

```haskell
define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = impurePrimitiveInModule module_
```

The only difference is that `impurePrimitiveInModule` sets `primitiveDefinitionIsPure = False`
on the resulting `PrimitiveDefinition`. Default implementations (`primitiveWithDefaultInModule`)
are not meaningful for impure primitives and are not used in practice. The `comments` field
convention follows the same rules as for pure primitives, but cites POSIX or platform docs
(e.g. the Open Group Base Specifications) as the authoritative source rather than Haskell `Data.*`.

## Adding a primitive: end-to-end recipe

The order is: **kernel metadata first**, then native implementations, then host
registrations, then DSL wrappers, then tests.

### 1. Declare the primitive's metadata (this is where its name is defined)

There is no separate name-constant step. A primitive's name — like its signature and
documentation — is declared exactly once, in its `PrimitiveDefinition` in the kernel
`Hydra/Sources/Kernel/Lib/<Sub>.hs` module (next step). Code generation emits that into the
per-host `hydra.lib.*` def-modules, and every host registry **derives** the name from there
(see [How primitive names flow](#how-primitive-names-flow)). So just proceed to declaring the
metadata — the name comes for free.

### 2. Declare the primitive's metadata

Open the corresponding `Hydra/Sources/Kernel/Lib/<Sub>.hs` file (e.g.
`Hydra/Sources/Kernel/Lib/Chars.hs`) and add a new entry to the `definitions` list,
plus its signature, and (optionally) its default implementation.

For a simple monomorphic primitive without a default:

```haskell
define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

isAlphaNum :: PrimitiveDefinition
isAlphaNum = define "isAlphaNum" "Check whether a character is alphanumeric."
  (sig $ TypeScheme [] (Types.int32 Types.~> Types.boolean) Nothing)
  ["True if the argument is a Unicode letter or digit, false otherwise. ..."]
```

For a primitive with a default implementation expressed in terms of other primitives:

```haskell
defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

bimap :: PrimitiveDefinition
bimap = defineWithDefault "bimap" "Map over both elements of a pair."
  (sig $ TypeScheme [Name "a", Name "b", Name "c", Name "d"]
    ((Types.var "a" Types.~> Types.var "c") Types.~>
     (Types.var "b" Types.~> Types.var "d") Types.~>
     Types.pair (Types.var "a") (Types.var "b") Types.~>
     Types.pair (Types.var "c") (Types.var "d"))
    Nothing)
  ["bimap(f, g, p) returns a new pair (f(first(p)), g(second(p))). ..."]
  ("f" ~> "g" ~> "p" ~> pair (var "f" @@ Pairs.first (var "p"))
                              (var "g" @@ Pairs.second (var "p")))
```

The `TypeScheme`'s variable list must be in **declaration order** — the order
the variables first appear in the body — **not alphabetical**. Hosts that
re-register primitives by hand (e.g. `heads/typescript/.../lib/libraries.ts`,
`overlay/java/hydra-kernel/.../overlay/java/lib/Libraries.java`) must follow the same order: kernel
typeApps bind positionally to that list, and mis-ordering silently swaps
domain and codomain in inferred lambda types. For example, `optionals.fromOptional :
a → optional<a> → a` lists vars as `[a]`; a polymorphic primitive whose result
type variable appears before its argument's must list them in that body order, not
alphabetically.

For a constrained polymorphic primitive (e.g. requires `ordering`):

```haskell
compare :: PrimitiveDefinition
compare = define "compare" "Compare two values and return a Comparison."
  (sig $ Types.polyConstrained [("x", [Name "ordering"])]
    (Types.var "x" Types.~> Types.var "x" Types.~> Types.var "hydra.util.Comparison"))
  ["compare(x, y) returns the hydra.util.Comparison value classifying the relationship between x and y. ..."]
```

**Definitions must be in alphabetical order by primitive name.** The kernel
validator enforces this.

### 3. Implement natively in each host

#### Haskell

Add the implementation in `overlay/haskell/hydra-kernel/src/main/haskell/Hydra/Overlay/Haskell/Lib/<Library>.hs` (#418):

```haskell
-- Hydra/Overlay/Haskell/Lib/Chars.hs
isAlphaNum :: Int -> Bool
isAlphaNum = C.isAlphaNum . C.chr
```

Then register it in `overlay/haskell/hydra-kernel/src/main/haskell/Hydra/Overlay/Haskell/Libraries.hs`:

```haskell
hydraLibChars :: Library
hydraLibChars = standardLibrary [
  prim1 DefChars.isAlphaNum Chars.isAlphaNum [] int32 boolean,
  ...]
```

The `prim1` / `prim2` / `prim3` helpers bind a primitive to a native implementation. They take the
generated `PrimitiveDefinition` directly (`DefChars.isAlphaNum`, where `DefChars` is the generated
`Hydra.Lib.Chars` def-module) — the single source of truth for the name. `standardLibrary` derives the
library's module name from the first primitive, so it needs no `ModuleName` argument. The type information
passed to `primN` is a sanity-check repetition the host registry needs in native form, not the source of
truth for the name. (Under the hood, `primN`/`primitive`/`standardLibrary` accept a `PrimitiveDefinition`
through the `ToPrimName` class; passing a bare `Name` still works for the rare site that has only a name.)

#### Per-parameter signature metadata (e.g. laziness)

Beyond types, a primitive's `TermSignature` carries per-parameter metadata such as
`isLazy` (whether coders must thunk that argument; see
[Lazy evaluation and thunking](new-implementation.md#lazy-evaluation-and-thunking)).
Such metadata lives in **three** places that must agree, because the in-memory graph
the coders consult at generation time (`bootstrapGraph`) is built from the hand-written
registry, **not** from the DSL sources or JSON:

1. the kernel DSL signature in `Hydra.Sources.Kernel.Lib.*` (e.g. via `lazySig [positions]`);
2. the regenerated `dist/json/hydra-kernel/.../lib/*.json` (produced from 1);
3. the `prim*` call site in `Hydra.Sources.Libraries` — wrap it to attach the metadata
   (e.g. `Prims.lazyArgs [0,1] $ prim3 ...`). This is what reaches `bootstrapGraph`.

One further subtlety: the kernel adapts each primitive's signature to the target language
before code generation. Adaptation must preserve this per-parameter metadata — it does so
via `adaptTermSignature`, which rewrites parameter/result *types* in place. Do **not**
route a signature through `TypeScheme` (`termSignatureToTypeScheme` / `typeSchemeToTermSignature`)
to transform it: `TypeScheme` is the type-only view and that round-trip silently erases
parameter names, descriptions, and `isLazy`.

#### Default term-level implementations

The "default implementation" recorded on every `PrimitiveDefinition` is a
pure Hydra-term reference — directly executable by any host with a term
reducer, no native call required. It is the last argument to `defineWithDefault`
(see Step 2 for an example).

A default term must type-check at the primitive's public signature.
Express it using only other primitives and term-level constructs; do
not reach for `InferenceContext`, `Graph`, or `ExtractCore.*` — the
interpreter strips and reduces arguments before calling the primitive,
so the default body sees ordinary terms of the declared type.

Higher-order operations (`lists.foldl`, `eithers.bimap`, etc.) are
typically expressible this way using `foldr`/`foldl`/`cases`/`cons`/etc.
For example, `eithers.lefts` from the canonical Eithers registry:

```haskell
-- lefts xs = foldr (\e acc -> either (\l -> cons l acc) (\_ -> acc) e) [] xs
lefts_ :: TypedTermDefinition ([Either a b] -> [a])
lefts_ = define "lefts" $
  doc "Extract Left values, defined in terms of either + foldr." $
  "xs" ~>
    Lists.foldr
      ("e" ~> "acc" ~>
        Eithers.either_
          ("l" ~> Lists.cons (var "l") (var "acc" :: TypedTerm [a]))
          ("_" ~> (var "acc" :: TypedTerm [a]))
          (var "e"))
      (list ([] :: [TypedTerm a]))
      (var "xs")
```

There is no separate "interpreter-shape" Defaults module any more
(issue #437). Either a primitive has a portable default that fits its
public signature — write it inline with `defineWithDefault` — or it doesn't,
and the registry entry uses `define`. `define` is the right choice for:
fundamental eliminators (`optionals.cases`, `eithers.either`,
`pairs.first`, `lists.foldl`, `logic.ifElse`), host-native operations
(arithmetic, regex, literal parsing), and any primitive whose only
sensible implementation would have to reference itself.

Host-side registrations are unchanged regardless of which kind of
default is used: each primitive is bound with `prim1` / `prim2` /
`prim3`, passing the generated `PrimitiveDefinition` directly
(`prim1 DefChars.isAlphaNum ...`) so the name comes from that single source.

#### Java

Create the per-primitive class
`overlay/java/hydra-kernel/src/main/java/hydra/overlay/java/lib/<library>/<FunctionName>.java` (#418):

```java
package hydra.lib.chars;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
import hydra.util.Either;

public class IsAlphaNum extends PrimitiveFunction {
    public Name name() { return new Name("hydra.lib.chars.isAlphaNum"); }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), boolean_()));
    }

    @Override
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph ->
            hydra.lib.eithers.Map.apply(
                c -> Terms.boolean_(apply(c)),
                hydra.extract.Core.int32(graph, args.get(0)));
    }

    public static boolean apply(int codePoint) {
        return Character.isLetterOrDigit(codePoint);
    }
}
```

**Structure:**
- `name()`: returns the fully qualified Hydra name.
- `type()`: declares the type scheme (use type parameters for polymorphic functions).
- `implementation()`: extracts arguments via `hydra.extract.Core.*` and wraps
  the result with `Terms.*`. The shape is `args -> cx -> graph -> Either<Error_, Term>`.
- `apply()`: static Java method(s) for direct host-language calls.

Note: the error type is `hydra.errors.Error_` (trailing underscore) — Hydra-side
`hydra.core.Error` would clash with `java.lang.Error`.

**Higher-order primitives in Java:** when the primitive takes function arguments,
the `implementation()` method must use `Reduction.reduceTerm(cx, graph, eager, term)`
to evaluate function applications. See `overlay/java/hydra-kernel/src/main/java/hydra/overlay/java/lib/lists/Foldr.java`
for an example that iterates in reverse and reduces on each step.

Then register the new primitive in
`overlay/java/hydra-kernel/src/main/java/hydra/overlay/java/lib/Libraries.java` (#418) by adding it to its
category's list (`charsPrimitives()` in this case). The category will already
be enumerated in `standardPrimitives()`.

```java
private static List<PrimitiveFunction> charsPrimitives() {
    return Arrays.asList(
        new IsAlphaNum(),
        new IsLower(),
        // ...
        new ToUpper());
}
```

`PrimitiveFunction.toNative()` builds a `PrimitiveDefinition` from the host-side
`name()` and `type()` (via `Scoping.typeSchemeToTermSignature`) and pairs it
with the host implementation. Hand-written PrimitiveDefinition constructions
in the Java head use the kernel's current 10-field shape; the helper does the
expansion for you.

#### Python

Add the implementation in `overlay/python/hydra-kernel/src/main/python/hydra/overlay/python/lib/<library>.py` (#418):

```python
def is_alpha_num(value: int) -> bool:
    """Check whether a character (as int code point) is alphanumeric."""
    return chr(value).isalnum()
```

Then register it in the Python source registry at
`overlay/python/hydra-kernel/src/main/python/hydra/overlay/python/sources/libraries.py` (#418). Each module name has
its own `register_<sub>_primitives()` function that returns a
`dict[Name, Primitive]`; add an entry there using `prims.prim1` /
`prims.prim2` / `prims.prim3` to match the kernel signature:

```python
def register_chars_primitives() -> dict[Name, Primitive]:
    from hydra.lib import chars

    namespace = "hydra.lib.chars"
    primitives: dict[Name, Primitive] = {}

    primitives[qname(namespace, "isAlphaNum")] = prims.prim1(
        qname(namespace, "isAlphaNum"), chars.is_alpha_num, [],
        prims.int32(), prims.boolean())
    # ... other chars primitives
    return primitives
```

The `prims` helpers (from `hydra.dsl.prims`) provide the same first-order
argument list — primitive Name, native callable, type-variable list, then
argument and result `TermCoder`s. See the existing entries for examples of
polymorphic primitives and higher-order patterns (`fun(dom, cod)` for
function-typed arguments).

### 4. DSL wrappers are generated — no manual step

The typed DSL wrapper for each primitive is *generated*, not hand-written (#467).
Because the `hydra.lib.*` modules are part of the kernel's `mainDslModules` set, the sync emits one
`hydra.dsl.lib.<library>` module per library into every target language
(Haskell `Hydra.Dsl.Lib.Chars`, Java `hydra.dsl.lib.Chars`, Python `hydra.dsl.lib.chars`, ...).
Each primitive projects to a typed, arity-aware reference builder derived from its declared
signature, e.g. `Hydra.Dsl.Lib.Chars.isAlphaNum :: TypedTerm Int -> TypedTerm Bool`.

So once the primitive definition (step 1) is in place, the wrapper appears automatically on the next
sync; there is nothing to write.
Hydra source modules call these generated wrappers to construct phantom-typed applications of the
primitive.

Two things to know at call sites (see the [DSL guide](../dsl-guide.md) for detail):
- Generated wrappers carry the primitive's type-class constraints (e.g. `Ord` for map/set keys), so
  polymorphic call sites may need concrete type annotations.
- They do not auto-lift definitions; a definition passed as a first-class argument needs `asTerm`.

### 5. Add common test cases

Tests live in `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Test/Lib/<Library>.hs`:

```haskell
charsIsAlphaNum :: TypedTerm TestGroup
charsIsAlphaNum = subgroup "isAlphaNum" [
  test "lowercase letter" (ord 'a') true,
  test "uppercase letter" (ord 'Z') true,
  test "digit"            (ord '5') true,
  test "space"            (ord ' ') false]
  where
    test name x result = primCase name DefChars.isAlphaNum [int32 x] result
```

Then add the test group to `allTests` in the same file.

### 6. Regenerate and run tests

```bash
bin/sync.sh
```

This regenerates JSON, downstream Haskell, and runs `stack test`. To validate
specific hosts:

```bash
(cd heads/java && ./gradlew :hydra-java:test)
cd heads/python && pytest
```

### 7. Verify your tests actually ran

```bash
# Haskell
cd heads/haskell
stack test 2>&1 | grep -i '<your-primitive-name>'

# Java
(cd heads/java && ./gradlew :hydra-java:test --tests "*TestSuiteRunner*" --info) 2>&1 | grep -i '<your-primitive-name>'

# Python
cd heads/python && pytest -v 2>&1 | grep -i '<your_primitive_name>'
```

If your test cases don't appear, the test group wasn't registered or the test
JSON wasn't regenerated.

## Checklist

When adding a new primitive function:

- [ ] **Kernel metadata** (required — this is where the name is declared, once)
  - [ ] Top-level `PrimitiveDefinition` via `define` or `defineWithDefault` in `Hydra.Sources.Kernel.Lib.<Library>`
  - [ ] Added to the module's `definitions` list (alphabetical order)
  - [ ] Signature as a `TermSignature` (via `sig $ TypeScheme ...` or `lazySig [...] $ TypeScheme ...`)
  - [ ] **(If applicable)** Default implementation inline as the last argument to `defineWithDefault`
- [ ] **Haskell**
  - [ ] Native implementation in `Hydra.Overlay.Haskell.Lib.<Library>`
  - [ ] Registration via `primN Def<Library>.<fn> ...` in `Hydra.Dsl.Libraries`
  - [ ] DSL wrapper in `Hydra.Dsl.Meta.Lib.<Library>`
- [ ] **Java**
  - [ ] `PrimitiveFunction` class in `hydra.lib.<library>` (its `name()` returns `hydra.lib.<Lib>.<fn>().name`)
  - [ ] Registration in `Libraries.java`
- [ ] **Python**
  - [ ] Function in `hydra.python.lib.<library>`
  - [ ] Registration in `hydra.sources.libraries` (name via `def_<library>.<fn>.name`)
- [ ] **Common test suite** (required)
  - [ ] Test group added to `Hydra.Sources.Test.Lib.<Library>`
  - [ ] Test group registered in `allTests`
- [ ] **Tests pass** in all three languages

## Renaming or removing a primitive

Modifying an existing primitive — renaming, removing, or changing its signature — is a
**rare task**, on a par with
[revising Hydra Core](extending-hydra-core.md): the kernel's primitive set is a stable
interface that downstream code, generated artifacts, and every host runtime depend on,
so changes ripple widely. Treat it deliberately rather than as a routine edit.

Renaming or removing a primitive is the inverse of the recipe above: undo every site
in the [Checklist](#checklist) (kernel `definitions` entry, default
implementation, each host's native impl + registration + DSL wrapper, and the common
test group). Because every host derives the name from the kernel `PrimitiveDefinition`,
a rename starts there and flows out through regeneration — there is no separate name
constant to update. Two things deserve special care.

**Flip every call site, not just the definition.** Removing a primitive in favour of
another (e.g. the #401 replacement of the value-first `optionals.maybe` eliminator with
the scrutinee-first `optionals.cases`) means rewriting every reference, including ones that
do not match a simple text search: DSL call sites (`Optionals.maybe`), name-binding
references (`_optionals_maybe`), hardcoded `Name "hydra.lib.optionals.maybe"` literals in
phantom helpers, and the per-host runtime registries (Java `Libraries.java`, Python
`hydra.sources.libraries`, the four Lisp `lib/libraries.*`, the TypeScript
`lib/libraries.ts`, and the per-host test runners). When the replacement has a
different argument order, each call site must be reordered, not just renamed.

**Manually invalidate the synthesis cache.** This is the non-obvious step. The
synthesized decode/encode JSON modules (`dist/json/<pkg>/.../hydra/decode/*.json`)
embed the *names* of the primitives their generated terms reference. Their freshness
check keys on the source module's *type shape*, not on the emitted content — so a
primitive rename, which leaves the type shape unchanged, is **not** detected, and the
sync leaves these artifacts stale. (Primitives change rarely enough that automatic
invalidation is not worth building; invalidate by hand on the rare occasion.)

The symptom of skipping this is a sync that fails one stale-artifact layer deeper each
run, with `no such binding: <prim>` during Haskell per-package inference, or
`untyped term variable: <prim>` during Java/Python generation. To force a clean
regeneration:

```bash
# 1. Delete the stale synthesized decode/encode JSON artifacts:
grep -rl "hydra.lib.<old.prim.name>" dist/json/*/src/main/json/hydra/{decode,encode} | xargs rm -f
# 2. Clear the Phase-1 / bootstrap-from-json caches:
rm -f heads/haskell/.stack-work/{phase1-input-cache,bootstrap-from-json-cache}.txt
# 3. Re-export, including the cold-start java/python coder JSON (which also caches):
HYDRA_INCLUDE_JAVA_PYTHON=1 ./bin/sync.sh
```

For the cache layers involved see [the build system](../build-system.md);
for the matching "untyped bindings" / stale-JSON failure mode see
[troubleshooting](../troubleshooting.md).

## Common pitfalls

1. **Out-of-order definitions.** The kernel validator requires alphabetical
   order by primitive name within each module's `definitions` list.
   `bigintToInt8` comes *after* `bigintToInt64` lexically (`8` > `6`).

2. **Missing `description`.** Every `PrimitiveDefinition` must have a
   non-empty description string; the documentation validator rejects empty ones.

3. **Signature mismatch across hosts.** The canonical signature is in
   `Hydra.Sources.Kernel.Lib.<Library>`. Host-side registrations (Haskell
   `prim1`/`prim2`, Java `type()`, Python registry) should agree with it.
   Mismatches show up as inference failures during `bin/sync.sh`.

4. **Circular default implementations.** A primitive cannot use *itself* in its
   `defaultImplementation` body — there's no base case for reduction. If the
   primitive has no expression in terms of *other* primitives (e.g. `pairs.first`,
   `logic.ifElse`), use `define` (no default) rather than `defineWithDefault`.

5. **Type-variable naming.** The canonical type-variable names come from
   `Hydra.Sources.Libraries` (`_x`, `_xOrd`, `_xEq`, etc., resolving to `TypeVar`
   values). Use the same names in your `TermSignature` to keep host signatures
   in sync.

6. **Polymorphic primitives over containers.** When the primitive involves
   `Set` or `Map`, the relevant type variable usually carries an `ordering`
   constraint (e.g. `_sets_member` uses `[_xOrd]`, not `[_x]`). Use
   `Types.polyConstrained` to express the constraint.

7. **`binary` has a different host representation in every target.** A native
   impl that takes or returns the `binary` type must use the representation that
   the *coder for that host* emits, not a uniform byte buffer. Verified mappings:

   | Host | `binary` value representation |
   |------|------------------------------|
   | Haskell | `ByteString` |
   | Java | `byte[]` |
   | Scala | base64-encoded `String` (decode with `java.util.Base64`) |
   | Python | `bytes` |
   | Clojure | vector of byte ints `[65 66]` (→ `(byte-array (map unchecked-byte …))`) |
   | Scheme | Scheme vector `#(65 66)` (not a bytevector — convert before I/O) |
   | Common Lisp | vector `#(65 66)` |
   | Emacs Lisp | elisp vector `[65 66]` |

   The symptom of getting this wrong is not a compile error but a silently empty
   or wrong result at runtime (e.g. a file write that produces no bytes). Confirm
   the representation by reading a generated test that passes a `binary` literal,
   or that host's `hydra.lib.literals` runtime (`binaryToBytes`/`stringToBinary`).
   The registered *type scheme* still uses the abstract kernel `binary` type; only
   the native impl's parameter/return type changes.

## Adding a new type constructor (kernel core extension)

Most primitive work reuses existing types. Adding a *new* type constructor to
`hydra.core.Type` (as `effect<t>` was added for `hydra.lib.effects`/`hydra.lib.files`)
is a heavier, cross-cutting change with steps the per-primitive recipe does not cover:

- **Bootstrap the core type.** Extending `Type` is circular (the kernel describes
  itself); see [extending-hydra-core.md](extending-hydra-core.md).
- **Teach every host's coder to encode it.** Each host coder must handle the new
  variant or code generation crashes with a non-exhaustive-case error. For a
  *transparent* wrapper like `effect<t>` (which encodes as its inner type `t`),
  add a recursing case to the host's `encodeType` — and to any other full
  type-walkers (the Java coder needed it in `applySubstFull` and two more
  substitution/collection helpers; missing one leaks an uninstantiated type
  variable into generated code). The host `Language` must also list the variant.
- **Add it to the type adapter** (`Hydra/Sources/Kernel/Terms/Adapt.hs`
  `typeAlternatives`) so targets without native support get a fallback.
- **Watch the published-host gate.** A backward-incompatible core change means no
  published host can compile the current DSL sources; build the whole branch with
  `--local-host` (see [build-system.md](../build-system.md) and
  [migration-shims.md](migration-shims.md)).
- **Bust coder caches when iterating on a host coder.** The Java/Python native
  coder freshness check keys on the kernel JSON, not the coder DSL source, so an
  edit to a host coder can be silently cache-skipped; clear
  `heads/haskell/.stack-work/{bootstrap-from-json,java-python-json}-cache.txt`
  and the relevant `dist/<lang>/<pkg>/build/**/digest.json` before re-syncing.

## Higher-order primitives

Primitives that take function arguments (e.g. `lists.map`, `optionals.bind`) are
called higher-order. They have the same shape as other primitives at the
metadata level — they're declared with a function-type signature. Two
implementation notes:

- **In native impls**, the function argument arrives as a Hydra `Term`. To
  apply it, the host uses its term-reduction machinery (Haskell:
  `Reduction.reduceTerm`; Java: `Reduction.reduceTerm()`). See
  `overlay/haskell/hydra-kernel/src/main/haskell/Hydra/Overlay/Haskell/Lib/Lists.hs` for the Haskell pattern
  and `overlay/java/hydra-kernel/src/main/java/hydra/overlay/java/lib/lists/Foldr.java` for the Java
  pattern.

- **In default implementations**, the function argument is just a TypedTerm —
  apply it with `@@` in Haskell DSL:

  ```haskell
  -- optionals.map f m = cases m none (\x -> given (f x))
  map_ :: TypedTermDefinition ((a -> b) -> Maybe a -> Maybe b)
  map_ = define "map" $
    doc "Map a function over an optional, defined in terms of cases." $
    "f" ~> "m" ~> Optionals.cases (var "m") nothing ("x" ~> just (var "f" @@ var "x"))
  ```

## Example: studying an existing migration

The `hydra.lib.optionals` module name is a good case study: 12 primitives, most of
which have default implementations in terms of `cases`. See:

- Metadata: [Hydra/Sources/Kernel/Lib/Optionals.hs](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Optionals.hs)
- Haskell native impl: [Hydra/Overlay/Haskell/Lib/Optionals.hs](https://github.com/CategoricalData/hydra/blob/main/overlay/haskell/hydra-kernel/src/main/haskell/Hydra/Overlay/Haskell/Lib/Optionals.hs)
- Haskell phantom-typed DSL infrastructure: [Hydra/Overlay/Haskell/Dsl/Typed/](https://github.com/CategoricalData/hydra/tree/main/overlay/haskell/hydra-kernel/src/main/haskell/Hydra/Overlay/Haskell/Dsl/Typed)
- Java native impls: [hydra/overlay/java/lib/optionals/](https://github.com/CategoricalData/hydra/tree/main/overlay/java/hydra-kernel/src/main/java/hydra/overlay/java/lib/optionals)

## Further reading

- [Testing documentation](https://github.com/CategoricalData/hydra/wiki/Testing) — common test suite
- [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) — Hydra's type system and data structures
- [Creating a new Hydra implementation](new-implementation.md) — adding an entirely new language target
