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
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.logic namespace."}
  where
    definitions = [
      toPrimitive "Compute the logical AND of two boolean values." andSig (Just
        "and(p, q) returns true iff both p and q are true. ...") and_,
      primNoDef "ifElse" "Compute a conditional expression." ifElseSig (Just
        "ifElse(p, t, f) returns t if p is true, or f if p is false. ..."),
      toPrimitive "Compute the logical NOT of a boolean value." notSig Nothing not_,
      toPrimitive "Compute the logical OR of two boolean values." orSig Nothing or_]

andSig :: TermSignature
andSig = sig $ TypeScheme [] (Types.boolean Types.~> Types.boolean Types.~> Types.boolean) Nothing

and_ :: TypedTermDefinition (Bool -> Bool -> Bool)
and_ = define "and" $
  doc "Logical AND, defined in terms of ifElse." $
  "a" ~> "b" ~> Logic.ifElse (var "a") (var "b" :: TypedTerm Bool) false
```

The two helpers used here:

- **`toPrimitive description signature comments defaultBody`** — declares a primitive
  whose `defaultImplementation` is a pure Hydra-term expression (the `TypedTermDefinition`
  body). Used when the primitive can be defined in terms of other primitives.
- **`primNoDef localName description signature comments`** — declares a primitive
  with no default implementation, for primitives that are fundamental (e.g.
  `logic.ifElse`, `pairs.first`) or whose meaning is host-native (e.g. arithmetic,
  char predicates, regex matching).

The `comments` argument is `Maybe String`. Pass `Nothing` for primitives whose
short `description` is self-explanatory, or `(Just "...")` to attach a longer,
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
  `maybe`, `set`). Use Haskell type names (`Int32`, `Double`, `Bool`, `Maybe`)
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

Both helpers produce a `Definition.primitive PrimitiveDefinition` entry which is
then enumerated alongside `Definition.term` and `Definition.type` in the module's
`moduleDefinitions` list.

The generated JSON in `dist/json/hydra-kernel/src/main/json/hydra/lib/<sub>.json`
is the cross-host source of truth — every host language reads this to know what
primitives exist, their signatures, and their default implementations.

### What lives where

| Concern | Location |
|---------|----------|
| Universal primitive metadata (name, description, comments, type signature, purity flags, default implementation) | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/<Sub>.hs` |
| Interpreter-friendly term-level reference impls (for higher-order primitives) | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Defaults/<Sub>.hs` |
| Primitive-name `Name` constants (`charsIsAlphaNum`, `logicAnd`, ...) | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Names.hs` |
| Legacy `_<namespace>_<localName>` aliases (`_chars_isAlphaNum`, `_logic_and`, ...) used by host registrations | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Libraries.hs` |
| Native Haskell implementation | `heads/haskell/src/main/haskell/Hydra/Haskell/Lib/<Sub>.hs` |
| Native Java implementation | `heads/java/src/main/java/hydra/lib/<sub>/<FunctionName>.java` |
| Native Python implementation | `heads/python/src/main/python/hydra/lib/<sub>.py` |
| Host-side primitive registry (binds names to native impls) | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Libraries.hs` (Haskell) and `heads/<lang>/.../lib/Libraries.<ext>` per host |
| Phantom-typed DSL wrappers (for writing Hydra source modules) | `heads/haskell/src/main/haskell/Hydra/Dsl/Meta/Lib/<Sub>.hs` |
| Common test cases | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Test/Lib/<Sub>.hs` |

**Important:** the canonical metadata for every primitive is the
`PrimitiveDefinition` in `Hydra/Sources/Kernel/Lib/<Sub>.hs`. All other files
(host registrations, DSL wrappers) reference primitives by name; they do **not**
re-declare the signature or description.

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
-- maybes.fromMaybe def m = maybe def (\x -> x) m
fromMaybe_ :: TypedTermDefinition (a -> Maybe a -> a)
fromMaybe_ = define "fromMaybe" $
  doc "Return the contained value or a default, defined in terms of maybe." $
  "def" ~> "m" ~> Maybes.maybe (var "def") ("x" ~> var "x") (var "m")
```

Not every primitive has a meaningful default. Fundamental operations
(`maybes.maybe`, `pairs.first`, character predicates, arithmetic) cannot be
expressed in terms of other primitives — those use `primNoDef` and rely on the
host's native implementation.

## Adding a primitive: end-to-end recipe

The order is: **kernel metadata first**, then native implementations, then host
registrations, then DSL wrappers, then tests.

### 1. Pick the module name and add the name constant

Open `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Names.hs`
and add the primitive's `Name` constant in the appropriate library section
(alphabetical):

```haskell
charsIsAlphaNum :: Name
charsIsAlphaNum = qname chars "isAlphaNum"
```

This module is a derived Haskell-side index — it has no `module_ :: Module`
declaration and no `definitions` list to update. Each constant has type
`Name` (not `TypedTermDefinition Name`), matching the type expected by
`prim1` / `prim2` / `prim3` and `primitive1` / `primitive2` / `primitive3`.

Then add a legacy-style alias in
`packages/hydra-kernel/src/main/haskell/Hydra/Sources/Libraries.hs` so
existing call sites that use the underscored convention keep working:

```haskell
-- Chars
_chars_isAlphaNum = LibNames.charsIsAlphaNum
```

Both names refer to the same `Name` value; the alias just adapts to the
legacy `_<namespace>_<localName>` naming style used by `prim1`/`prim2`/`prim3`
registrations and host-side primitive lists.

### 2. Declare the primitive's metadata

Open the corresponding `Hydra/Sources/Kernel/Lib/<Sub>.hs` file (e.g.
`Hydra/Sources/Kernel/Lib/Chars.hs`) and add a new entry to the `definitions` list,
plus its signature, and (optionally) its default implementation.

For a simple monomorphic primitive without a default:

```haskell
    definitions = [
      ...,
      primNoDef "isAlphaNum" "Check whether a character is alphanumeric." intToBoolSig (Just
        "True if the argument is a Unicode letter or digit, false otherwise. ..."),
      ...]

intToBoolSig :: TermSignature
intToBoolSig = sig $ TypeScheme [] (Types.int32 Types.~> Types.boolean) Nothing
```

For a primitive with a default implementation expressed in terms of other primitives:

```haskell
    definitions = [
      ...,
      toPrimitive "Map over both elements of a pair." bimapSig (Just
        "bimap(f, g, p) returns a new pair (f(first(p)), g(second(p))). ...") bimap_,
      ...]

bimapSig :: TermSignature
bimapSig = sig $ TypeScheme [Name "a", Name "b", Name "c", Name "d"]
  ((Types.var "a" Types.~> Types.var "c") Types.~>
   (Types.var "b" Types.~> Types.var "d") Types.~>
   Types.pair (Types.var "a") (Types.var "b") Types.~>
   Types.pair (Types.var "c") (Types.var "d"))
  Nothing
```

The `TypeScheme`'s variable list must be in **declaration order** — the order
the variables first appear in the body — **not alphabetical**. Hosts that
re-register primitives by hand (e.g. `heads/typescript/.../lib/libraries.ts`,
`heads/java/.../lib/Libraries.java`) must follow the same order: kernel
typeApps bind positionally to that list, and mis-ordering silently swaps
domain and codomain in inferred lambda types. For example, `maybes.maybe :
b → (a → b) → maybe<a> → b` lists vars as `[b, a]`, not `[a, b]`.

```haskell

bimap_ :: TypedTermDefinition ((a -> c) -> (b -> d) -> (a, b) -> (c, d))
bimap_ = define "bimap" $
  doc "Map over both elements of a pair, defined in terms of first and second." $
  "f" ~> "g" ~> "p" ~> pair (var "f" @@ Pairs.first (var "p"))
                            (var "g" @@ Pairs.second (var "p"))
```

For a constrained polymorphic primitive (e.g. requires `ordering`):

```haskell
    definitions = [
      ...,
      primNoDef "compare" "Compare two values and return a Comparison." compareSig (Just
        "compare(x, y) returns the hydra.util.Comparison value classifying the relationship between x and y. ..."),
      ...]

compareSig :: TermSignature
compareSig = sig $ Types.polyConstrained [("x", [Name "ordering"])]
  (Types.var "x" Types.~> Types.var "x" Types.~> Types.var "hydra.util.Comparison")
```

**Definitions must be in alphabetical order by primitive name.** The kernel
validator enforces this.

### 3. Implement natively in each host

#### Haskell

Add the implementation in `heads/haskell/src/main/haskell/Hydra/Haskell/Lib/<Library>.hs`:

```haskell
-- Hydra/Haskell/Lib/Chars.hs
isAlphaNum :: Int -> Bool
isAlphaNum = C.isAlphaNum . C.chr
```

Then register it in `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Libraries.hs`:

```haskell
hydraLibChars :: Library
hydraLibChars = standardLibrary _hydra_lib_chars [
  prim1 _chars_isAlphaNum Chars.isAlphaNum [] int32 boolean,
  ...]
```

The `prim1` / `prim2` / `prim3` helpers bind a primitive name to a native
implementation. They reference the canonical metadata in the kernel by name
(via the `_chars_isAlphaNum` constant) — the type information passed to them is
a sanity-check repetition, not the source of truth.

#### Default term-level implementations (`Defaults/<Sub>.hs`)

The "default implementation" recorded on every `PrimitiveDefinition` is a
pure Hydra-term reference — directly executable by any host with a term
reducer, no native call required. Most primitives can express their default
this way; see Step 2 above for an example using `toPrimitive`.

For primitives whose canonical default is *not* a pure
`Term`-of-the-declared-type but instead a small interpreter routine —
typically higher-order operations like `lists.foldl` / `eithers.bimap`
that need to construct unevaluated `(Core.termApplication ...)` terms
and recurse — the term-level reference is published as a separate
"defaults" module under
`packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Defaults/<Sub>.hs`.
These modules emit a sibling kernel module name
`hydra.lib.defaults.<sub>` whose `TypedTermDefinition`s take the standard
interpreter context: `InferenceContext -> Graph -> ... -> Either Error Term`.

If your new primitive belongs to a module name that already has a
`Defaults/<Sub>.hs`, add an interpreter-friendly companion there
alongside the existing entries:

```haskell
-- packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Defaults/Eithers.hs
bimap_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Term -> Term -> Either Error Term)
bimap_ = define "bimap" $
  doc "Interpreter-friendly bimap for Either terms." $
  "cx" ~> "g" ~>
  "leftFun" ~> "rightFun" ~> "eitherTerm" ~>
  cases _Term (var "eitherTerm")
    (Just (ExtractCore.unexpected (string "either value") (ShowCore.term @@ var "eitherTerm"))) [
    _Term_either>>: "e" ~>
      right $ Eithers.either_
        ("val" ~> Core.termEither $ left $ Core.termApplication $ Core.application (var "leftFun") (var "val"))
        ("val" ~> Core.termEither $ right $ Core.termApplication $ Core.application (var "rightFun") (var "val"))
        (var "e")]
```

Then add `toDefinition bimap_` to the `definitions` list at the top of
the same module, keeping alphabetical order. The `define` helper is
locally bound to `definitionInModuleName ns` where `ns = ModuleName
"hydra.lib.defaults.eithers"`. No registry-table update is needed:
the kernel sync picks up the new entry through the
`Defaults/<Sub>.hs` module's own `module_` declaration.

If the module name has no `Defaults/<Sub>.hs` yet, you usually don't need
to create one — the in-line `defaultImplementation` body inside
`Kernel/Lib/<Sub>.hs` (as shown in Step 2 with `toPrimitive`) is
sufficient for almost all cases. The separate `Defaults/<Sub>.hs`
module is reserved for the "needs the interpreter context to do its
job" pattern.

Host-side registrations are unchanged regardless of which kind of
default is used: each primitive is bound with `prim1` / `prim2` /
`prim3` referencing the same `_<namespace>_<localName>` constant.

#### Java

Create the per-primitive class
`heads/java/src/main/java/hydra/lib/<library>/<FunctionName>.java`:

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
to evaluate function applications. See `hydra/lib/lists/Foldr.java` for an example
that iterates in reverse and reduces on each step.

Then register the new primitive in
`heads/java/src/main/java/hydra/lib/Libraries.java` by adding it to its
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

Add the implementation in `heads/python/src/main/python/hydra/lib/<library>.py`:

```python
def is_alpha_num(value: int) -> bool:
    """Check whether a character (as int code point) is alphanumeric."""
    return chr(value).isalnum()
```

Then register it in the Python source registry at
`heads/python/src/main/python/hydra/sources/libraries.py`. Each module name has
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

### 4. Add the DSL wrapper

Add typed wrappers in
`heads/haskell/src/main/haskell/Hydra/Dsl/Meta/Lib/<Library>.hs`:

```haskell
-- Hydra/Dsl/Meta/Lib/Chars.hs
module Hydra.Dsl.Meta.Lib.Chars where

import Hydra.Typed
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries

isAlphaNum :: TypedTerm Int -> TypedTerm Bool
isAlphaNum = primitive1 _chars_isAlphaNum

toLower :: TypedTerm Int -> TypedTerm Int
toLower = primitive1 _chars_toLower
```

These wrappers are what Hydra source modules (kernel and per-host DSL)
call to construct phantom-typed term applications referencing the
primitive by name. The phantom type must match the primitive's
signature.

**Guidelines:**
- Use phantom types (`TypedTerm`) for type safety.
- Use `primitive1` for unary, `primitive2` for binary, `primitive3` for ternary.
- Reference the name constants exposed via `Hydra.Sources.Libraries`
  (`_chars_isAlphaNum`, etc.).

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
    test name x result = primCase name _chars_isAlphaNum [int32 x] result
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

- [ ] **Kernel metadata** (required)
  - [ ] Name constant in `Hydra.Sources.Kernel.Lib.Names`
  - [ ] `toPrimitive` or `primNoDef` entry in `Hydra.Sources.Kernel.Lib.<Library>`
  - [ ] Signature definition (`TypeScheme` → `TermSignature` via `typeSchemeToTermSignature`)
  - [ ] **(If applicable)** Default implementation as a `TypedTermDefinition`
- [ ] **Haskell**
  - [ ] Native implementation in `Hydra.Haskell.Lib.<Library>`
  - [ ] Registration via `primN` in `Libraries.hs`
  - [ ] DSL wrapper in `Hydra.Dsl.Meta.Lib.<Library>`
- [ ] **Java**
  - [ ] `PrimitiveFunction` class in `hydra.lib.<library>`
  - [ ] Registration in `Libraries.java`
- [ ] **Python**
  - [ ] Function in `hydra.lib.<library>`
  - [ ] Registration in `hydra.sources.libraries`
- [ ] **Common test suite** (required)
  - [ ] Test group added to `Hydra.Sources.Test.Lib.<Library>`
  - [ ] Test group registered in `allTests`
- [ ] **Tests pass** in all three languages

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
   `logic.ifElse`), use `primNoDef`.

5. **Type-variable naming.** The canonical type-variable names come from
   `Hydra.Sources.Libraries` (`_x`, `_xOrd`, `_xEq`, etc., resolving to `TypeVar`
   values). Use the same names in your `TermSignature` to keep host signatures
   in sync.

6. **Polymorphic primitives over containers.** When the primitive involves
   `Set` or `Map`, the relevant type variable usually carries an `ordering`
   constraint (e.g. `_sets_member` uses `[_xOrd]`, not `[_x]`). Use
   `Types.polyConstrained` to express the constraint.

## Higher-order primitives

Primitives that take function arguments (e.g. `lists.map`, `maybes.bind`) are
called higher-order. They have the same shape as other primitives at the
metadata level — they're declared with a function-type signature. Two
implementation notes:

- **In native impls**, the function argument arrives as a Hydra `Term`. To
  apply it, the host uses its term-reduction machinery (Haskell:
  `Reduction.reduceTerm`; Java: `Reduction.reduceTerm()`). See
  `heads/haskell/src/main/haskell/Hydra/Haskell/Lib/Lists.hs` for the Haskell pattern
  and `heads/java/src/main/java/hydra/lib/lists/Foldr.java` for the Java
  pattern.

- **In default implementations**, the function argument is just a TypedTerm —
  apply it with `@@` in Haskell DSL:

  ```haskell
  -- maybes.map f m = maybe Nothing (\x -> Just (f x)) m
  map_ :: TypedTermDefinition ((a -> b) -> Maybe a -> Maybe b)
  map_ = define "map" $
    doc "Map a function over an optional, defined in terms of maybe." $
    "f" ~> "m" ~> Maybes.maybe nothing ("x" ~> just (var "f" @@ var "x")) (var "m")
  ```

## Example: studying an existing migration

The `hydra.lib.maybes` module name is a good case study: 13 primitives, 11 of
which have default implementations in terms of `maybe`. See:

- Metadata: [Hydra/Sources/Kernel/Lib/Maybes.hs](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Maybes.hs)
- Haskell native impl: [Hydra/Haskell/Lib/Maybes.hs](https://github.com/CategoricalData/hydra/blob/main/heads/haskell/src/main/haskell/Hydra/Haskell/Lib/Maybes.hs)
- Haskell DSL wrapper: [Hydra/Dsl/Meta/Lib/Maybes.hs](https://github.com/CategoricalData/hydra/blob/main/heads/haskell/src/main/haskell/Hydra/Dsl/Meta/Lib/Maybes.hs)
- Java native impls: [hydra/lib/maybes/](https://github.com/CategoricalData/hydra/tree/main/heads/java/src/main/java/hydra/lib/maybes)

## Further reading

- [Testing documentation](https://github.com/CategoricalData/hydra/wiki/Testing) — common test suite
- [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) — Hydra's type system and data structures
- [Creating a new Hydra implementation](new-implementation.md) — adding an entirely new language target
