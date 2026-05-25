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

1. **Universal metadata** (name, description, signature, isPure / isTotal flags,
   and an optional cross-compilable reference implementation in Hydra terms),
   declared once in the kernel as a `PrimitiveDefinition`.
2. **Per-host implementation** — the actual native code that runs in each target language
   (Haskell, Java, Python, Scala, Lisp), paired with the universal metadata into a
   `Primitive` record at host-side registration time.

The kernel's `Hydra/Sources/Kernel/Lib/<sub>.hs` files **are** the primitive registry:
they enumerate every primitive in each `hydra.lib.<sub>` namespace with its full
metadata. Host registrations look up that metadata by name and pair it with their
native implementation.

Most primitives are functions (*primitive functions*), but there are also *primitive
constants* like `hydra.lib.math.pi` and `hydra.lib.sets.empty`.

### How the registry works

For each library namespace, e.g. `hydra.lib.logic`, there is a kernel source module
`packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Logic.hs` that
declares every primitive in that namespace as a `PrimitiveDefinition`:

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
      toPrimitive "Compute the logical AND of two boolean values." andSig and_,
      primNoDef "ifElse" "Compute a conditional expression." ifElseSig,
      toPrimitive "Compute the logical NOT of a boolean value." notSig not_,
      toPrimitive "Compute the logical OR of two boolean values." orSig or_]

andSig :: TermSignature
andSig = sig $ TypeScheme [] (Types.boolean Types.~> Types.boolean Types.~> Types.boolean) Nothing

and_ :: TTermDefinition (Bool -> Bool -> Bool)
and_ = define "and" $
  doc "Logical AND, defined in terms of ifElse." $
  "a" ~> "b" ~> Logic.ifElse (var "a") (var "b" :: TTerm Bool) false
```

The two helpers used here:

- **`toPrimitive description signature defaultBody`** — declares a primitive whose
  `defaultImplementation` is a pure Hydra-term expression (the `TTermDefinition`
  body). Used when the primitive can be defined in terms of other primitives.
- **`primNoDef localName description signature`** — declares a primitive with no
  default implementation, for primitives that are fundamental (e.g. `logic.ifElse`,
  `pairs.first`) or whose meaning is host-native (e.g. arithmetic, char predicates,
  regex matching).

Both helpers produce a `Definition.primitive PrimitiveDefinition` entry which is
then enumerated alongside `Definition.term` and `Definition.type` in the module's
`moduleDefinitions` list.

The generated JSON in `dist/json/hydra-kernel/src/main/json/hydra/lib/<sub>.json`
is the cross-host source of truth — every host language reads this to know what
primitives exist, their signatures, and their default implementations.

### What lives where

| Concern | Location |
|---------|----------|
| Universal primitive metadata (name, description, signature, isPure, isTotal, defaultImplementation) | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/<Sub>.hs` |
| Primitive-name constants (`_logic_and`, `_chars_isAlphaNum`, ...) for use in source code | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Names.hs` |
| Native Haskell implementation | `heads/haskell/src/main/haskell/Hydra/Lib/<Sub>.hs` |
| Native Java implementation | `heads/java/src/main/java/hydra/lib/<sub>/<FunctionName>.java` |
| Native Python implementation | `heads/python/src/main/python/hydra/lib/<sub>.py` |
| Host-side primitive registry (binds names to native impls) | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Libraries.hs` (Haskell) and analogous per host |
| Phantom-typed DSL wrappers (for writing Hydra programs) | `heads/haskell/src/main/haskell/Hydra/Dsl/Meta/Lib/<Sub>.hs` |
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
fromMaybe_ :: TTermDefinition (a -> Maybe a -> a)
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

### 1. Pick the namespace and add the name constant

Open `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Names.hs`
and add the name constant in the appropriate library section (alphabetical):

```haskell
charsIsAlphaNum = defineName "charsIsAlphaNum" "hydra.lib.chars" "isAlphaNum"
```

Also add it to the `definitions` list in the same file. These constants are used
everywhere a primitive is referenced by name.

### 2. Declare the primitive's metadata

Open the corresponding `Hydra/Sources/Kernel/Lib/<Sub>.hs` file (e.g.
`Hydra/Sources/Kernel/Lib/Chars.hs`) and add a new entry to the `definitions` list,
plus its signature, and (optionally) its default implementation.

For a simple monomorphic primitive without a default:

```haskell
    definitions = [
      ...,
      primNoDef "isAlphaNum" "Check whether a character is alphanumeric." intToBoolSig,
      ...]

intToBoolSig :: TermSignature
intToBoolSig = sig $ TypeScheme [] (Types.int32 Types.~> Types.boolean) Nothing
```

For a primitive with a default implementation expressed in terms of other primitives:

```haskell
    definitions = [
      ...,
      toPrimitive "Map over both elements of a pair." bimapSig bimap_,
      ...]

bimapSig :: TermSignature
bimapSig = sig $ TypeScheme [Name "a", Name "b", Name "c", Name "d"]
  ((Types.var "a" Types.~> Types.var "c") Types.~>
   (Types.var "b" Types.~> Types.var "d") Types.~>
   Types.pair (Types.var "a") (Types.var "b") Types.~>
   Types.pair (Types.var "c") (Types.var "d"))
  Nothing

bimap_ :: TTermDefinition ((a -> c) -> (b -> d) -> (a, b) -> (c, d))
bimap_ = define "bimap" $
  doc "Map over both elements of a pair, defined in terms of first and second." $
  "f" ~> "g" ~> "p" ~> pair (var "f" @@ Pairs.first (var "p"))
                            (var "g" @@ Pairs.second (var "p"))
```

For a constrained polymorphic primitive (e.g. requires `ordering`):

```haskell
    definitions = [
      ...,
      primNoDef "compare" "Compare two values and return a Comparison." compareSig,
      ...]

compareSig :: TermSignature
compareSig = sig $ Types.polyConstrained [("x", [Name "ordering"])]
  (Types.var "x" Types.~> Types.var "x" Types.~> Types.var "hydra.util.Comparison")
```

**Definitions must be in alphabetical order by primitive name.** The kernel
validator enforces this.

### 3. Implement natively in each host

#### Haskell

Add the implementation in `heads/haskell/src/main/haskell/Hydra/Lib/<Library>.hs`:

```haskell
-- Hydra/Lib/Chars.hs
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

#### Java

Create `heads/java/src/main/java/hydra/lib/<library>/<FunctionName>.java`:

```java
public class IsAlphaNum extends PrimitiveFunction {
    public Name name() { return new Name("hydra.lib.chars.isAlphaNum"); }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), boolean_()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(
            c -> Terms.boolean_(apply(c)),
            hydra.extract.Core.int32(cx, graph, args.get(0)));
    }

    public static boolean apply(int codePoint) {
        return Character.isLetterOrDigit(codePoint);
    }
}
```

Register it in `heads/java/src/main/java/hydra/lib/Libraries.java`:

```java
private static List<PrimitiveFunction> charsPrimitives() {
    return Arrays.asList(new IsAlphaNum(), new ToLower(), ...);
}
```

`PrimitiveFunction.toNative()` builds a `PrimitiveDefinition` from the host-side
`name()` and `type()` (via `Scoping.typeSchemeToTermSignature`) and pairs it
with the host implementation.

#### Python

Add the implementation in `heads/python/src/main/python/hydra/lib/<library>.py`:

```python
def is_alpha_num(value: int) -> bool:
    """Check if a character (as int) is alphanumeric."""
    return chr(value).isalnum()
```

Register it in the Python source registry (`heads/python/src/main/python/hydra/sources/libraries.py`).

### 4. Add the DSL wrapper

Add typed wrappers in `heads/haskell/src/main/haskell/Hydra/Dsl/Meta/Lib/<Library>.hs`:

```haskell
isAlphaNum :: TTerm Int -> TTerm Bool
isAlphaNum = primitive1 _chars_isAlphaNum
```

These wrappers are what Hydra programs call. They construct phantom-typed term
applications referencing the primitive by name. The phantom type matches the
primitive's signature.

### 5. Add common test cases

Tests live in `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Test/Lib/<Library>.hs`:

```haskell
charsIsAlphaNum :: TTerm TestGroup
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
  - [ ] **(If applicable)** Default implementation as a `TTermDefinition`
- [ ] **Haskell**
  - [ ] Native implementation in `Hydra.Lib.<Library>`
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
  `heads/haskell/src/main/haskell/Hydra/Lib/Lists.hs` for the Haskell pattern
  and `heads/java/src/main/java/hydra/lib/lists/Foldr.java` for the Java
  pattern.

- **In default implementations**, the function argument is just a TTerm —
  apply it with `@@` in Haskell DSL:

  ```haskell
  -- maybes.map f m = maybe Nothing (\x -> Just (f x)) m
  map_ :: TTermDefinition ((a -> b) -> Maybe a -> Maybe b)
  map_ = define "map" $
    doc "Map a function over an optional, defined in terms of maybe." $
    "f" ~> "m" ~> Maybes.maybe nothing ("x" ~> just (var "f" @@ var "x")) (var "m")
  ```

## Example: studying an existing migration

The `hydra.lib.maybes` namespace is a good case study: 13 primitives, 11 of
which have default implementations in terms of `maybe`. See:

- Metadata: [Hydra/Sources/Kernel/Lib/Maybes.hs](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Maybes.hs)
- Haskell native impl: [Hydra/Lib/Maybes.hs](https://github.com/CategoricalData/hydra/blob/main/heads/haskell/src/main/haskell/Hydra/Lib/Maybes.hs)
- Haskell DSL wrapper: [Hydra/Dsl/Meta/Lib/Maybes.hs](https://github.com/CategoricalData/hydra/blob/main/heads/haskell/src/main/haskell/Hydra/Dsl/Meta/Lib/Maybes.hs)
- Java native impls: [hydra/lib/maybes/](https://github.com/CategoricalData/hydra/tree/main/heads/java/src/main/java/hydra/lib/maybes)

## Further reading

- [Testing documentation](https://github.com/CategoricalData/hydra/wiki/Testing) — common test suite
- [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) — Hydra's type system and data structures
- [Creating a new Hydra implementation](new-implementation.md) — adding an entirely new language target
