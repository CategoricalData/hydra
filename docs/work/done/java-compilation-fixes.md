# Java Compilation and Test Fixes

**Updated:** 2026-02-05 (archived to done/)

## Overview
This document tracks the investigation and fixes for Java compilation errors and kernel test failures in the generated Hydra Java code. Issue #166 was resolved on February 5, 2026, with all kernel and generation tests passing.

## Final Status
**Compilation errors: 0** (down from 100+ at the start — all resolved)
**Test results: All kernel and generation tests pass. Issue #166 closed.**

### Build Pipeline
```
stack build          # in hydra-ext
stack exec update-java-kernel
./gradlew :hydra-java:compileJava
```

### Acceptance Criteria
- Zero Java compile errors
- Haskell tests pass (4159 expected) ✓
- Python tests pass (1899 expected)

## Compilation Errors (all resolved)

All compilation errors have been resolved. At peak there were 100+ errors; the last 4 were fixed by `detectAccumulatorUnification` in the Java coder and by disabling `Comparable` bound generation. The errors and their fixes are documented below for reference.

### Error 1: Rewriting.java — forFieldsWithAccessor accumulator pair mismatch (FIXED)
**Category:** Over-generalized TypeScheme after hoisting

Fixed by `detectAccumulatorUnification` selfRefSubst/directRefSubst in Coder.hs (see Fix 21-23).

### Error 2: Tarjan.java — SortOn needs Comparable bound (FIXED in session 15)
**Category:** Typeclass constraint propagation

Fixed by correcting constraint naming (`_TypeClass_ordering`/`_TypeClass_equality`), implementing `writeTypeBound` in Serde.hs, removing `Comparable` bounds from Java lib primitives, and disabling Comparable bound generation in `toParam` (see Fix 24).

### Error 3: Schemas.java — fullyStripAndNormalizeType_go dangling T0 (FIXED)
**Category:** Dangling codomain type variable with concrete match

Fixed by `detectAccumulatorUnification` danglingSubst/codSubst in Coder.hs (see Fix 21-23).

### Analysis: Kernel vs. Coder

These errors were all symptoms of the same kernel-level issue: **the inference/hoisting pipeline produces TypeSchemes that are over-generalized**. The Java coder's `detectAccumulatorUnification` works around this by detecting and correcting over-generalized type variables at the coder level. The ideal long-term fix is in the kernel's inference or hoisting modules to produce more accurate TypeSchemes.

## Fixed Errors (sessions 13-14)

### Rewriting.java:1557 — Tuple2<T0,Elimination> vs Tuple2<T1,Elimination> (FIXED)
Fixed by `detectAccumulatorUnification` selfRefSubst.

### Rewriting.java:1928 — T0 vs T2 in monadic fold version (FIXED)
Same fix as above.

### Hoisting.java:932/935 — T4 cannot be converted to T2 (FIXED)
Fixed by `directRefSubst` in `detectAccumulatorUnification`. Key insight: in Hydra type schemes, named types like `Lambda` appear as `TypeVariable` with qualified names (e.g., `hydra.core.Lambda`), so detection must distinguish scheme type parameters (e.g., `t0`) from named type references using `tparamSet`.

### Call-site type arg mismatches after callee type param reduction (FIXED)
Fixed by updating `filterPhantomTypeArgs` and `correctTypeApps`.

## Implementation: `detectAccumulatorUnification`

Located in `Coder.hs`, this function detects over-generalized type variables and returns a `Map Name Type` substitution. It handles four patterns:

1. **selfRefSubst** (pair-based): For function params `V → ... → (V, ...)` and `V → ... → (W, ...)` with same input V, substitute W → V.

2. **directRefSubst** (context-threading): For function params `V → Concrete → V` (2+ occurrences) and `V → Concrete → W`, substitute W → V. Guards:
   - At least 2 self-refs for the same input var
   - W is not the method's codomain variable
   - W is not an input var in other patterns
   - Middle arg must not be a scheme type parameter (to distinguish concrete named types)

3. **danglingSubst**: Codomain pair-first variable not in any domain, with self-ref vars from function params.

4. **codSubst**: Codomain pair-first variable that doesn't match any self-ref var.

Applied at:
- `encodeTermDefinition`: fixes method signatures (fixedCod, fixedDoms, fixedTparams)
- `correctTypeApps`: fixes call-site type args for `encodeApplication`
- `filterPhantomTypeArgs`: fixes call-site type args for static method invocations

## All Fixes Applied (1-20 from prior sessions, 21+ sessions 13-15)

### Fix 1-20: See git history
(Covers: writeArrayType, Object_ fix, byte[] fixes, nullary constants, hoisting fixes, type lambda capture, typeOfVariable, adaptDataGraph, typesAllEffectivelyEqual, emptyGraph, RDF Serde exclusion, unshadowVariables, lambda shadowing, PartialVisitor, Find/Filter overloads, getState type witness, visitor type variables, let-bound tracking, projection elimination)

### Fix 21: Accumulator fold unification
`detectAccumulatorUnification` selfRefSubst — detects pair-return fold patterns where different type variables should be unified. Fixed Rewriting.java errors 1557 and 1928.

### Fix 22: Call-site type arg filtering for overgen
Updated `filterPhantomTypeArgs` and `correctTypeApps` to also detect and filter over-generalized type variables at call sites.

### Fix 23: Context-threading direct return unification
`detectAccumulatorUnification` directRefSubst — detects context-extension function patterns (`V → ConcreteType → V/W`) and unifies non-self-referencing return variables. Fixed Hoisting.java errors 932 and 935.

### Fix 24: Typeclass constraint naming and serialization (session 15)
- Fixed constraint names: `"hydra.typeclass.Ord"` → `_TypeClass_ordering` (`"ordering"`), `"hydra.typeclass.Eq"` → `_TypeClass_equality` (`"equality"`)
- Implemented `writeTypeBound` and `writeAdditionalBound` in Serde.hs
- Removed `extends Comparable` bounds from Java lib primitives (SortOn, Sort, comparison ops)
- Fixed Tarjan.java:26 error

## Files Modified (sessions 13-15)

- `hydra-ext/src/main/haskell/Hydra/Ext/Staging/Java/Coder.hs` — Major changes:
  - Added `substituteTypeVarsWithTypes` helper (Map Name Type → Type → Type)
  - Added `detectAccumulatorUnification` function (returns Map Name Type)
  - Modified `encodeTermDefinition` to apply overgen substitution
  - Modified `filterPhantomTypeArgs` to detect/filter overgen vars
  - Modified `correctTypeApps` to detect/filter overgen vars
  - Session 15: `toParam` bound set to `Nothing` (Comparable generation disabled)
- `hydra-ext/src/main/haskell/Hydra/Ext/Staging/Java/Utils.hs` — Added `aliasesMethodCodomain` field to Aliases
- `hydra-ext/src/main/haskell/Hydra/Ext/Staging/Java/Serde.hs` — Session 15: Implemented `writeTypeBound` and `writeAdditionalBound`
- `hydra-haskell/src/main/haskell/Hydra/Dsl/Prims.hs` — Session 15: Use `_TypeClass_ordering`/`_TypeClass_equality` constants
- `hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Terms/Decoding.hs` — Session 15: Use `Core.nameLift _TypeClass_ordering`
- `hydra-haskell/src/main/haskell/Hydra/Sources/Haskell/Coder.hs` — Session 15: Use `_TypeClass_*` constants for string comparison
- `hydra-haskell/src/gen-main/haskell/Hydra/Decoding.hs` — Session 15: `"ordering"` instead of `"hydra.typeclass.Ord"`
- `hydra-haskell/src/gen-main/haskell/Hydra/Ext/Haskell/Coder.hs` — Session 15: `"ordering"`/`"equality"` instead of `"hydra.typeclass.Ord"`/`"hydra.typeclass.Eq"`
- `hydra-haskell/src/gen-main/haskell/Hydra/Sources/Decode/Relational.hs` — Session 15: `"ordering"` instead of `"hydra.typeclass.Ord"`
- `hydra-java/src/main/java/hydra/lib/lists/SortOn.java` — Session 15: Removed `Comparable` bound, use unchecked cast
- `hydra-java/src/main/java/hydra/lib/lists/Sort.java` — Session 15: Removed `Comparable` bound, use unchecked cast
- `hydra-java/src/main/java/hydra/lib/equality/{Lt,Gt,Lte,Gte,Min,Max,Compare}.java` — Session 15: Removed `Comparable` bound, use unchecked cast

## Key Technical Insights

### TypeScheme constraint representation
- Constraints are stored using `_TypeClass_ordering` = `Name "ordering"` and `_TypeClass_equality` = `Name "equality"` from `Hydra.Classes`
- The Haskell coder converts these to `TypeClass` values via `typeSchemeConstraintsToClassMap`
- The Java coder's `toParam` checks for `Name "ordering"` to generate `Comparable` bounds (currently disabled)
- `writeTypeBound` in Serde.hs is now implemented (was previously a STUB)
- Hydra's `Ord` constraint comes from Haskell's `Data.Set`/`Data.Map` requiring `Ord`. In Java, `HashSet`/`HashMap` don't need `Comparable`, so most `Ord` constraints are irrelevant for Java code generation

### Named types in Hydra type schemes
Named types like `hydra.core.Lambda`, `hydra.core.Type` are represented as `TypeVariable (Name "hydra.core.Lambda")` in TypeSchemes. They are NOT concrete record types. To distinguish them from actual type parameters, check membership in the `tparams` set.

### Hoisting constraint propagation
The hoisting pipeline (`hoistPolymorphicLetBindings`) starts with empty `classConstraints`. Parent binding constraints are available via `typeSchemeConstraints` but are not injected into the TypeContext. Attempted fix: inject parent constraints into `InferenceContext.classConstraints` before traversal. This correctly propagated constraints to hoisted bindings but caused 47 errors due to missing call-site propagation.

## Test Infrastructure (sessions 16-17)

### Setup
- Created `hydra-ext/src/exec/update-java-kernel-tests/Main.hs` — generates test data to `hydra-java/src/gen-test/java`
- Created `hydra-ext/src/exec/update-java-eval-lib/Main.hs` — generates eval lib to `hydra-java/src/gen-main/java`
- Created `hydra-ext/bin/sync-java.sh` — master sync script (build, generate, test)
- Implemented `hydra-java/src/test/java/hydra/TestSuiteRunner.java` — runs all kernel test cases

### Test Results History

| Session | Tests | Failures | Passing | Notes |
|---------|-------|----------|---------|-------|
| 16 (initial) | 1961 | 764 | 1197 (61%) | First test run |
| 16 (unit-type fix) | 2703 | 742 | 1961 (73%) | Fixed phantom Boolean equality (128 classes) |
| 17 | 2089 | 747 | 1342 (64%) | TermLet Supplier encoding + Name Comparable + unit-type fix |
| 18 | 2089 | 319 | 1770 (84%) | Lambda body thunking (Supplier pattern for local vars) + fixed unit-type script |
| 19 (batch 1) | 2089 | 280 | 1809 (86%) | Primitive impls (reduceTerm), comparison fixes, Sort/Keys/ToList, ShowUint |
| 19 (batch 2) | 2089 | 266 | 1823 (87%) | StringToBinary, Sort compareTerms, numeric literal comparison |
| 19 (batch 3) | 2089 | 236 | 1841 (88.1%) | Round banker's rounding, Binary/BigDecimal equals, ifElse arg order, Maybes bind/compose/mapMaybe, Transpose ragged, sets Member/Delete |
| 19 (batch 4) | 2089 | 234 | 1843 (88.2%) | Unicode surrogate pair fix in Java string codegen, float precision tolerance, improved JSON test diagnostics |
| 19 (batch 5) | 2089 | 206 | 1871 (89.6%) | Kernel term source modules (hydra.sources.monads, hydra.sources.annotations) — 23 tests fixed |
| 19 (batch 6) | 2089 | 165 | 1912 (91.5%) | Lazy JSON parser (Parsers.lazy combinator) — 41 tests fixed |
| 19 (final) | ~1984 | 75 | ~1908 (96.2%) | Flows.mapM list mutation bug — 90 tests fixed |
| 20 | 1987 | 68 | 1919 (96.6%) | Comparable codegen, type-aware compareTo, unit-type comprehensive fix |
| 21 (current) | 2089 | 70 | 2019 (96.6%) | Ordered sets (TreeSet), ordered maps (TreeMap), alpha-equivalence normalization |

### Failure Categories (session 18, after thunking)

| Count | Error | Root Cause |
|-------|-------|------------|
| 147 | Wrong result | Various: annotations, shadowing, polymorphic evaluation, type mismatch |
| 122 | Execution timeout | Eager evaluation still present in some evaluation paths |
| 40 | StackOverflowError | JSON parser `many`/`some` mutual recursion |
| 5 | ClassCastException | Sort-related (likely missing Comparable on some types) |
| 5 | Other | IllegalArgumentException from reduceTerm or mapM |

**Previous session:** 77 ClassCastException (Name Comparable) + 31 unification errors (unit-type equality)
**This session:** 428 failures fixed by lambda body thunking + 6 fixed by corrected unit-type script

### Fixes Applied (sessions 16-17)

#### Fix 25: String Unicode handling
Java's `String.length()`, `charAt()`, `toCharArray()` operate on UTF-16 code units; Haskell operates on Unicode codepoints.
- `strings/Length.java`: `s.length()` → `s.codePointCount(0, s.length())`
- `strings/CharAt.java`: `s.charAt(index)` → `s.codePointAt(s.offsetByCodePoints(0, index))`
- `strings/ToList.java`: char array iteration → `s.codePoints().forEach(list::add)`
- `strings/FromList.java`: `sb.append((char) i)` → `sb.appendCodePoint(i)`
- `strings/SplitOn.java`: byte-level → `String.indexOf`-based

#### Fix 26: Float formatting (HaskellShowFloat)
Created `hydra/lib/literals/HaskellShowFloat.java` matching Haskell's `show` for floats:
- Scientific notation for |x| < 0.1, lowercase 'e'
- `Float.toString()`/`Double.toString()` then format conversion
- Updated `ShowFloat32.java`, `ShowFloat64.java`, `ShowBigfloat.java`

#### Fix 27: BigfloatToBigint rounding
Changed `value.toBigInteger()` to `value.setScale(0, RoundingMode.HALF_EVEN).toBigInteger()` to match Haskell's `round`.

#### Fix 28: JSON Number equality
Changed `Value.Number_.equals()` from `value.equals(o.value)` to `value.compareTo(o.value) == 0` for scale-insensitive BigDecimal comparison.

#### Fix 29: Sort implementation
- `lists/Sort.java`: Fixed `implementation()` to actually sort terms (was returning unsorted)
- `maps/Keys.java`: Added sorting for Comparable keys (Haskell's `Data.Map.keys` is sorted)
- `maps/ToList.java`: Added sorting by key for Comparable keys

#### Fix 30: Unit-like type equality (128 classes)
Generated code uses phantom `Boolean value` fields in unit variant types (e.g., `IntegerType.Int32`). Fixed `equals()` to check `instanceof` only and `hashCode()` to return `getClass().hashCode()`. Requires post-generation script since `update-java-kernel` overwrites.

**Classes preserved (NOT phantom):** `Literal.Boolean_`, `json.model.Value.Boolean_` — these have real boolean values.

#### Fix 31: Name implements Comparable
Added `Comparable<Name>` to `hydra.core.Name` with `compareTo` delegating to `value.compareTo`. Fixes 77 `ClassCastException` failures from `SortOn`. Requires post-generation fix since Name.java is generated.

#### Fix 32: TermLet lazy encoding (Supplier pattern)
Changed the Java code generator's `TermLet` handler from eager lambda application:
```java
((Function<T,R>) (x -> body)).apply(expr)  // eagerly evaluates expr
```
to lazy Supplier block:
```java
((Supplier<R>) (() -> { T x = expr; return body; })).get()  // lazily evaluates expr
```

**Location:** `hydra-ext/src/main/haskell/Hydra/Ext/Staging/Java/Coder.hs` lines 1551-1574

**Impact:** Addresses some "head of empty list" errors where let-bindings appear outside lambda bodies.

#### Fix 33: Lambda body thunking (session 18)
Extended the Supplier pattern to let-bindings hoisted into lambda bodies by `analyzeJavaFunction`.

**Mechanism:** In `bindingsToStatements`, zero-arity bindings with structurally complex terms (containing `TermLet`, `TermTypeApplication`, or `TermTypeLambda`) are wrapped in `Supplier<T>`:
```java
// Before (eager):
T x = complexExpr;

// After (lazy):
java.util.function.Supplier<T> x = () -> complexExpr;
```

References to thunked variables use `.get()`:
```java
// Before:
doSomething(x)

// After:
doSomething(x.get())
```

**Implementation details:**
1. Added `aliasesThunkedVars :: S.Set Name` field to `Aliases` record in `Utils.hs`
2. `bindingsToStatements` computes `thunkedVars` for bindings that are non-recursive, have complex terms, and zero arity
3. `toDeclStatement` wraps thunked binding RHS in `Supplier<T>` lambda
4. `encodeVariable` emits `.get()` for references to thunked variables
5. `needsThunking` check is structural only (does NOT follow variable references to avoid over-thunking)

**Impact:** Reduced failures from 747 to 319 (428 fewer failures, 57% reduction).

#### Fix 33b: Unit-type equality script fix (session 18)
The original unit-type fix script used file-level regex replacement, which incorrectly modified equals/hashCode for non-unit inner classes in files that also contained unit-like classes (e.g., `Term.Literal`'s equals was broken in `Term.java` because `Term.Unit` is unit-like).

**Fix:** Rewrote script to parse class boundaries and only modify equals/hashCode within classes that have `Boolean value` fields. Now correctly handles files with mixed inner class types (19 such files identified).

### Fix 34: Primitive function implementations — missing reduceTerm (session 19)

Many primitive function `implementation()` methods were building `Term.Application` terms without reducing them. This means when the test evaluator called `reduceTerm`, these primitives returned unreduced application trees instead of actual computed results.

**Root cause:** The pattern `Terms.apply(f, x)` just constructs a `Term.Application` node in the AST — it does NOT evaluate the function. The `implementation()` methods need to call `hydra.Reduction.reduceTerm(true, ...)` on these application terms to get actual results.

**Files fixed:**

| File | Issue |
|------|-------|
| `lists/Map.java` | `map f list` returned `[f(x1), f(x2), ...]` as unevaluated applications |
| `lists/Foldl.java` | `foldl f init list` built nested `f(f(init,x1),x2)` without reducing |
| `lists/ZipWith.java` | `zipWith f l1 l2` built `[f(a,b), ...]` without reducing |
| `lists/Apply.java` | `<*>` built applications without reducing |
| `lists/Bind.java` | `>>=` built applications then passed to concat without reducing |
| `lists/Span.java` | `implementation()` was a stub returning `([], list)` |
| `lists/Transpose.java` | `implementation()` was a stub returning input unchanged |
| `lists/SortOn.java` | `implementation()` was a stub returning input unchanged |
| `maps/Map.java` | `map f map` returned map with unevaluated value applications |
| `maps/MapKeys.java` | `mapKeys f map` returned map with unevaluated key applications |
| `maps/Bimap.java` | `bimap kf vf map` built applications without reducing |
| `equality/Compare.java` | Made `compareTerms` public static (needed by SortOn) |

**Impact:** Pending test results.

### Fix 35: Comparison primitives using Object.toString() (session 19)

`Lt`, `Gt`, `Lte`, `Gte` `implementation()` methods were using `args.get(0).toString().compareTo(args.get(1).toString())` for term comparison. Since `Term` has no custom `toString()`, this compared Java default `Object.toString()` strings containing hash codes — giving effectively random results.

**Fix:** Changed to use `Compare.compareTerms()` which uses `hydra.show.Core.term()` for deterministic string comparison. Also fixed `Compare.compareTerms()` itself for the same issue.

This was the root cause of all predicate-based list test failures (dropWhile, filter, find, partition, span — 15 tests).

### Fix 36: ShowUint8/ShowUint32 using wrong Expect (session 19)

- `ShowUint8.implementation()` used `Expect.int16()` → fixed to `Expect.uint8()`
- `ShowUint32.implementation()` used `Expect.int64()` → fixed to `Expect.uint32()`

**Impact:** 4 test failures (showUint8 zero/max, showUint32 cases)

### Fix 37: Proper structural comparison in Compare.compareTerms (session 19)

`Compare.compareTerms()` was using `hydra.show.Core.term()` string comparison, which fails for numeric values (e.g., "1:int32" < "10:int32" because ':' > '0'). Implemented proper structural comparison:
- Integer literals: convert to BigInteger and compare numerically
- Float literals: convert to BigDecimal and compare
- String literals: use String.compareTo
- Boolean literals: use Boolean.compare
- Fallback: show representation comparison

Also fixed `Sort.java`, `maps/Keys.java`, `maps/ToList.java`, `maps/Elems.java`, `sets/ToList.java` — all were casting `Term` to `Comparable` which fails since `Term` doesn't implement `Comparable`.

### Fix 38: Round uses banker's rounding (session 19)

`Round.apply(Double)` used `Math.round(x)` which is "round half up". Haskell's `round` uses "round half to even" (banker's rounding). Changed to `BigDecimal.valueOf(x).setScale(0, RoundingMode.HALF_EVEN).toBigIntegerExact()`.

### Fix 39: Binary literal equals (session 19)

`Literal.Binary.equals()` used `value.equals(o.value)` on `byte[]` — this is reference equality. Changed to `java.util.Arrays.equals(value, o.value)`.

### Fix 40: StringToBinary/BinaryToString base64 encoding (session 19)

Tests expect base64 encoding/decoding between string and binary representations:
- `StringToBinary.apply()`: `Base64.getDecoder().decode(str)` — base64 string → bytes
- `BinaryToString.apply()`: `Base64.getEncoder().encodeToString(binary)` — bytes → base64 string
- `BinaryToString.implementation()`: Extract raw bytes from `Literal.Binary` instead of using `Expect.binary()` (which does UTF-8 decode)

### Fix 41: Maybes.Bind double-wrapping (session 19)

`Bind.implementation()` wrapped result in `Terms.optional(Maybe.just(...))`, but bind should return `f(x)` directly (which already returns Optional). Also added `reduceTerm` call.

### Fix 42: Maybes.Compose wrong implementation (session 19)

`Compose.implementation()` treated `args.get(0)` as an Optional value instead of a function. Fixed to properly apply `f(x)` → reduce → extract Maybe → if Just, apply `g(b)`.

### Fix 43: Maybes.MapMaybe not filtering (session 19)

`MapMaybe.implementation()` collected raw unevaluated applications without reducing or filtering. Fixed to reduce each application, extract the Maybe, and keep only Just values.

### Fix 44: Transpose ragged matrix (session 19)

`Transpose.apply()` used `matrix.get(0).size()` as column count — only considers first row length. For ragged matrices, need max column count across all rows.

### Fix 45: Sets.Member wrong name (session 19)

`Member.name()` returned `"hydra.lib.sets.contains"` but test data references `"hydra.lib.sets.member"`.

### Fix 46: Sets.Delete wrong type arity (session 19)

`Delete.type()` had `function("x", set("x"))` (arity 1) but implementation accesses `args.get(1)` → IndexOutOfBounds. Fixed to `function(var("x"), set("x"), set("x"))`.

### Fix 47: IfElse argument order (session 19)

Type was `function(var("a"), var("a"), boolean_(), var("a"))` — boolean last. Haskell signature is `Bool → a → a → a` — boolean first. Fixed to `function(boolean_(), var("a"), var("a"), var("a"))`.

### Fix 48: ShowUint64 wrong Expect (session 19)

`ShowUint64.implementation()` used `Expect.bigint()` → fixed to `Expect.uint64()`.

### Fix 49: Float comparison tolerance in test runner (session 19)

Added `termsEqual()` helper that handles:
- Float64: ULP tolerance for precision differences (Java vs Haskell math implementations)
- Float32: same ULP tolerance
- Bigfloat: `BigDecimal.compareTo()` instead of `equals()` (scale-insensitive)

### Fix 50: Unicode supplementary plane characters in Java string codegen (session 19)

Java's `\uXXXX` escape sequence only supports 4 hex digits (BMP, U+0000-U+FFFF). The Haskell Java coder's `writeStringLiteral` function in `Serde.hs` used `padHex` which produces 5-hex-digit sequences for supplementary plane characters (U+10000+). For example, U+1F30D (🌍) was output as `\u1F30D` which Java interprets as `\u1F30` (ἰ) + `D`.

**Fix:** Added `javaUnicodeEscape` function that detects code points > 0xFFFF and emits UTF-16 surrogate pairs:
```haskell
javaUnicodeEscape n
  | n > 0xFFFF =
      let n' = n - 0x10000
          hi = 0xD800 + (n' `div` 0x400)
          lo = 0xDC00 + (n' `mod` 0x400)
      in "\\u" ++ padHex hi ++ "\\u" ++ padHex lo
  | otherwise = "\\u" ++ padHex n
```

Same fix applied to `writeCharacterLiteral`.

**Location:** `hydra-ext/src/main/haskell/Hydra/Ext/Staging/Java/Serde.hs` lines 835-860

### Fix 51: Kernel term source modules (session 19, continued)

Created `update-java-kernel-sources` executable in hydra-ext that generates Term AST source modules — mirroring the Python `update-python-kernel-sources` pattern. These modules contain term-encoded `hydra.module.Module` objects that provide kernel term bindings as data.

**Files created:**
- `hydra-ext/src/exec/update-java-kernel-sources/Main.hs`
- `hydra-ext/package.yaml` (added executable entry)

**Files generated:**
- `hydra-java/src/gen-main/java/hydra/sources/monads/Monads.java` — Term AST for hydra.monads module (79KB)
- `hydra-java/src/gen-main/java/hydra/sources/annotations/Annotations.java` — Term AST for hydra.annotations module (103KB)

**Note:** Only monads and annotations source modules were kept; the other 110 were too large for Java's 64KB method bytecode limit (e.g., `rewriting` at 881KB). A splitting strategy would be needed for larger modules.

**TestSuiteRunner change:** `buildTestGraph()` now loads kernel term bindings from these source modules and includes them in the term graph.

**Impact:** 23 tests fixed (229 → 206)

### Fix 52: Lazy JSON parser combinator (session 19, continued)

JSON parser had mutual recursion: `jsonValue()` → `jsonArray()`/`jsonObject()` → `jsonValue()`. In Haskell, lazy evaluation breaks the cycle. In Java, all parsers are eagerly constructed, causing StackOverflow.

**Fix:** Added `Parsers.lazy(Supplier<Parser<T>>)` method to defer parser construction:
```java
static <T0> Parser<T0> lazy(Supplier<Parser<T0>> supplier) {
    return new Parser<>(input -> supplier.get().value.apply(input));
}
```

Modified `jsonValue()` to use lazy wrappers for recursive parsers:
```java
Parsers.lazy(() -> Parser.jsonArray()),
Parsers.lazy(() -> Parser.jsonObject())
```

**Impact:** 41 tests fixed (206 → 165). All JSON parsing tests now pass.

### Fix 53: Flows.mapM/sequence list mutation bug (session 19, continued)

`Flows.mapM()` and `Flows.sequence()` used in-place list mutation (`ys.add(b); return ys;`) inside the bind/map callbacks. When the Flow monad's state threading caused these callbacks to be re-invoked (e.g., during trace propagation), the shared mutable list accumulated duplicate elements.

**Example:** Encoding `[1,2,3]` through the JSON coder produced `[1,2,3,1,2,3]`.

**Fix:** Changed all three functions to create new lists/sets instead of mutating:
```java
// Before:
ys.add(b);
return ys;

// After:
List<B> newList = new ArrayList<>(ys);
newList.add(b);
return newList;
```

**Files modified:** `hydra-java/src/main/java/hydra/dsl/Flows.java` — `mapM(List)`, `mapM(Set)`, `sequence(List)`

**Impact:** 90 tests fixed (165 → 75). The bug was much more widespread than the 3 JSON coder tests — it affected all flow-based list operations including inference, checking, and many evaluation tests.

### Fix 54: Map ordering for Term-keyed maps (session 20)

`ToList.apply()`, `Keys.apply()`, and `Elems.apply()` only sorted when keys implemented `Comparable`. `Term` doesn't implement `Comparable`, so maps with Term keys were iterated in HashMap's arbitrary order. Added fallback sorting using `Compare.compareTerms()` for Term keys.

**Files modified:** `hydra-java/src/main/java/hydra/lib/maps/ToList.java`, `Keys.java`, `Elems.java`

**Impact:** 2 tests fixed ("string to int map", "map in let binding")

### Fix 55: Annotation primitives and source module bridge (session 20)

Annotation tests require `setTermAnnotation`/`setTermDescription` which are defined in the annotations source module AST. The source module references external functions (primitives, constants) as Term.Variable, but the reducer only looks up variables in term bindings, not primitives.

Solution: Bridge all primitives into term bindings as `Function.Primitive(...)` references, and add constant term bindings for `hydra.constants.*`. Also registered `deannotateTerm` and `deannotateType` as primitive functions.

**Files created:** `hydra-java/src/main/java/hydra/lib/rewriting/DeannotateTerm.java`, `DeannotateType.java`, `SetTermAnnotation.java`, `SetTermDescription.java`
**Files modified:** `hydra-java/src/main/java/hydra/lib/Libraries.java`, `hydra-java/src/test/java/hydra/TestSuiteRunner.java`

**Impact:** 6 tests fixed ("set single annotation #1-3", "set description #1-3"). 15 remaining annotation failures need further debugging.

### Fix 56: Type variable normalization in TypeChecking tests (session 20)

Java's inference engine assigns fresh type variable names in a different order than Haskell's. Applied `normalizeTypeVariablesInTerm` to both expected and actual terms before comparison in TypeChecking tests.

**Files modified:** `hydra-java/src/test/java/hydra/TestSuiteRunner.java`

**Impact:** Potentially 5 inferred-term failures ("lists foldl", "optionals maybe", "nested projection", "chained projections", "unwrap with maybe")

### Remaining Test Failures — Root Cause Analysis (session 21, updated)

**70 failures remaining** across these categories:

#### 1. Execution timeouts (47 failures, 67%)
Tests that exceed 5-second timeout:
- **hoistCaseStatements** (~20): All case statement hoisting tests timeout
- **hoistPolymorphicLetBindings** (~8): All poly let binding hoisting tests timeout
- **hoistPolymorphicTypeParameters** (~7): All poly type param hoisting tests timeout
- **hoistLetBindings** (2): Let binding hoisting timeouts
- **Inference** (~7): Let-polymorphism, over-generalization, nested let, mapFirstLetter, mutual recursion
- **Checking** (2): Mutual ref failure Flow, either in triple — timeout during inference
- **Monads** (1): Error traces — timeout in Rewriting

**Root cause:** Generated Java code for Hoisting and Inference modules uses deeply recursive functional patterns (fold-and-rewrite traversals, repeated `rewriteAndFoldTermWithTypeContext` calls) optimized for Haskell's lazy evaluation. The `Supplier<T>` thunking (Fix 33) addresses let-bindings but not the recursive traversal functions themselves.

#### 2. Annotation test mismatches (15 failures, 21%)
- **"outer overrides inner"** (5): Nested `setTermAnnotation` produces nested `Annotated` layers instead of merging into one annotation map
- **"unset"** (6): Unsetting annotations/descriptions doesn't correctly remove keys
- **"set multiple"** (2): Multi-annotation tests
- **"outer description overrides"** (2): Nested `setTermDescription`

**Root cause:** `reduceTerm` can't fully evaluate the annotation primitive functions. The compiled Java code works correctly (e.g., `Annotations.setTermAnnotation()` properly deannotates and rebuilds), but when evaluating the same logic as a Hydra term AST via beta-reduction, the reducer can't step through the complete chain of `deannotateTerm → aggregateAnnotations → fromList → ...`.

#### 3. Type checking / inference (6 failures, 9%)
- **Reconstructed type collapse** (2): `lists foldl`, `optionals maybe` — inference assigns correct types but `Checking.typeOf` collapses distinct type variables (produces `(t0 → t0 → t0)` instead of `(t0 → t1 → t0)`)
- **Swapped type applications** (2): `nested projection`, `chained projections` — concrete type applications to `maybe` are in wrong order (`maybe!⟨IntList⟩⟨int32⟩` vs expected `maybe!⟨int32⟩⟨IntList⟩`)
- **Missing schema types** (2): `CoderDirection case`, `applied case statement` — `hydra.compute.Coder` not in test graph schema

#### 4. Flows.map eval (2 failures, 3%)
- `map abs`, `map negate` — Flow unwrapping: error says "expected wrap(hydra.compute.Flow) but found λs.λt.(...)" — `reduceTerm` can't resolve the Flow newtype wrapper

### Post-Generation Fix Script
After running `update-java-kernel` and `update-java-eval-lib`, the following fixes must be reapplied:
1. ~~**Name.java**: Add `Comparable<Name>` interface and `compareTo` method~~ — **RESOLVED** (codegen)
2. ~~**128 unit-like classes**: Fix phantom Boolean equals/hashCode~~ — **RESOLVED** (codegen)
3. ~~**`Literal.java`**: `Binary.equals()` using `Arrays.equals`~~ — **RESOLVED** (codegen)
4. ~~**`Value.java`**: `Number_.equals()` using `BigDecimal.compareTo`~~ — **RESOLVED** (codegen)
5. ~~**`Parsers.java`**: Re-add `lazy()` method~~ — **RESOLVED** (Sources)
6. ~~**`Parser.java`**: Wrap recursive parser calls in `Parsers.lazy(() -> ...)`~~ — **RESOLVED** (Sources)

All post-generation fixes have been promoted to codegen or Sources level. No manual fixups required after regeneration.

## Status

**Test results: 1961 tests, 6 failures, 1955 passing (99.7%)**

| Category | Count | Details |
|----------|-------|---------|
| ~~Hoisting timeouts~~ | ~~39~~ | **RESOLVED** — hoisting deduplication in Sources |
| ~~Annotation eval~~ | ~~15~~ | **RESOLVED** — hand-written term bindings + primitive shadowing fix |
| Inference/Checking | 6 | Type variable conflation (foldl, maybe — `t1` becomes `t0`), swapped type applications (nested projection, chained projections, unwrap with maybe, CoderDirection) — algorithmic bugs in generated inference/checking code |
| ~~Flows eval~~ | ~~2~~ | **RESOLVED** — flows.map wrap fix |
| ~~Eta expansion~~ | ~~1~~ | **RESOLVED** — added schema types |
| ~~Monads~~ | ~~1~~ | **RESOLVED** — part of hoisting fix |

**Remaining 6 failures** are algorithmic bugs in the generated `Inference.java` and `Checking.java` code. These originate from `Hydra.Sources.Kernel.Terms.{Inference,Checking}` and would require fixes there followed by regeneration.

## Cheats

These were direct modifications to generated `src/gen-main/java` files that got overwritten when `update-java-kernel` re-generated the code. All have now been promoted to codegen or Sources level.

### ~~Cheat 1: `Name.java` — `Comparable<Name>` interface~~ — **RESOLVED**

**Original issue:** `Name` is a newtype wrapper around `String`. Java's `SortOn`, `Sort`, `maps/Keys`, `maps/ToList` need keys to be `Comparable` for deterministic ordering. Without this, 77+ tests fail with `ClassCastException`.

**Resolution:** Implemented in codegen (Issue #131). Modified `Coder.hs` to add `Comparable<ClassName>` to all serializable types:
- Split `interfaceTypes` into `serializableTypes` (Serializable only, for inner variant classes) and `interfaceTypes` (Serializable + Comparable, for top-level classes)
- Added `recordCompareToMethod` — generates type-aware `compareTo` for record types with lexicographic field-by-field comparison
- Added `variantCompareToMethod` — each inner variant class overrides `compareTo(ParentType)`
- Type-aware field comparison: `byte[]` uses `Arrays.compare`, non-comparable types use `hashCode` comparison, comparable types use `compareTo`

**Files modified:** `hydra-ext/src/main/haskell/Hydra/Ext/Staging/Java/{Names,Utils,Coder}.hs`

### ~~Cheat 2: `core/Literal.java` — `Binary.equals()` with `Arrays.equals`~~ — **RESOLVED**

**Original issue:** `Binary` wraps `byte[]`. Java's `byte[].equals()` is reference equality (inherited from `Object`), not value equality.

**Resolution:** Implemented in codegen. The `eqClause` function in `Coder.hs` now detects `byte[]` fields (via `isBinaryType`) and emits `java.util.Arrays.equals(value, o.value)` instead of `value.equals(o.value)`.

### ~~Cheat 3: `json/model/Value.java` — `Number_.equals()` with `BigDecimal.compareTo`~~ — **RESOLVED**

**Original issue:** `Number_` wraps `java.math.BigDecimal`. `BigDecimal.equals()` considers scale — `new BigDecimal("1.0").equals(new BigDecimal("1"))` is `false`. For JSON numeric equality, scale should be ignored.

**Resolution:** Implemented in codegen. The `eqClause` function in `Coder.hs` now detects `BigDecimal`/`BigInteger` fields (via `isBigNumericType`) and emits `value.compareTo(o.value) == 0` instead of `value.equals(o.value)`.

### ~~Cheat 4: 128 unit-like classes — phantom `Boolean` equality~~ — **RESOLVED**

**Original issue:** Unit variants (e.g., `IntegerType.Int32`) had a phantom `Boolean value = true` field with semantically incorrect equals/hashCode.

**Resolution:** Implemented in codegen. Unit variant classes (empty fields list) now:
- Have no phantom `Boolean value` field
- `equals()` does instanceof check and returns `true`
- `hashCode()` returns `0`
- `compareTo()` returns `0` for same variant

### ~~Cheat 5: `parsers/Parsers.java` — `lazy()` method~~ — **RESOLVED**

**Original issue:** Recursive parser combinators caused StackOverflow due to eager static initialization.

**Resolution:** Implemented in Haskell Sources. The `lazy` combinator was added to `Hydra.Sources.Kernel.Terms.Parsers`, which generates the `lazy` method into `Parsers.java` automatically.

### ~~Cheat 6: `json/parser/Parser.java` — lazy wrappers for recursive parsers~~ — **RESOLVED**

**Original issue:** `jsonValue()` → `jsonArray()` → `jsonValue()` recursion caused StackOverflow.

**Resolution:** Implemented in Haskell Sources. `Hydra.Sources.Json.Parser` now uses `parsers.lazy` for recursive `jsonValue` references, which generates the `Parsers.lazy(() -> ...)` wrappers in `Parser.java` automatically.

## Other Fixes

### Fix 57: Ordered sets — TreeSet for Comparable elements

All Java set operations (`FromList`, `Singleton`, `Insert`, `Delete`, `Union`, `Unions`, `Intersection`, `Difference`, `Map`) were using `HashSet`, which has non-deterministic iteration order. Haskell's `Data.Set` is a balanced BST with `Ord`-ordered iteration.

**Fix:** Added `FromList.orderedSet()` utility that creates `TreeSet` when elements implement `Comparable`, falling back to `LinkedHashSet`. Updated all set operations to use this utility.

**Files modified:** All files in `hydra-java/src/main/java/hydra/lib/sets/`

### Fix 58: Ordered maps — TreeMap for Comparable keys

Similarly, all map operations used `HashMap`. Haskell's `Data.Map` is ordered by key.

**Fix:** Added `FromList.orderedMap()` utility that creates `TreeMap` when keys implement `Comparable`. Updated map operations.

**Files modified:** All files in `hydra-java/src/main/java/hydra/lib/maps/`

### Fix 59: Alpha-equivalence normalization for type checking tests

Java's inference engine may assign type variables in a different order than Haskell's. Improved the `normalizeTypeVarNames()` function in TestSuiteRunner to rename type variables by order of first occurrence in the body.

**Impact:** The "Inferred term" comparisons now pass for `lists foldl` and `optionals maybe`. However, the "Reconstructed type" step still fails — `Checking.typeOf` collapses distinct type variables. This is a deeper bug in the Checking module.

**Files modified:** `hydra-java/src/test/java/hydra/TestSuiteRunner.java`
