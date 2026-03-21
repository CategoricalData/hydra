# Issue #274: Add Tests for Typeclass Inference

> **GitHub Issue**: [#274 - Add tests for typeclass inference](https://github.com/CategoricalData/hydra/issues/274)
>
> **Status**: Open
>
> **Created**: 2026-03-06
>
> **Related**: #164 (Implement limited typeclass inference)

## Summary

Create a new test source module `Hydra.Sources.Test.Inference.Classes` with test cases that exercise Hydra's two built-in type classes (`equality` and `ordering`) during type inference. The existing inference test suite has no tests that verify type class constraints on inferred TypeSchemes. Some tests may initially fail, revealing weaknesses in the current type class inference.

## Background

### Current state of type class constraints

Hydra has two built-in type classes:
- **`ordering`** (analogous to Haskell's `Ord`): Required for Map keys (all 20 map primitives), Set elements (all 14 set primitives), `lists.sort`, `lists.sortOn`, and comparison operators (`equality.compare`, `gt`, `gte`, `lt`, `lte`, `max`, `min`)
- **`equality`** (analogous to Haskell's `Eq`): Required for `equality.equal`, `lists.elem`, `lists.group`, `lists.nub`

### How constraints enter TypeSchemes

1. **Primitive type schemes** carry constraints (defined in `Libraries.hs` via `vOrd`/`vEq` helpers)
2. When a primitive is referenced during inference, `inferTypeOfPrimitive` extracts constraints from the TypeScheme
3. `substInClassConstraints` propagates constraints through type substitution
4. `generalize` closes accumulated constraints into the final TypeScheme at let-binding sites
5. `mergeClassConstraints` unions constraint sets when the same variable appears in multiple maps

### Gap in existing tests

The existing tests in `AlgebraicTypes.hs` use `maps.map`, `maps.fromList`, `sets.fromList`, etc. but their expected TypeSchemes use `expectPoly`/`expectMono`, which always set `constraints = Nothing`. These tests verify the **type structure** is correct but not that **ordering/equality constraints** are inferred on the relevant type variables.

### Test comparison

The `expectMono`/`expectPoly` helpers call `T.mono`/`T.poly`, which always produce `TypeScheme` with `constraints = Nothing`. The test runner compares the inferred `TypeScheme` with the expected one. For constraint testing, the expected TypeSchemes must include `Just constraintMap` in the `constraints` field.

The user will handle updating the "show" output for TypeSchemes to display class constraints. No new helper like `expectConstrained` is needed — instead, a new `polyConstrained` (or similar) function in `Hydra.Dsl.Meta.Types` is needed so the expected TypeScheme can carry constraints.

## Implementation Plan

### 1. Add `polyConstrained` helper to `Hydra.Dsl.Meta.Types`

Add a variant of `poly` that accepts constraint information:

```haskell
-- | Create a polymorphic type scheme with class constraints
-- Example: polyConstrained ["k", "v"] [("k", ["ordering"])] (T.map (T.var "k") (T.var "v"))
polyConstrained :: [String] -> [(String, [String])] -> TTerm Type -> TTerm TypeScheme
polyConstrained params constraints t = Phantoms.record _TypeScheme [
  Phantoms.field _TypeScheme_variables (Phantoms.list (name <$> params)),
  Phantoms.field _TypeScheme_type t,
  Phantoms.field _TypeScheme_constraints constraintsTerm]
  where
    constraintsTerm
      | null constraints = Phantoms.nothing
      | otherwise = Phantoms.just $ Phantoms.map $ M.fromList
          [(name v, typeVariableMetadata (Phantoms.set (S.fromList (name <$> classes))))
          | (v, classes) <- constraints]
```

Also add a corresponding `expectPolyConstrained` helper in `Hydra.Dsl.Meta.Testing`.

### 2. Create `Hydra.Sources.Test.Inference.Classes` module

New file: `hydra-haskell/src/main/haskell/Hydra/Sources/Test/Inference/Classes.hs`

Test categories:

#### A. Direct primitive application (monomorphic — constraints vanish)

When all type variables are instantiated to concrete types, constraints disappear:

- `maps.fromList [("a", 1)]` => `Map String Int32` (mono, no constraints)
- `sets.fromList [1, 2, 3]` => `Set Int32` (mono, no constraints)
- `equality.equal 1 2` => `Boolean` (mono, no constraints)
- `lists.sort [1, 2, 3]` => `[Int32]` (mono, no constraints)

#### B. Polymorphic primitive references (constraints on type vars)

When a primitive with constrained type variables is referenced without full application, the free type variables should carry constraints:

- `primitive maps.fromList` => `forall k v. Ord k => [(k, v)] -> Map k v`
- `primitive maps.lookup` => `forall k v. Ord k => k -> Map k v -> Maybe v`
- `primitive sets.fromList` => `forall x. Ord x => [x] -> Set x`
- `primitive sets.member` => `forall x. Ord x => x -> Set x -> Bool`
- `primitive equality.equal` => `forall x. Eq x => x -> x -> Bool`
- `primitive lists.sort` => `forall x. Ord x => [x] -> [x]`
- `primitive lists.nub` => `forall x. Eq x => [x] -> [x]`
- `primitive lists.elem` => `forall x. Eq x => x -> [x] -> Bool`

#### C. Partial application preserving constraints

When a constrained primitive is partially applied with a lambda that doesn't fix the constrained variable:

- `\k -> maps.lookup k` => `forall k v. Ord k => k -> Map k v -> Maybe v`
- `\x -> sets.member x` => `forall x. Ord x => x -> Set x -> Bool`
- `\x -> \y -> equality.equal x y` => `forall x. Eq x => x -> x -> Bool`

#### D. Constraint propagation through let bindings

When a let-bound function uses a constrained primitive, the constraint should propagate to the binding's generalized type:

- `let lookup = \k -> \m -> maps.lookup k m in lookup` => `forall k v. Ord k => k -> Map k v -> Maybe v`
- `let member = \x -> \s -> sets.member x s in member` => `forall x. Ord x => x -> Set x -> Bool`
- `let fromList = maps.fromList in fromList` => `forall k v. Ord k => [(k, v)] -> Map k v`

#### E. Constraint propagation through function composition

When a function composes constrained primitives, constraints on shared variables should merge:

- `\xs -> maps.fromList (lists.map (\x -> (x, x)) xs)` — key type needs Ord
- `\f -> \xs -> sets.fromList (lists.map f xs)` — result element type needs Ord
- `\m -> maps.map lists.sort m` — value type needs Ord (from `lists.sort`), key type needs Ord (from `maps.map`)

#### F. Multiple constraints on the same variable

- `\xs -> let s = sets.fromList xs; e = lists.elem (lists.head xs) xs in (s, e)` — `xs`'s element type needs both Ord (from sets.fromList) and Eq (from lists.elem). Whether both constraints are inferred is a key test.

#### G. Nested container types

- `\xss -> sets.fromList (lists.map sets.fromList xss)` — outer elements are `Set x`, inner elements are `x`; both need Ord
- `\m -> maps.map sets.fromList m` — map values are lists, converted to sets; value element type needs Ord, key type needs Ord

### 3. Register in All.hs

Add `Classes` to the import list and test group list in `Hydra.Sources.Test.Inference.All`.

### 4. Register in cabal

Add the new module to `hydra-haskell.cabal` (and `hydra-ext.cabal` if needed).

### 5. Build and observe results

```bash
cd hydra-haskell && stack build && stack test
```

Some tests may fail because:
- The inference engine may not currently propagate constraints for certain patterns
- The test comparison may need TypeScheme comparison to include constraints (user will handle the "show" output update)
- Map/set **literal** inference (`inferTypeOfMap`, `inferTypeOfSet`) does not add constraints — only primitive function type schemes do

## Notes

- Tests that are expected to fail initially should use `tag_disabled` so the test suite passes while revealing the gaps
- The order of type variables (`t0`, `t1`, ...) in expected TypeSchemes must match the inference engine's normalization order
- Constraint variable names in expected TypeSchemes must match the normalized variable names assigned by the inference engine
