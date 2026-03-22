# Java code generator: PartialVisitor diamond inference bug

Date: 2026-03-19

## The problem

When the Lisp coder modules are generated to Java, two methods in `Coder.java` produce
`PartialVisitor<>()` instantiations that fail to compile. The Java compiler cannot infer
the correct type parameter from the diamond operator `<>` in these specific cases.

### Error locations

Both are in `hydra-java/src/gen-main/java/hydra/ext/lisp/coder/Coder.java`:

1. **Line ~959** in `encodeApplication`: `Term.PartialVisitor<>()` inside a method returning
   `Either<T2, Expression>`. The `otherwise` and `visit(Application)` methods are generated
   with return type `Either<Expression, Expression>` instead of `Either<T2, Expression>`.

2. **Line ~1211** in `encodeTermDefinition`: `Function.PartialVisitor<>()` inside a method
   returning `Either<T2, TopLevelFormWithComments>`. Same mismatch — methods use the concrete
   type instead of `T2`.

### The compilation errors

```
error: <anonymous Coder$14$1> is not abstract and does not override abstract method visit(Application) in Visitor
error: otherwise(Term) in <anonymous Coder$14$1> cannot implement otherwise(Term) in PartialVisitor
  return type Either<Expression,Expression> is not compatible with Either<T2,Expression>
```

### Manual fix (works, but lost on regeneration)

Replace:
```java
return (dMidFun).accept(new hydra.core.Term.PartialVisitor<>() {
    public Either<Expression, Expression> otherwise(Term instance) { ... }
    public Either<Expression, Expression> visit(Term.Application app3) { ... }
```

With:
```java
return (dMidFun).accept(new hydra.core.Term.PartialVisitor<Either<T2, Expression>>() {
    public Either<T2, Expression> otherwise(Term instance) { ... }
    public Either<T2, Expression> visit(Term.Application app3) { ... }
```

Same pattern for the `Function.PartialVisitor` at the second location.

## Why this only affects the Lisp coder

The Haskell, Java, and Python coders have been generated to Java for years without this issue.
The Lisp coder was first generated to Java during this integration work (March 2026). The
problem is specific to **nested `cases` (pattern matches) inside polymorphic functions**.

The Lisp coder's `encodeApplication` has this structure:

```haskell
encodeApplication = def "encodeApplication" $
  "dialect" ~> "cx" ~> "g" ~> lambda "rawFun" $ lambda "rawArg" $
    cases _Term (var "dFun") (Just defaultBranch)
    [_Term_application>>: lambda "app2" $
       ...
       cases _Term (var "dMidFun") (Just defaultBranch)    -- NESTED cases
       [_Term_application>>: lambda "app3" $ ...]]
```

The nested `cases _Term` produces a nested `PartialVisitor`. In the generated Java, the outer
method has type parameter `T2` (the error type), but the inner `PartialVisitor`'s methods
are generated with concrete types from the Hydra source rather than `T2`.

The other coders don't have nested `cases` on Term/Function inside polymorphic methods, so
they never trigger this bug.

## What was tried

### Approach 1: Disable diamond, always emit explicit type args

Changed `typeArgsOrDiamond` in the Java coder DSL source to always use
`TypeArgumentsOrDiamondArguments` instead of `TypeArgumentsOrDiamondDiamond` for
PartialVisitor instantiations.

**File**: `hydra-ext/src/main/haskell/Hydra/Ext/Sources/Java/Coder.hs`, line 4305

**Change**:
```haskell
-- Before:
"targs" <~ (typeArgsOrDiamond @@ (...))
-- After:
"targs" <~ (JavaDsl.typeArgumentsOrDiamondArguments (...))
```

**Result**: The DSL change was correct, but required regenerating the Haskell Java coder via
`update-haskell-ext-main`. This regeneration:
1. Produced the correct change in `Hydra/Ext/Java/Coder.hs` (line 2315 now uses explicit args)
2. But also corrupted unrelated DSL files (zeroed out `Hydra/Dsl/Ext/Io/Shex/Syntax.hs`,
   produced build errors in `Hydra/Dsl/Ext/Scala/Meta.hs` and others)

The `update-haskell-ext-main` tool regenerates ALL ext Haskell modules, and apparently has
issues with some modules on this branch. This made it impractical to use for a targeted fix.

### Approach 2: Patch the generated Haskell coder directly

Applied the same change directly to the generated file
`hydra-ext/src/gen-main/haskell/Hydra/Ext/Java/Coder.hs`, bypassing the DSL regeneration.

**Result**: hydra-ext built successfully. Regenerated Java, but the explicit type args resolved
to the **concrete** types (`Either<Expression, Expression>`) rather than the type variable
(`Either<T2, Expression>`). This made the code explicitly wrong rather than ambiguously wrong.

### Why explicit type args don't help

The issue is not diamond vs explicit — it's that the Java code generator computes the
PartialVisitor's type parameter from `cod` (the codomain type of the elimination being encoded).
In the Hydra type system, this is `Either (InContext Error) Expression`. When encoded to Java,
`InContext Error` maps to a concrete Java type, not to the type variable `T2`.

The type variable `T2` exists only in Java — it's introduced by the Java code generator when
the enclosing method has a polymorphic Hydra type that maps to Java generics. The PartialVisitor
instantiation site doesn't have access to this type variable because it computes its types from
the Hydra-level codomain, not from the Java-level method signature.

## What might work

### Option A: Thread type variables through the visitor codegen

The Java code generator's elimination encoding (around line 4300 of `Sources/Java/Coder.hs`)
computes `cod` (codomain type) and `rt` (Java reference type) from the Hydra type. When the
enclosing method has free type variables (like `T2`), the `rt` should use `T2` instead of the
concrete type.

This would require:
1. Detecting when the enclosing method has free type variables in its return type
2. Mapping from the concrete Hydra error type to the corresponding Java type variable
3. Using the type variable in the PartialVisitor instantiation

This is the correct fix but requires understanding the full type variable propagation in the
Java coder, particularly how `JavaEnvironment` tracks type parameters.

### Option B: Post-process the generated Java

Add a post-processing step (in the Java sync script or in bootstrap-from-json) that detects
`PartialVisitor<>()` or `PartialVisitor<Either<ConcreteType, X>>()` inside methods with
free type variables, and replaces the type parameter with the method's return type.

This is fragile but would work as an automated version of the manual patch.

### Option C: Restructure the Lisp coder to avoid nested cases

Refactor `encodeApplication` to extract the inner `cases` into a separate named function. This
would eliminate the nested PartialVisitor pattern and sidestep the issue entirely. The inner
pattern match on `dMidFun` could be a separate `encodeNestedApplication` function.

This is the simplest fix and doesn't require any changes to the Java code generator.

### Option D: Use Visitor instead of PartialVisitor for nested matches

If the inner `cases` used a full `Visitor` (with all branches) instead of `PartialVisitor`
(with `otherwise` + specific branches), the type inference might work because the `R` type
parameter would be fully determined by the exhaustive method signatures.

This would require adding explicit branches for all Term/Function variants in the Lisp coder
source, which is verbose but avoids the inference issue.
