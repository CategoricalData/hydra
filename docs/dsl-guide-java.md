# Hydra DSL Guide (Java)

This guide explains Hydra's domain-specific language (DSL) utilities for constructing types and terms in Java.

> **Status note (0.15+).** The **direct DSLs** (`hydra.dsl.Types`, `hydra.dsl.Terms`,
> `hydra.dsl.Literals`, `hydra.dsl.LiteralTypes`, and the `hydra.dsl.prims.*` wrappers)
> are current and in wide use. The **phantom-typed Java DSL** — `hydra.dsl.meta.Phantoms`
> plus library wrappers under `hydra.dsl.meta.lib.*` (`Lists`, `Maps`, `Sets`, `Logic`,
> `Maths`, `Maybes`, `Strings`, `Literals`) — is also current and is the foundation for
> the host-native Java coder sources at `packages/hydra-java/src/main/java/hydra/sources/`
> (post-#344). The domain-specific DSLs sketched in §4 below (`hydra.dsl.meta.Core`,
> `Graph`, `Compute`) and the `examples/` files in §"File reference" are **aspirational
> — not present in the current codebase**. For full kernel-source authoring, use the
> Haskell DSL ([DSL Guide (Haskell)](dsl-guide.md)); the Java DSL covers Java-coder
> authoring only.

**Note**: Hydra provides DSLs in all five implementation languages (Haskell, Java, Python, Scala, and Lisp).
This guide focuses on the Java DSLs.
For the comprehensive Haskell DSL guide (including kernel development context), see [DSL Guide (Haskell)](dsl-guide.md).
For the Python DSLs, see [DSL Guide (Python)](dsl-guide-python.md).

## Prerequisites

**Before using the DSL utilities**, you should:
- Understand Hydra's core concepts: [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts)
- Know basic Java syntax
- Have built Hydra-Java locally (see [Hydra-Java README](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-java))

## Table of Contents

1. [Overview](#overview)
2. [The DSL variants](#the-dsl-variants)
3. [When to use each variant](#when-to-use-each-variant)
4. [Direct DSLs (Types and Terms)](#direct-dsls-types-and-terms)
5. [Phantom-typed DSL](#phantom-typed-dsl)
6. [Domain-specific DSLs](#domain-specific-dsls)
7. [Library wrappers](#library-wrappers)
8. [Type definitions](#type-definitions)
9. [Term definitions](#term-definitions)
10. [Common patterns](#common-patterns)
11. [Working with generated code](#working-with-generated-code)
12. [Error handling](#error-handling)
13. [Examples in the codebase](#examples-in-the-codebase)

## Overview

Hydra-Java provides a layered DSL system for working with Hydra types and terms:

| Layer | Module | Purpose |
|-------|--------|---------|
| **Direct DSLs** | `hydra.dsl.Types`, `hydra.dsl.Terms` | Raw construction of `Type` and `Term` instances |
| **Phantom-typed DSL** | `hydra.dsl.meta.Phantoms` | Compile-time type safety via `TTerm<A>` phantom types |
| **Domain-specific DSLs** | `hydra.dsl.meta.Core`, `hydra.dsl.meta.Graph`, `hydra.dsl.meta.Compute` | Typed accessors for Hydra kernel types |
| **Library wrappers** | `hydra.dsl.meta.Lib.*` | Typed wrappers around Hydra primitives (lists, sets, maps, etc.) |

The Direct DSLs are suitable for casual use: constructing test fixtures, prototyping, or building types.
The Phantom-typed and Domain-specific DSLs are designed for writing Hydra kernel source code in Java,
mirroring the Haskell DSLs used in `packages/hydra-haskell/src/main/haskell/Hydra/Sources/`.

## The DSL variants

### 1. Direct Types DSL

**Module**: `hydra.dsl.Types`

Constructs `Type` instances directly. Used for defining Hydra data types (records, unions, wrappers).

```java
import hydra.dsl.Types;

Type personType = Types.record(
    Types.field("name", Types.string()),
    Types.field("age", Types.int32()));
```

### 2. Direct Terms DSL

**Module**: `hydra.dsl.Terms`

Constructs raw `Term` instances. Useful for test data and simple term construction.

```java
import hydra.dsl.Terms;

Term person = Terms.record(new Name("Person"),
    Terms.field("name", Terms.string("Alice")),
    Terms.field("age", Terms.int32(30)));
```

### 3. Phantom-typed DSL

**Module**: `hydra.dsl.meta.Phantoms`

Wraps raw `Term` construction with `TTerm<A>` phantom types for compile-time type safety.
The phantom type parameter `A` tracks the Hydra type at the Java level.

```java
import static hydra.dsl.meta.Phantoms.*;

TTerm<String> greeting = string("hello");
TTerm<Integer> age = int32(30);
TTerm<Object> identity = lambda("x", var("x"));
```

### 4. Domain-specific DSLs (aspirational — not yet implemented)

**Planned modules**: `hydra.dsl.meta.Core`, `hydra.dsl.meta.Graph`, `hydra.dsl.meta.Compute`.

These would provide typed field accessors and constructors for Hydra kernel types,
parallel to the Haskell `Hydra.Dsl.Meta.Core` / `Hydra.Dsl.Meta.Graph` modules. They
do not currently exist in the Java codebase. The example below is provisional:

```java
// Hypothetical — these classes don't exist yet:
import static hydra.dsl.meta.Core.*;

// Extract the body of a lambda term
TTerm<Object> body = lambdaBody(var("myLambda"));

// Construct a Lambda record
TTerm<Object> lam = lambda_(
    wrap(Term.TYPE_NAME, string("x")),
    nothing(),
    var("body"));
```

For now, use direct `Terms.*` constructors or the host-native sources at
`packages/hydra-java/src/main/java/hydra/sources/` as concrete examples of
authoring Java coder DSL code with `Phantoms` only.

### 5. Library wrappers

Typed wrappers around Hydra primitive functions, providing phantom-typed interfaces
to operations like set union, list fold, etc.

```java
// Inline library helpers (or import from hydra.dsl.meta.Lib.Sets, etc.)
static <R> TTerm<R> setsUnion(TTerm<?> s1, TTerm<?> s2) {
    return primitive2(new Name("hydra.lib.sets.union"), s1, s2);
}
```

## When to use each variant

| Scenario | Recommended DSLs | Why |
|----------|----------------|-----|
| Defining Hydra types | Direct Types DSL | Constructs `Type` instances for type modules |
| Simple term construction | Direct Terms DSL | Quick and straightforward |
| Writing kernel source code | Phantom-typed + Domain-specific | Type safety + domain accessors |
| Field access on kernel types | Domain-specific DSLs | `Core.lambdaBody(t)` instead of manual projection |
| Primitive function calls | Library wrappers | `setsUnion(a, b)` instead of raw `primitive2(...)` |

**Rule of thumb**:
- **Type modules** (defining data types): Use `hydra.dsl.Types` with `Types.record()`, `Types.union()`, `Types.wrap()`
- **Term modules** (defining functions): Use `import static hydra.dsl.meta.Phantoms.*` with domain DSLs
- **Quick prototyping**: Use `hydra.dsl.Terms` directly

## Direct DSLs (Types and Terms)

### Constructing Types

```java
import hydra.core.*;
import hydra.dsl.Types;

// Literal types
Type stringType = Types.string();
Type int32Type = Types.int32();
Type booleanType = Types.boolean_();

// Container types
Type stringList = Types.list(Types.string());
Type stringMap = Types.map(Types.string(), Types.int32());
Type maybeInt = Types.optional(Types.int32());
Type intSet = Types.set(Types.int32());

// Pair and either
Type pairType = Types.pair(Types.string(), Types.int32());
Type eitherType = Types.either_(Types.string(), Types.int32());

// Function type
Type fn = Types.function(Types.string(), Types.int32());

// Record type (anonymous)
Type person = Types.record(
    Types.field("name", Types.string()),
    Types.field("age", Types.int32()));

// Union type
Type shape = Types.union(
    Types.field("circle", Types.float64()),
    Types.field("rectangle", Types.pair(Types.float64(), Types.float64())));

// Wrapper type (newtype)
Type name = Types.wrap(Types.string());

// Type variable (forward reference)
Type selfRef = Types.variable("hydra.core.Term");

// Unit type
Type unit = Types.unit();
```

### Constructing Terms

```java
import hydra.core.*;
import hydra.dsl.Terms;

// Literals
Term hello = Terms.string("hello");
Term answer = Terms.int32(42);
Term flag = Terms.boolean_(true);

// Lists
Term numbers = Terms.list(Terms.int32(1), Terms.int32(2), Terms.int32(3));

// Records
Term person = Terms.record(new Name("Person"),
    Terms.field("name", Terms.string("Alice")),
    Terms.field("age", Terms.int32(30)));

// Lambdas
Term identity = Terms.lambda("x", Terms.var("x"));
Term add = Terms.lambda("x", Terms.lambda("y",
    Terms.apply(Terms.apply(Terms.primitive("hydra.lib.math.add"),
        Terms.var("x")), Terms.var("y"))));

// Application
Term applied = Terms.apply(identity, Terms.int32(42));

// Optional values
Term justVal = Terms.just(Terms.int32(42));
Term nothingVal = Terms.nothing();

// Let bindings
Term letExpr = Terms.let_("x", Terms.int32(5), Terms.var("x"));

// Union injection
Term circle = Terms.inject("Shape", "circle", Terms.float64(3.14));

// Wrapped term (newtype)
Term name = Terms.wrap("hydra.core.Name", Terms.string("myName"));
```

### Working with Union Types (Visitor pattern)

```java
import hydra.core.*;

// Pattern match on a Term
String describe(Term term) {
    return term.accept(new Term.PartialVisitor<String>() {
        @Override
        public String visit(Term.Literal instance) {
            return "A literal value";
        }
        @Override
        public String visit(Term.List instance) {
            return "A list with " + instance.value.size() + " elements";
        }
        @Override
        public String otherwise(Term instance) {
            return "Some other term";
        }
    });
}
```

## Phantom-typed DSL

The phantom-typed DSL is the core of Hydra's Java metaprogramming system.
It wraps raw `Term` values in `TTerm<A>` to provide compile-time type tracking.

### Import pattern

```java
import hydra.phantoms.TBinding;
import hydra.phantoms.TTerm;
import hydra.util.Maybe;

import static hydra.dsl.meta.Phantoms.*;
import static hydra.dsl.meta.Core.*;
```

### Literals

```java
TTerm<String> greeting = string("hello");
TTerm<Integer> age = int32(42);
TTerm<Boolean> flag = boolean_(true);
TTerm<Boolean> yes = true_();
TTerm<Boolean> no = false_();
```

### Functions

```java
// Lambda (single parameter)
TTerm<Object> id = lambda("x", var("x"));

// Lambda (multiple parameters — curried)
TTerm<Object> add = lambdas(List.of("x", "y"),
    primitive2(new Name("hydra.lib.math.add"), var("x"), var("y")));

// Function application
TTerm<Object> result = apply(var("f"), int32(5));

// Composition
TTerm<Object> composed = compose(var("g"), var("f"));

// Constant function
TTerm<Object> alwaysTrue = constant(true_());

// Identity
TTerm<Object> id2 = identity();
```

### Data structures

```java
// Lists
TTerm<List<Integer>> nums = list(int32(1), int32(2), int32(3));

// Pairs
TTerm<Object> kv = pair(string("key"), int32(42));

// Optional values
TTerm<Object> some = just(int32(42));
TTerm<Object> none = nothing();

// Either
TTerm<Object> ok = right(int32(42));
TTerm<Object> err = left(string("error"));
```

### Records

```java
// Construct a record (requires type name + fields)
TTerm<Object> person = record(Person.TYPE_NAME,
    field(Person.FIELD_NAME_NAME, string("Alice")),
    field(Person.FIELD_NAME_AGE, int32(30)));
```

### Union injection

```java
// Inject into a union type
TTerm<Object> circle = inject(Shape.TYPE_NAME, Shape.FIELD_NAME_CIRCLE,
    float64(3.14));

// Unit injection (for enum-like variants)
TTerm<Object> none = injectUnit(FloatType.TYPE_NAME, FloatType.FIELD_NAME_FLOAT32);
```

### Pattern matching (`cases`/`match`)

```java
// match creates a case elimination (unapplied)
TTerm<Object> matcher = match(Term.TYPE_NAME,
    Maybe.just(var("default")),         // default case
    field(Term.FIELD_NAME_LITERAL,      // case: literal
        lambda("lit", string("found a literal"))),
    field(Term.FIELD_NAME_VARIABLE,     // case: variable
        lambda("v", string("found a variable"))));

// cases applies the match to an argument
TTerm<Object> result = cases(Term.TYPE_NAME, var("myTerm"),
    Maybe.nothing(),                    // no default
    field(Term.FIELD_NAME_LITERAL,
        lambda("lit", var("lit"))),
    field(Term.FIELD_NAME_VARIABLE,
        lambda("v", var("v"))));
```

### Let bindings

```java
// Single let binding
TTerm<Object> expr = let1("x", int32(5),
    apply(var("add"), var("x")));

// Multiple let bindings
TTerm<Object> expr2 = lets(List.of(
    field(new Name("x"), int32(5)),
    field(new Name("y"), int32(10))),
    apply(apply(var("add"), var("x")), var("y")));
```

### Projection (field access)

```java
// Create a field accessor function
TTerm<Object> getName = project(Person.TYPE_NAME, Person.FIELD_NAME_NAME);

// Apply it
TTerm<Object> name = apply(getName, var("person"));
```

The combined "project a field, then apply to a named variable" pattern
is so common that `Phantoms` provides a `proj` shortcut:

```java
// Equivalent to: apply(project(Person.TYPE_NAME, Person.FIELD_NAME), var("person"))
TTerm<Object> name = proj(Person.TYPE_NAME, Person.FIELD_NAME, "person");
```

Overloads accept `String` or `Name` for the type/field arguments, and
either a `String` variable name (which becomes `var("...")`) or a
`TTerm<?>` for the receiver. Prefer `proj()` in DSL source modules —
it's the idiomatic form.

If the field has a thunked type (e.g., `unit -> T`, used to defer
expression evaluation for benchmarking; see `UniversalTestCase.actual`),
the projection alone yields the thunk — *not* its forced value. Force
with an extra `apply(..., unit())`:

```java
// field type is `unit -> string` — force the thunk
TTerm<Object> value = apply(
    apply(
        project("hydra.testing.UniversalTestCase", "actual"),
        var("ucase")),
    unit());
```

Missing the outer `apply(..., unit())` causes inference to fail with
`cannot unify string with (unit → string)` for every binding in the
containing module, since the inferencer processes them in a shared context.

### Wrap/unwrap

```java
// Wrap a value (create a newtype instance)
TTerm<Object> hydraName = wrap(Name.TYPE_NAME, string("myName"));

// Unwrap function
TTerm<Object> unwrapper = unwrap(Name.TYPE_NAME);
```

### Primitive functions

```java
// Reference a primitive
TTerm<Object> addPrim = primitive(new Name("hydra.lib.math.add"));

// Apply primitives with 1, 2, or 3 arguments
TTerm<Object> len = primitive1(new Name("hydra.lib.strings.length"), var("s"));
TTerm<Object> sum = primitive2(new Name("hydra.lib.math.add"), var("x"), var("y"));
```

### Documentation

```java
// Attach documentation to a term
TTerm<Object> documented = doc("Adds two numbers", var("add"));
```

## Domain-specific DSLs

The domain-specific DSLs (`Core`, `Graph`, `Compute`) provide typed accessors
for Hydra's kernel types. These are more readable than manual `project()` calls
and less error-prone than using raw field name strings.

### Core DSL (`hydra.dsl.meta.Core`)

```java
import static hydra.dsl.meta.Core.*;

// Field accessors (each is project + apply)
TTerm<Object> param = lambdaParameter(var("lam"));       // Lambda.parameter
TTerm<Object> body = lambdaBody(var("lam"));             // Lambda.body
TTerm<Object> atBody = annotatedTermBody(var("at"));     // AnnotatedTerm.body
TTerm<Object> ann = annotatedTermAnnotation(var("at"));  // AnnotatedTerm.annotation
TTerm<Object> tname = injectionTypeName(var("inj"));     // Injection.typeName

// Constructors (build records)
TTerm<Object> lam = lambda_(
    wrap(Name.TYPE_NAME, string("x")),
    nothing(),
    var("body"));

TTerm<Object> at = annotatedTerm(var("body"), var("annotation"));
```

### Generated name constants

Generated Hydra types provide `TYPE_NAME` and `FIELD_NAME_*` constants:

```java
// From hydra.core.Term (generated)
Term.TYPE_NAME                // Name("hydra.core.Term")
Term.FIELD_NAME_LITERAL       // Name("literal")
Term.FIELD_NAME_VARIABLE      // Name("variable")
Term.FIELD_NAME_APPLICATION    // Name("application")
// ... etc.

// From hydra.core.Lambda (generated)
Lambda.TYPE_NAME              // Name("hydra.core.Lambda")
Lambda.FIELD_NAME_PARAMETER   // Name("parameter")
Lambda.FIELD_NAME_BODY        // Name("body")
```

Always use these constants rather than constructing `Name` instances manually.
This ensures correctness and enables refactoring.

## Library wrappers

Library wrappers provide phantom-typed interfaces to Hydra's primitive functions.
They follow a consistent pattern:

```java
// Pattern: wrap primitive2/primitive1 with descriptive names
static <R> TTerm<R> setsUnion(TTerm<?> s1, TTerm<?> s2) {
    return primitive2(new Name("hydra.lib.sets.union"), s1, s2);
}

static <R> TTerm<R> setsEmpty() {
    return primitive(new Name("hydra.lib.sets.empty"));
}

static <R> TTerm<R> listsFoldl(TTerm<?> f, TTerm<?> init, TTerm<?> list) {
    return primitive3(new Name("hydra.lib.lists.foldl"), f, init, list);
}
```

## Type definitions

Type-level modules define Hydra data types using the Direct Types DSL.
Each type definition is a `Binding` (a name-term pair).

### Pattern

```java
import hydra.core.*;
import hydra.dsl.Types;

public interface MyTypes {
    String NS = "my.namespace";

    static Binding define(String localName, Type type) {
        return hydra.Annotations.typeElement(
            new Name(NS + "." + localName), type);
    }

    // Forward references
    Type _Person = Types.variable(NS + ".Person");
    Type _Address = Types.variable(NS + ".Address");

    // Type definitions
    Binding person = define("Person",
        Types.record(
            Types.field("name", Types.string()),
            Types.field("age", Types.int32()),
            Types.field("address", _Address)));

    Binding address = define("Address",
        Types.record(
            Types.field("street", Types.string()),
            Types.field("city", Types.string())));
}
```

### Cross-referencing types

Types in the same module reference each other through `Types.variable()`:

```java
// Forward reference to another type in this module
Type _Term = Types.variable("hydra.core.Term");

// Use it in a record field
Binding lambda = define("Lambda",
    Types.record(
        Types.field("parameter", _Name),
        Types.field("body", _Term)));
```

### Complete example: hydra.core

The `examples/` directory is aspirational — the file does not yet exist. For a
real reference, see the host-native Java coder sources at
`packages/hydra-java/src/main/java/hydra/sources/`, which use the same Phantoms
idiom against the full Hydra kernel.

## Term definitions

Term-level modules define Hydra functions using the Phantom-typed DSL.
Each function definition is a `TBinding<A>` (a phantom-typed name-term pair).

### Pattern

```java
import hydra.phantoms.*;
import hydra.util.Maybe;
import static hydra.dsl.meta.Phantoms.*;
import static hydra.dsl.meta.Core.*;

public class MyFunctions {
    public static final ModuleName NS = new ModuleName("my.namespace");

    private static Def def(String localName, Supplier<TTerm<?>> body) {
        return Defs.define(NS, localName, body);
    }

    // Simple function: pattern match + extract body
    TBinding<Object> deannotateTerm = define("deannotateTerm",
        doc("Remove annotations from a term",
        lambda("term",
            cases(Term.TYPE_NAME, var("term"),
                Maybe.just(var("term")),       // default: return unchanged
                field(Term.FIELD_NAME_ANNOTATED,
                    lambda("at",
                        apply(var("deannotateTerm"),
                            annotatedTermBody(var("at")))))))));
}
```

### Self-references

In Java, interface-level fields can reference themselves (the JVM handles initialization order).
Use `var("namespace.functionName")` for qualified self-references:

```java
// Recursive: apply same function to the body
apply(var("my.namespace.deannotateTerm"), annotatedTermBody(var("at")))
```

### Complete example: hydra.rewriting

The `examples/` directory is aspirational — the file does not yet exist. The same
patterns (simple pattern matching, case branches, composition with projection,
let-bindings, nested pattern matching, sets/folds/binding-aware rewriting,
structural rewriting, traversal-order dispatching) appear throughout the
host-native Java coder sources at `packages/hydra-java/src/main/java/hydra/sources/`,
which serve as the live working examples.

## Common patterns

### Pattern 1: Simple case dispatch

Match on a union type, handle one variant, pass others through:

```java
TTerm<Object> fn = lambda("term",
    cases(Term.TYPE_NAME, var("term"),
        Maybe.just(var("term")),                    // default: identity
        field(Term.FIELD_NAME_ANNOTATED,            // handle one case
            lambda("at", annotatedTermBody(var("at"))))));
```

### Pattern 2: Let-binding with rewrite

Bind a local transform, pass it to a rewriting function:

```java
TTerm<Object> fn = lambda("typ",
    let1("f",
        lambda("recurse", lambda("t",
            cases(Type.TYPE_NAME, var("t"),
                Maybe.just(apply(var("recurse"), var("t"))),
                field(Type.FIELD_NAME_ANNOTATED,
                    lambda("at",
                        apply(var("recurse"),
                            annotatedTypeBody(var("at")))))))),
        apply(apply(var("rewriteType"), var("f")), var("typ"))));
```

### Pattern 3: Fold with set operations

Accumulate results over subterms:

```java
TTerm<Object> vars = let1("dfltVars",
    listsFoldl(
        lambda("s", lambda("t",
            setsUnion(var("s"),
                apply(var("freeVariablesInTerm"), var("t"))))),
        setsEmpty(),
        apply(var("subterms"), var("term"))),
    // then match on specific cases...
    cases(Term.TYPE_NAME, var("term"),
        Maybe.just(var("dfltVars")),
        // ...
    ));
```

### Pattern 4: Binding-aware rewriting

Check whether a variable is shadowed before rewriting:

```java
TTerm<Object> replaceFn = lambda("recurse", lambda("t",
    cases(Term.TYPE_NAME, var("t"),
        Maybe.just(apply(var("recurse"), var("t"))),
        field(Term.FIELD_NAME_FUNCTION,
            match(Function.TYPE_NAME,
                Maybe.just(apply(var("recurse"), var("t"))),
                field(Function.FIELD_NAME_LAMBDA,
                    lambda("l",
                        // Stop if lambda shadows our variable
                        apply(apply(var("ifElse"),
                            equalName(lambdaParameter(var("l")), var("name"))),
                            var("t"),
                            apply(var("recurse"), var("t"))))))))));
```

## Working with generated code

Generated Java classes for Hydra types provide:

1. **Visitor pattern** for union types (`accept`, `Visitor<R>`, `PartialVisitor<R>`)
2. **Static name constants** (`TYPE_NAME`, `FIELD_NAME_*`)
3. **Serializable** implementations
4. **Comparable** implementations

### Example: Generated Term class

```java
// hydra.core.Term (generated)
public abstract class Term implements Serializable, Comparable<Term> {
    public static final Name TYPE_NAME = new Name("hydra.core.Term");
    public static final Name FIELD_NAME_LITERAL = new Name("literal");
    public static final Name FIELD_NAME_VARIABLE = new Name("variable");
    // ...

    public static final class Literal extends Term { ... }
    public static final class Variable extends Term { ... }
    // ...

    public abstract <R> R accept(Visitor<R> visitor);
}
```

## Error handling

Hydra computations use `Either<Error, A>` for error handling (the former Flow monad
was removed in #245). A `Context` value is threaded alongside the graph and carries a
trace stack and diagnostic messages.

```java
import hydra.util.Either;
import hydra.context.Context;
import hydra.errors.Error;
import hydra.graph.Graph;

// Create a successful result
Either<Error, String> ok = Either.right("result");

// Map over a result
Either<Error, Integer> mapped =
    hydra.lib.eithers.Map.apply(s -> s.length(), ok);

// Chain computations (bind / flatMap)
Either<Error, String> bound =
    hydra.lib.eithers.Bind.apply(result1, value ->
        Either.right(value + " processed"));

// Create a failure (Error is a tagged-union type; construct a variant from hydra.errors)
Either<Error, String> err = Either.left(Error.other("something went wrong"));

// Inspect a result
if (result.isRight()) {
    String value = result.get();
} else {
    Error failure = result.getLeft();
}
```

## Examples in the codebase

| File | Description |
|------|-------------|
| `packages/hydra-java/src/main/java/hydra/dsl/meta/Phantoms.java` | Phantom-typed DSL (all operations) |
| `packages/hydra-java/src/main/java/hydra/dsl/meta/Defs.java` | Module-definition helpers for the Java coder DSL |
| `packages/hydra-java/src/main/java/hydra/dsl/meta/lib/Lists.java`, `Maps.java`, `Sets.java`, `Logic.java`, `Maths.java`, `Maybes.java`, `Strings.java`, `Literals.java` | Library wrappers |
| `packages/hydra-java/src/main/java/hydra/sources/` | Live host-native Java coder DSL sources (reference for current Phantoms idiom) |
| `heads/java/src/main/java/hydra/dsl/Types.java` | Direct Types DSL (runtime) |
| `heads/java/src/main/java/hydra/dsl/Terms.java` | Direct Terms DSL (runtime) |

## Related Documentation

- [DSL Guide (Haskell)](dsl-guide.md) - Comprehensive DSL guide for kernel development
- [DSL Guide (Python)](dsl-guide-python.md) - Python DSL guide
- [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) - Core Hydra concepts
- [Implementation](implementation.md) - Implementation details and architecture
- [Hydra-Java README](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-java) - Getting started
