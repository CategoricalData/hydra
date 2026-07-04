# Hydra DSL Guide (Java)

This guide explains Hydra's domain-specific language (DSL) utilities for constructing types and terms in Java.

> **Status note (0.17+).** Two namespaces matter, and the boundary is architectural:
> `hydra.overlay.java.*` is **hand-written** host-native code; `hydra.*` (e.g. `hydra.dsl.lib.*`)
> is **generated**. The **hand-written direct + phantom DSLs** live under
> `hydra.overlay.java.dsl.*`: the direct DSLs `hydra.overlay.java.dsl.{Types,Terms,Literals,LiteralTypes}`,
> the phantom-typed DSL `hydra.overlay.java.dsl.meta.Phantoms` (+ `hydra.overlay.java.dsl.meta.Defs`
> and the helper layer `hydra.overlay.java.dsl.Helpers`). These are the foundation for the host-native
> Java coder sources at `packages/hydra-java/src/main/java/hydra/sources/` (post-#344). The **library
> wrappers** are the *generated* modules `hydra.dsl.lib.*` (`Lists`, `Maps`, `Sets`, `Logic`, `Math_`,
> `Optionals`, `Strings`, `Literals`, `Eithers`, `Pairs`, `Equality`, …), imported directly. The
> hand-written *meta-level* domain DSLs once sketched here (a `hydra.dsl.meta.Core`/`Graph`/`Compute`
> accessor layer) were **never built and have been removed from this guide**. Note this is distinct from
> the *generated per-record constructor* DSLs `hydra.dsl.Core`/`hydra.dsl.Graph`/… (e.g.
> `hydra.dsl.Core.lambda(...)`), which do exist and are covered under "Accessing kernel-type fields" below.
> For typed field access in the coder sources, authors use `Phantoms` projections (`proj(...)`) + `Helpers`;
> for primitive calls, the generated `hydra.dsl.lib.*`. For full kernel-source authoring, use the Haskell DSL
> ([DSL Guide (Haskell)](dsl-guide.md)); the Java DSL covers Java-coder authoring only.

**Note**: Hydra provides DSLs in all five implementation languages (Haskell, Java, Python, Scala, and Lisp).
This guide focuses on the Java DSLs.
For the comprehensive Haskell DSL guide (including kernel development context), see [DSL Guide (Haskell)](dsl-guide.md).
For the Python DSLs, see [DSL Guide (Python)](dsl-guide-python.md).
For the Scala DSLs, see [DSL Guide (Scala)](dsl-guide-scala.md).

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
6. [Library wrappers](#library-wrappers)
7. [Type definitions](#type-definitions)
8. [Term definitions](#term-definitions)
9. [Common patterns](#common-patterns)
10. [Working with generated code](#working-with-generated-code)
11. [Error handling](#error-handling)
12. [Examples in the codebase](#examples-in-the-codebase)

## Overview

Hydra-Java provides a layered DSL system for working with Hydra types and terms:

| Layer | Module | Purpose |
|-------|--------|---------|
| **Direct DSLs** | `hydra.overlay.java.dsl.Types`, `hydra.overlay.java.dsl.Terms` | Raw construction of `Type` and `Term` instances |
| **Phantom-typed DSL** | `hydra.overlay.java.dsl.meta.Phantoms` | `TypedTerm<A>` term construction; the phantom `A` documents intent (not a load-bearing check — see below) |
| **Definition + helper layer** | `hydra.overlay.java.dsl.meta.Defs`, `hydra.overlay.java.dsl.Helpers` | Fluent `define(NS,"name").doc(…).lam(…).to(…)` builder + `ref`; `typeref`/`typeDef`/`doc` for assembling modules |
| **Library wrappers** | `hydra.dsl.lib.*` (generated) | Typed wrappers around Hydra primitives (lists, sets, maps, etc.) |

The Direct DSLs are suitable for casual use: constructing test fixtures, prototyping, or building types.
The Phantom-typed DSL plus the definition/helper layer are used for writing Hydra kernel source code in
Java, mirroring the Haskell DSLs used in `packages/hydra-haskell/src/main/haskell/Hydra/Sources/`. The
`hydra.dsl.lib.*` wrappers are generated (not hand-written) and imported directly.

## The DSL variants

### 1. Direct Types DSL

**Module**: `hydra.overlay.java.dsl.Types`

Constructs `Type` instances directly. Used for defining Hydra data types (records, unions, wrappers).

```java
import hydra.overlay.java.dsl.Types;

Type personType = Types.record(
    Types.field("name", Types.string()),
    Types.field("age", Types.int32()));
```

### 2. Direct Terms DSL

**Module**: `hydra.overlay.java.dsl.Terms`

Constructs raw `Term` instances. Useful for test data and simple term construction.

```java
import hydra.overlay.java.dsl.Terms;

Term person = Terms.record(new Name("Person"),
    Terms.field("name", Terms.string("Alice")),
    Terms.field("age", Terms.int32(30)));
```

### 3. Phantom-typed DSL

**Module**: `hydra.overlay.java.dsl.meta.Phantoms`

Wraps raw `Term` construction in a `TypedTerm<A>` whose phantom parameter `A` *names the intended*
Hydra type at the Java level.

> **On the phantom `A`:** it is documentation of intent, not a load-bearing type check. The DSL is a
> meta-program that manipulates `Term` as data, so most builders are `<A> TypedTerm<A>` with `A`
> unconstrained (it surfaces as `<?>`/`Object` at nearly every call site — see the many `TypedTerm<?>`
> signatures in `Phantoms`). It gives near-zero compile-time safety today; treat it as a readable
> annotation and **do not** write code that depends on `A` being accurate. This is a deliberate stance
> (a usability review considered both making `A` real end-to-end and dropping it, and chose to leave the
> signatures as-is): the meta-program never needs it, and threading real Hydra types through `A` would be
> a large change for no functional gain.

```java
import static hydra.overlay.java.dsl.meta.Phantoms.*;

TypedTerm<String> greeting = string("hello");
TypedTerm<Integer> age = int32(30);
TypedTerm<Object> identity = lambda("x", var("x"));
```

For typed field *access* on kernel types, there is no separate hand-written accessor "domain DSL" — the
coder sources project with `Phantoms` (`proj(...)`) and the `hydra.overlay.java.dsl.Helpers` layer. (For
*constructing* kernel records there are the generated `hydra.dsl.Core`/`Graph`/… constructor DSLs — see
"Accessing kernel-type fields" below.) See the host-native sources at
`packages/hydra-java/src/main/java/hydra/sources/` for concrete examples.

### 4. Library wrappers

The generated `hydra.dsl.lib.*` modules provide typed wrappers around Hydra primitive functions,
so a primitive call reads as a normal method call rather than a raw `primitive2(...)`.

```java
import hydra.dsl.lib.Sets;
import hydra.dsl.lib.Lists;

TypedTerm<java.util.Set<R>> u = Sets.union(s1, s2);
TypedTerm<java.util.List<B>> ys = Lists.map(f, xs);
```

These modules are **generated** (one per `hydra.lib.*` library) and imported directly; they are not
hand-written. See [Library wrappers](#library-wrappers) below for the full list.

## When to use each variant

| Scenario | Recommended DSLs | Why |
|----------|----------------|-----|
| Defining Hydra types | Direct Types DSL | Constructs `Type` instances for type modules |
| Simple term construction | Direct Terms DSL | Quick and straightforward |
| Writing kernel source code | Phantom-typed DSL + `Helpers` | Type safety + module-assembly helpers |
| Field access on kernel types | `Phantoms.proj(...)` | Typed projection onto a variable |
| Primitive function calls | Library wrappers | `Sets.union(a, b)` instead of raw `primitive2(...)` |

**Rule of thumb**:
- **Type modules** (defining data types): Use `hydra.overlay.java.dsl.Types` with `Types.record()`, `Types.union()`, `Types.wrap()`
- **Term modules** (defining functions): Use `import static hydra.overlay.java.dsl.meta.Phantoms.*`
- **Quick prototyping**: Use `hydra.overlay.java.dsl.Terms` directly

## Direct DSLs (Types and Terms)

### Constructing Types

```java
import hydra.core.*;
import hydra.overlay.java.dsl.Types;

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
import hydra.overlay.java.dsl.Terms;

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
It wraps raw `Term` values in `TypedTerm<A>` to provide compile-time type tracking.

### Import pattern

```java
import hydra.typed.TypedBinding;
import hydra.typed.TypedTerm;
import hydra.util.Maybe;

import static hydra.overlay.java.dsl.meta.Phantoms.*;
```

### Literals

```java
TypedTerm<String> greeting = string("hello");
TypedTerm<Integer> age = int32(42);
TypedTerm<Boolean> flag = boolean_(true);
TypedTerm<Boolean> yes = true_();
TypedTerm<Boolean> no = false_();
```

### Functions

```java
// Lambda (single parameter)
TypedTerm<Object> id = lambda("x", var("x"));

// Lambda (multiple parameters — curried)
TypedTerm<Object> add = lambdas(List.of("x", "y"),
    primitive2(new Name("hydra.lib.math.add"), var("x"), var("y")));

// Function application
TypedTerm<Object> result = apply(var("f"), int32(5));

// Composition
TypedTerm<Object> composed = compose(var("g"), var("f"));

// Constant function
TypedTerm<Object> alwaysTrue = constant(true_());

// Identity
TypedTerm<Object> id2 = identity();
```

### Data structures

```java
// Lists
TypedTerm<List<Integer>> nums = list(int32(1), int32(2), int32(3));

// Pairs
TypedTerm<Object> kv = pair(string("key"), int32(42));

// Optional values
TypedTerm<Object> some = just(int32(42));
TypedTerm<Object> none = nothing();

// Either
TypedTerm<Object> ok = right(int32(42));
TypedTerm<Object> err = left(string("error"));
```

### Records

```java
// Construct a record (requires type name + fields)
TypedTerm<Object> person = record(Person.TYPE_NAME,
    field(Person.FIELD_NAME_NAME, string("Alice")),
    field(Person.FIELD_NAME_AGE, int32(30)));
```

### Union injection

```java
// Inject into a union type
TypedTerm<Object> circle = inject(Shape.TYPE_NAME, Shape.FIELD_NAME_CIRCLE,
    float64(3.14));

// Unit injection (for enum-like variants)
TypedTerm<Object> none = injectUnit(FloatType.TYPE_NAME, FloatType.FIELD_NAME_FLOAT32);
```

### Pattern matching (`cases`/`match`)

```java
// match creates a case elimination (unapplied)
TypedTerm<Object> matcher = match(Term.TYPE_NAME,
    Maybe.just(var("default")),         // default case
    field(Term.FIELD_NAME_LITERAL,      // case: literal
        lambda("lit", string("found a literal"))),
    field(Term.FIELD_NAME_VARIABLE,     // case: variable
        lambda("v", string("found a variable"))));

// cases applies the match to an argument
TypedTerm<Object> result = cases(Term.TYPE_NAME, var("myTerm"),
    Maybe.nothing(),                    // no default
    field(Term.FIELD_NAME_LITERAL,
        lambda("lit", var("lit"))),
    field(Term.FIELD_NAME_VARIABLE,
        lambda("v", var("v"))));
```

### Let bindings

```java
// Single let binding
TypedTerm<Object> expr = let1("x", int32(5),
    apply(var("add"), var("x")));

// Multiple let bindings
TypedTerm<Object> expr2 = lets(List.of(
    field(new Name("x"), int32(5)),
    field(new Name("y"), int32(10))),
    apply(apply(var("add"), var("x")), var("y")));
```

### Projection (field access)

```java
// Create a field accessor function
TypedTerm<Object> getName = project(Person.TYPE_NAME, Person.FIELD_NAME_NAME);

// Apply it
TypedTerm<Object> name = apply(getName, var("person"));
```

The combined "project a field, then apply to a named variable" pattern
is so common that `Phantoms` provides a `proj` shortcut:

```java
// Equivalent to: apply(project(Person.TYPE_NAME, Person.FIELD_NAME), var("person"))
TypedTerm<Object> name = proj(Person.TYPE_NAME, Person.FIELD_NAME, "person");
```

Overloads accept `String` or `Name` for the type/field arguments, and
either a `String` variable name (which becomes `var("...")`) or a
`TypedTerm<?>` for the receiver. Prefer `proj()` in DSL source modules —
it's the idiomatic form.

If the field has a thunked type (e.g., `unit -> T`, used to defer
expression evaluation for benchmarking; see `UniversalTestCase.actual`),
the projection alone yields the thunk — *not* its forced value. Force
with an extra `apply(..., unit())`:

```java
// field type is `unit -> string` — force the thunk
TypedTerm<Object> value = apply(
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
TypedTerm<Object> hydraName = wrap(Name.TYPE_NAME, string("myName"));

// Unwrap function
TypedTerm<Object> unwrapper = unwrap(Name.TYPE_NAME);
```

### Primitive functions

```java
// Reference a primitive
TypedTerm<Object> addPrim = primitive(new Name("hydra.lib.math.add"));

// Apply primitives with 1, 2, or 3 arguments
TypedTerm<Object> len = primitive1(new Name("hydra.lib.strings.length"), var("s"));
TypedTerm<Object> sum = primitive2(new Name("hydra.lib.math.add"), var("x"), var("y"));
```

### Documentation

```java
// Attach documentation to a term
TypedTerm<Object> documented = doc("Adds two numbers", var("add"));
```

## Accessing kernel-type fields

There is no separate "domain DSL" of typed accessors in Java — author field access with `Phantoms`
projection. `proj(typeName, fieldName, varName)` is the idiomatic form (project-then-apply onto a
variable):

```java
import static hydra.overlay.java.dsl.meta.Phantoms.*;

// Lambda.body of the term bound to "lam"
TypedTerm<Object> body = proj(Lambda.TYPE_NAME, Lambda.FIELD_NAME_BODY, "lam");
// AnnotatedTerm.annotation of the term bound to "at"
TypedTerm<Object> ann = proj(AnnotatedTerm.TYPE_NAME, AnnotatedTerm.FIELD_NAME_ANNOTATION, "at");
```

For constructing kernel records, use the `Terms.record(...)` / `Phantoms` constructors directly, or the
generated `hydra.dsl.*` constructor DSLs (e.g. `hydra.dsl.Core.lambda(...)`). The host-native sources at
`packages/hydra-java/src/main/java/hydra/sources/` are the canonical worked examples.

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

Library wrappers provide phantom-typed interfaces to Hydra's primitive functions. They are
**generated** — one `hydra.dsl.lib.<Library>` module per `hydra.lib.*` library — so you import and call
them directly rather than hand-rolling `primitive2(...)` wrappers:

```java
import hydra.dsl.lib.Sets;
import hydra.dsl.lib.Lists;

TypedTerm<java.util.Set<R>> u = Sets.union(s1, s2);
TypedTerm<Integer> n = Lists.length(xs);
TypedTerm<B> acc = Lists.foldl(f, init, xs);
```

The generated modules are: `Lists`, `Maps`, `Sets`, `Logic`, `Math_`, `Optionals`, `Strings`,
`Literals`, `Eithers`, `Pairs`, `Equality`, `Chars` (plus any other `hydra.lib.*` library). Each method
name matches the primitive's local name; each is fully typed via `TypedTerm<A>`.

The underlying primitive-reference form is still available when you need the unapplied primitive as a
term: `Terms.primitive("hydra.lib.sets.union")` yields the `Term.Variable` for that primitive.

## Type definitions

Type-level modules define Hydra data types using the Direct Types DSL.
Each type definition is a `Binding` (a name-term pair).

### Pattern

```java
import hydra.core.*;
import hydra.overlay.java.dsl.Types;

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
Each function definition is a `TypedBinding<A>` (a phantom-typed name-term pair).

### Pattern

Definitions use the **fluent builder** — the blessed idiom across all Java coder sources:
`def("name").doc("...").lam("x").lam("y").to(() -> body)`. It reads top-to-bottom (name, doc,
parameters, then body) instead of the inside-out `def("name", () -> doc("...", lambda("x", ...)))`
nesting. The two forms are exactly equivalent — `.to(() -> body)` composes `doc(desc, lambda([params],
body))`, omitting the `doc`/`lambda` wrappers when none are given — but the fluent form is what to write.
The body passed to `.to(() -> ...)` stays lazy (a `Supplier`), so a definition may reference sibling
`Def` fields declared later in the class.

```java
import hydra.typed.*;
import hydra.util.Maybe;
import static hydra.overlay.java.dsl.meta.Phantoms.*;

public class MyFunctions {
    public static final ModuleName NS = new ModuleName("my.namespace");

    // Flat form (still supported); the fluent def(String) below is preferred.
    private static Def def(String localName, Supplier<TypedTerm<?>> body) {
        return Defs.define(NS, localName, body);
    }
    // Fluent form: def("name").doc("...").lam("x").to(() -> body)
    private static Defs.DefBuilder def(String localName) {
        return Defs.define(NS, localName);
    }

    // Simple function: pattern match + extract body
    public static final Def deannotateTerm = def("deannotateTerm")
        .doc("Remove annotations from a term")
        .lam("term")
        .to(() ->
            cases(Term.TYPE_NAME, var("term"),
                Maybe.just(var("term")),       // default: return unchanged
                field(Term.FIELD_NAME_ANNOTATED,
                    lambda("at",
                        apply(var("deannotateTerm"),
                            annotatedTermBody(var("at")))))));
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
TypedTerm<Object> fn = lambda("term",
    cases(Term.TYPE_NAME, var("term"),
        Maybe.just(var("term")),                    // default: identity
        field(Term.FIELD_NAME_ANNOTATED,            // handle one case
            lambda("at", annotatedTermBody(var("at"))))));
```

### Pattern 2: Let-binding with rewrite

Bind a local transform, pass it to a rewriting function:

```java
TypedTerm<Object> fn = lambda("typ",
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
TypedTerm<Object> vars = let1("dfltVars",
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
TypedTerm<Object> replaceFn = lambda("recurse", lambda("t",
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
5. **Fluent builders** and **copy-update methods** for record types (see below)

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

### Constructing record values: builders and copy-update

Every generated record type carries two native-Java affordances for construction, so applications
do not have to call the all-args constructor directly or re-implement builder boilerplate.

**Fluent builder.**
Each record exposes a static `builder()` factory and a nested `Builder` class with one setter per
field (named after the field) and a `build()` that returns the immutable record:

```java
import hydra.core.Binding;

Binding b = Binding.builder()
    .name(new hydra.core.Name("x"))
    .term(myTerm)
    .typeScheme(hydra.util.Optional.empty())
    .build();
```

For generic records the type parameters are threaded through, so the builder stays type-safe:

```java
// hydra.coders.Coder<V1, V2>
Coder<A, B> c = Coder.<A, B>builder()
    .encode(myEncode)
    .decode(myDecode)
    .build();   // returns Coder<A, B>
```

**Copy-update methods.**
Each record also has one `withFieldName(...)` method per field, returning a new instance with that one
field replaced and all others copied — useful for tweaking a single field of an immutable value:

```java
Binding b2 = b.withName(new hydra.core.Name("y"));   // same term + typeScheme, new name
```

Notes:
- Setters and copy-update methods are emitted for **every** generated record (no opt-in flag).
- A field whose name is a Java reserved word (e.g. `default`, `static`, `implements`) gets a
  trailing underscore in its setter, matching the field/parameter escaping (e.g. `.default_(...)`).
- A field literally named `build` or `builder` would collide with the generated methods and is
  likewise escaped to `build_` / `builder_`.
- Builders do not null-check in `build()`; Hydra-generated code never passes nulls, and user code is
  expected to supply every field (the all-args constructor remains available as well).

## Error handling

Hydra computations use `Either<Error, A>` for error handling (the former Flow monad
was removed in #245). An `InferenceContext` value is threaded alongside the graph
and carries the fresh-type-variable counter and the current subterm-path trace.

```java
import hydra.util.Either;
import hydra.typing.InferenceContext;
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

All hand-written DSLs live under `overlay/java/hydra-kernel/src/main/java/hydra/overlay/java/dsl/`
(namespace `hydra.overlay.java.dsl.*`); the library wrappers are generated.

| File | Description |
|------|-------------|
| `overlay/java/hydra-kernel/src/main/java/hydra/overlay/java/dsl/meta/Phantoms.java` | Phantom-typed DSL (all operations) |
| `overlay/java/hydra-kernel/src/main/java/hydra/overlay/java/dsl/meta/Defs.java` | Module-definition helpers (`define`/`ref`/`definitionsOf`) |
| `overlay/java/hydra-kernel/src/main/java/hydra/overlay/java/dsl/Helpers.java` | `typeref`/`typeDef`/`doc`/`typeScheme` helpers |
| `overlay/java/hydra-kernel/src/main/java/hydra/overlay/java/dsl/Types.java` | Direct Types DSL |
| `overlay/java/hydra-kernel/src/main/java/hydra/overlay/java/dsl/Terms.java` | Direct Terms DSL |
| `hydra.dsl.lib.*` (generated; e.g. `hydra.dsl.lib.Lists`/`Maps`/`Sets`/`Logic`/`Math_`/`Optionals`/`Strings`) | Library wrappers — generated, imported directly |
| `packages/hydra-java/src/main/java/hydra/sources/` | Live host-native Java coder DSL sources (reference for current Phantoms idiom) |

## Related Documentation

- [DSL Guide (Haskell)](dsl-guide.md) - Comprehensive DSL guide for kernel development
- [DSL Guide (Python)](dsl-guide-python.md) - Python DSL guide
- [DSL Guide (Scala)](dsl-guide-scala.md) - Scala DSL guide
- [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) - Core Hydra concepts
- [Implementation](implementation.md) - Implementation details and architecture
- [Hydra-Java README](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-java) - Getting started
