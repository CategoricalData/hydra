# Hydra DSL Guide (Python)

This guide explains Hydra's domain-specific language (DSL) utilities for constructing types and terms in Python.

**Note**: Hydra provides DSLs in all five implementation languages (Haskell, Java, Python, Scala, and Lisp).
This guide focuses on the Python DSLs.
For the comprehensive Haskell DSL guide (including kernel development context), see [DSL Guide (Haskell)](dsl-guide.md).
For the Java DSLs, see [DSL Guide (Java)](dsl-guide-java.md).

## Prerequisites

**Before using the DSL utilities**, you should:
- Understand Hydra's core concepts: [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts)
- Know basic Python syntax
- Have set up Hydra-Python locally
  (see [Hydra-Python README](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-python))
- Python 3.12+ required (for `match` statement support in generated code)

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

Hydra-Python provides a layered DSL system for working with Hydra types and terms:

| Layer | Module | Purpose |
|-------|--------|---------|
| **Direct DSLs** | `hydra.dsl.types`, `hydra.dsl.terms` | Raw construction of `Type` and `Term` instances |
| **Phantom-typed DSL** | `hydra.dsl.meta.phantoms` | Type safety via `TTerm[A]` phantom types |
| **Domain-specific DSLs** | `hydra.dsl.meta.core`, `hydra.dsl.meta.graph`, `hydra.dsl.meta.compute` | Typed accessors for Hydra kernel types |
| **Library wrappers** | `hydra.dsl.meta.lib.*` | Typed wrappers around Hydra primitives (lists, sets, maps, etc.) |

The Direct DSLs are suitable for casual use: constructing test fixtures, prototyping, or building types.
The Phantom-typed and Domain-specific DSLs are designed for writing Hydra kernel source code in Python,
mirroring the Haskell DSLs used in `packages/hydra-haskell/src/main/haskell/Hydra/Sources/`.

## The DSL variants

### 1. Direct Types DSL

**Module**: `hydra.dsl.types`

Constructs `Type` instances directly. Used for defining Hydra data types.

```python
import hydra.dsl.types as T

person_type = T.record([
    T.field("name", T.string()),
    T.field("age", T.int32())])
```

### 2. Direct Terms DSL

**Module**: `hydra.dsl.terms`

Constructs raw `Term` instances. Useful for test data and simple term construction.

```python
import hydra.dsl.terms as Terms

person = Terms.record(Name("Person"), [
    Terms.field("name", Terms.string("Alice")),
    Terms.field("age", Terms.int32(30))])
```

### 3. Phantom-typed DSL

**Module**: `hydra.dsl.meta.phantoms`

Wraps raw `Term` construction with `TTerm[A]` phantom types for type tracking.

```python
import hydra.dsl.meta.phantoms as P

greeting = P.string("hello")
age = P.int32(42)
identity = P.lambda_("x", P.var("x"))
```

### 4. Domain-specific DSLs

**Modules**: `hydra.dsl.meta.core`, `hydra.dsl.meta.graph`, `hydra.dsl.meta.compute`

Provide typed field accessors and constructors for Hydra kernel types.

```python
import hydra.dsl.meta.core as Core

# Extract the body of a lambda term
body = Core.lambda_body(P.var("myLambda"))

# Extract the parameter of a lambda
param = Core.lambda_parameter(P.var("myLambda"))
```

### 5. Library wrappers

**Modules**: `hydra.dsl.meta.lib.sets`, `hydra.dsl.meta.lib.lists`, etc.

Typed wrappers around Hydra primitive functions.

```python
import hydra.dsl.meta.lib.sets as Sets
import hydra.dsl.meta.lib.lists as Lists

# Set operations
empty = Sets.empty()
union = Sets.union(set_a, set_b)
from_list = Sets.from_list(my_list)

# List operations
folded = Lists.foldl(fn, init, my_list)
```

## When to use each variant

| Scenario | Recommended DSLs | Why |
|----------|----------------|-----|
| Defining Hydra types | Direct Types DSL | Constructs `Type` instances for type modules |
| Simple term construction | Direct Terms DSL | Quick and straightforward |
| Writing kernel source code | Phantom-typed + Domain-specific | Type tracking + domain accessors |
| Field access on kernel types | Domain-specific DSLs | `Core.lambda_body(t)` vs manual projection |
| Primitive function calls | Library wrappers | `Sets.union(a, b)` vs raw `P.primitive2(...)` |

**Rule of thumb**:
- **Type modules** (defining data types): Use `hydra.dsl.types as T` with `T.record()`, `T.union()`, `T.wrap()`
- **Term modules** (defining functions): Use `hydra.dsl.meta.phantoms as P` with domain DSLs
- **Quick prototyping**: Use `hydra.dsl.terms` directly

## Direct DSLs (Types and Terms)

### Constructing Types

```python
import hydra.dsl.types as T
from hydra.core import Name

# Primitive types
string_type = T.string()
int32_type = T.int32()
boolean_type = T.boolean()

# All integer types
int8_type = T.int8()
int16_type = T.int16()
int32_type = T.int32()
int64_type = T.int64()
uint8_type = T.uint8()
uint16_type = T.uint16()
uint32_type = T.uint32()
uint64_type = T.uint64()
bigint_type = T.bigint()

# Float types
float32_type = T.float32()
float64_type = T.float64()
bigfloat_type = T.bigfloat()

# Arbitrary-precision decimal (Python Decimal)
decimal_type = T.decimal()

# Container types
string_list = T.list_(T.string())
string_map = T.map_(T.string(), T.int32())
maybe_int = T.optional(T.int32())
int_set = T.set_(T.int32())

# Pair and either
pair_type = T.pair(T.string(), T.int32())
either_type = T.either(T.string(), T.int32())

# Function type
fn = T.function(T.string(), T.int32())

# Record type
person = T.record([
    T.field("name", T.string()),
    T.field("age", T.int32())])

# Union type
shape = T.union([
    T.field("circle", T.float64()),
    T.field("rectangle", T.pair(T.float64(), T.float64()))])

# Wrapper type (newtype)
name = T.wrap(T.string())

# Type variable (forward reference)
self_ref = T.variable("hydra.core.Term")

# Unit type
unit = T.unit()
```

### Constructing Terms

```python
import hydra.dsl.terms as Terms
from hydra.core import Name, Field

# Literals
hello = Terms.string("hello")
answer = Terms.int32(42)
flag = Terms.boolean(True)

# Lists
numbers = Terms.list_([Terms.int32(1), Terms.int32(2), Terms.int32(3)])

# Records
person = Terms.record(Name("Person"), [
    Field(Name("name"), Terms.string("Alice")),
    Field(Name("age"), Terms.int32(30))])

# Lambdas
identity = Terms.lambda_("x", Terms.variable("x"))

# Application
applied = Terms.apply(identity, Terms.int32(42))

# Optional values
just_val = Terms.just(Terms.int32(42))
nothing_val = Terms.nothing()
```

### Working with Union Types

Python uses `match` statements (3.10+) or `isinstance` checks for pattern matching:

```python
from hydra.core import Term

# Python 3.10+ match statement
def describe(term: Term) -> str:
    match term:
        case Term.Literal(value):
            return "A literal value"
        case Term.List(value):
            return f"A list with {len(value)} elements"
        case _:
            return "Some other term"

# Pre-3.10 isinstance approach
def describe(term: Term) -> str:
    if isinstance(term, Term.Literal):
        return "A literal value"
    elif isinstance(term, Term.List):
        return f"A list with {len(term.value)} elements"
    else:
        return "Some other term"
```

## Phantom-typed DSL

The phantom-typed DSL is the core of Hydra's Python metaprogramming system.
It wraps raw `Term` values in `TTerm[A]` to provide type tracking.

### Import pattern

```python
import hydra.core
import hydra.dsl.meta.phantoms as P
import hydra.dsl.meta.core as Core
from hydra.core import Name
from hydra.dsl.python import Just, Nothing
from hydra.phantoms import TBinding, TTerm
```

### Literals

```python
greeting = P.string("hello")
age = P.int32(42)
flag = P.boolean(True)
yes = P.true
no = P.false
```

### Functions

```python
# Lambda (single parameter)
id_fn = P.lambda_("x", P.var("x"))

# Lambda (multiple parameters — curried)
add = P.lambdas(["x", "y"],
    P.primitive2(Name("hydra.lib.math.add"), P.var("x"), P.var("y")))

# Function application
result = P.apply(P.var("f"), P.int32(5))

# Composition
composed = P.compose(P.var("g"), P.var("f"))

# Constant function
always_true = P.constant(P.true)

# Identity
id_fn2 = P.identity()
```

### Data structures

```python
# Lists
nums = P.list_([P.int32(1), P.int32(2), P.int32(3)])

# Pairs
kv = P.pair(P.string("key"), P.int32(42))

# Optional values
some = P.just(P.int32(42))
none = P.nothing()

# Either
ok = P.right(P.int32(42))
err = P.left(P.string("error"))
```

### Records

```python
# Construct a record (requires type name + fields)
person = P.record(hydra.core.PERSON__NAME, [
    P.field(hydra.core.PERSON__NAME__NAME, P.string("Alice")),
    P.field(hydra.core.PERSON__AGE__NAME, P.int32(30))])
```

### Union injection

```python
# Inject into a union type
circle = P.inject(hydra.core.SHAPE__NAME, hydra.core.SHAPE__CIRCLE__NAME,
    P.float64(3.14))

# Unit injection (for enum-like variants)
f32 = P.inject_unit(hydra.core.FLOAT_TYPE__NAME,
    hydra.core.FLOAT_TYPE__FLOAT32__NAME)
```

### Pattern matching (cases/match)

```python
# match creates a case elimination (unapplied)
matcher = P.match(hydra.core.TERM__NAME,
    Just(P.var("default")),                 # default case
    [P.field(hydra.core.TERM__LITERAL__NAME,    # case: literal
        P.lambda_("lit", P.string("found a literal"))),
     P.field(hydra.core.TERM__VARIABLE__NAME,    # case: variable
        P.lambda_("v", P.string("found a variable")))])

# cases applies the match to an argument
result = P.cases(hydra.core.TERM__NAME, P.var("myTerm"),
    Nothing(),                              # no default
    [P.field(hydra.core.TERM__LITERAL__NAME,
        P.lambda_("lit", P.var("lit"))),
     P.field(hydra.core.TERM__VARIABLE__NAME,
        P.lambda_("v", P.var("v")))])
```

**Note**: In Python, case fields are passed as a `list`, not as varargs.

### Let bindings

```python
# Single let binding
expr = P.let1("x", P.int32(5),
    P.apply(P.var("add"), P.var("x")))

# Multiple let bindings
expr2 = P.lets([
    P.field(Name("x"), P.int32(5)),
    P.field(Name("y"), P.int32(10))],
    P.apply(P.apply(P.var("add"), P.var("x")), P.var("y")))
```

### Projection (field access)

```python
# Create a field accessor function
get_name = P.project(hydra.core.PERSON__NAME, hydra.core.PERSON__NAME__NAME)

# Apply it
name = P.apply(get_name, P.var("person"))
```

### Wrap/unwrap

```python
# Wrap a value (create a newtype instance)
hydra_name = P.wrap(hydra.core.NAME__NAME, P.string("myName"))

# Unwrap function
unwrapper = P.unwrap(hydra.core.NAME__NAME)
```

### Primitive functions

```python
# Reference a primitive
add_prim = P.primitive(Name("hydra.lib.math.add"))

# Apply primitives with 1, 2, or 3 arguments
length = P.primitive1(Name("hydra.lib.strings.length"), P.var("s"))
sum_ = P.primitive2(Name("hydra.lib.math.add"), P.var("x"), P.var("y"))
```

### Documentation

```python
# Attach documentation to a term
documented = P.doc("Adds two numbers", P.var("add"))
```

## Domain-specific DSLs

The domain-specific DSLs (`core`, `graph`, `compute`) provide typed accessors
for Hydra's kernel types.

### Core DSL (`hydra.dsl.meta.core`)

```python
import hydra.dsl.meta.core as Core

# Field accessors
param = Core.lambda_parameter(P.var("lam"))         # Lambda.parameter
body = Core.lambda_body(P.var("lam"))               # Lambda.body
at_body = Core.annotated_term_body(P.var("at"))     # AnnotatedTerm.body
ann = Core.annotated_term_annotation(P.var("at"))   # AnnotatedTerm.annotation
tname = Core.injection_type_name(P.var("inj"))      # Injection.typeName
```

### Generated name constants

Generated Hydra modules provide `TYPE_NAME` and `FIELD_NAME_*` constants using
Python naming conventions (double underscores for namespace separation):

```python
import hydra.core

# Type names
hydra.core.TERM__NAME                    # Name("hydra.core.Term")
hydra.core.LAMBDA__NAME                  # Name("hydra.core.Lambda")

# Field names (TYPE__FIELD__NAME pattern)
hydra.core.TERM__LITERAL__NAME           # Name("literal")
hydra.core.TERM__VARIABLE__NAME          # Name("variable")
hydra.core.LAMBDA__PARAMETER__NAME       # Name("parameter")
hydra.core.LAMBDA__BODY__NAME            # Name("body")
```

Always use these constants rather than constructing `Name` instances manually.

## Library wrappers

Library wrappers provide typed interfaces to Hydra's primitive functions.

### Sets (`hydra.dsl.meta.lib.sets`)

```python
import hydra.dsl.meta.lib.sets as Sets

empty = Sets.empty()
union = Sets.union(set_a, set_b)
delete = Sets.delete(elem, set_a)
from_list = Sets.from_list(my_list)
to_list = Sets.to_list(my_set)
```

### Lists (`hydra.dsl.meta.lib.lists`)

```python
import hydra.dsl.meta.lib.lists as Lists

folded = Lists.foldl(fn, init, my_list)
mapped = Lists.map_(fn, my_list)
concat = Lists.concat(list_of_lists)
```

### Logic (`hydra.dsl.meta.lib.logic`)

```python
import hydra.dsl.meta.lib.logic as Logic

result = Logic.if_else(condition, then_branch, else_branch)
negated = Logic.not_(condition)
```

### Equality (`hydra.dsl.meta.lib.equality`)

```python
import hydra.dsl.meta.lib.equality as Equality

eq = Equality.equal_name(name_a, name_b)
```

### Maybes (`hydra.dsl.meta.lib.maybes`)

```python
import hydra.dsl.meta.lib.maybes as Maybes

mapped = Maybes.map_(fn, maybe_val)
```

## Type definitions

Type-level modules define Hydra data types using the Direct Types DSL.

### Pattern

```python
import hydra.annotations
import hydra.dsl.types as T
from hydra.core import Binding, Name

NS = "my.namespace"

def define(local_name: str, typ) -> Binding:
    return hydra.annotations.type_element(
        Name(NS + "." + local_name), typ)

# Forward references
_Person = T.variable(NS + ".Person")
_Address = T.variable(NS + ".Address")

# Type definitions
person: Binding = define("Person",
    T.record([
        T.field("name", T.string()),
        T.field("age", T.int32()),
        T.field("address", _Address)]))

address: Binding = define("Address",
    T.record([
        T.field("street", T.string()),
        T.field("city", T.string())]))
```

### Complete example: hydra.core

See `heads/python/src/main/python/hydra/dsl/meta/examples/core_types.py` for a complete
implementation of all 33 `hydra.core` types using this pattern.

## Term definitions

Term-level modules define Hydra functions using the Phantom-typed DSL.

### Pattern

```python
import hydra.core
import hydra.packaging
import hydra.dsl.meta.phantoms as P
import hydra.dsl.meta.core as Core
from hydra.dsl.python import Just, Nothing
from hydra.phantoms import TBinding

ns = hydra.packaging.Namespace("my.namespace")

def define(lname: str, term) -> TBinding:
    return P.definition_in_namespace(ns, lname, term)

# Qualified self-reference helper
def _self(lname: str):
    return P.var("my.namespace." + lname)

# Simple function
deannotate_term: TBinding = define("deannotateTerm",
    P.doc("Remove annotations from a term",
    P.lambda_("term",
        P.cases(hydra.core.TERM__NAME, P.var("term"),
            Just(P.var("term")),
            [P.field(hydra.core.TERM__ANNOTATED__NAME,
                P.lambda_("at",
                    P.apply(_self("deannotateTerm"),
                        Core.annotated_term_body(P.var("at")))))]))))
```

### Self-references in Python

**Important**: Python module-level variables cannot reference themselves during construction
(unlike Haskell's lazy bindings or Java's interface fields). Use a qualified variable
reference via `P.var("namespace.functionName")` pattern:

```python
# WRONG - Python error: name 'deannotate_term' not referenced yet
deannotate_term = define("deannotateTerm",
    P.lambda_("term",
        P.apply(P.ref(deannotate_term), ...)))  # NameError!

# RIGHT - use qualified variable reference
def _self(lname: str):
    return P.var("my.namespace." + lname)

deannotate_term = define("deannotateTerm",
    P.lambda_("term",
        P.apply(_self("deannotateTerm"), ...)))  # Works!
```

### Complete example: hydra.rewriting

See `heads/python/src/main/python/hydra/dsl/meta/examples/rewriting.py` for a partial
implementation of `hydra.rewriting` demonstrating:

- Simple pattern matching (`deannotate_term`)
- Multiple case branches (`deannotate_and_detype_term`)
- Composition with projection (`deannotate_type`)
- Let-bindings and recursive rewriting (`deannotate_type_recursive`)
- Nested pattern matching (`is_lambda`)
- Complex operations with sets, folds, and binding-aware rewriting (`free_variables_in_term`)
- Structural rewriting patterns (`remove_term_annotations`)
- TraversalOrder dispatching (`fold_over_term`)

## Common patterns

### Pattern 1: Simple case dispatch

Match on a union type, handle one variant, pass others through:

```python
fn = P.lambda_("term",
    P.cases(hydra.core.TERM__NAME, P.var("term"),
        Just(P.var("term")),                        # default: identity
        [P.field(hydra.core.TERM__ANNOTATED__NAME,  # handle one case
            P.lambda_("at",
                Core.annotated_term_body(P.var("at"))))]))
```

### Pattern 2: Let-binding with rewrite

Bind a local transform, pass it to a rewriting function:

```python
fn = P.lambda_("typ",
    P.let1("f",
        P.lambda_("recurse", P.lambda_("t",
            P.cases(hydra.core.TYPE__NAME, P.var("t"),
                Just(P.apply(P.var("recurse"), P.var("t"))),
                [P.field(hydra.core.TYPE__ANNOTATED__NAME,
                    P.lambda_("at",
                        P.apply(P.var("recurse"),
                            Core.annotated_type_body(P.var("at")))))]))),
        P.apply(P.apply(_self("rewriteType"), P.var("f")), P.var("typ"))))
```

### Pattern 3: Fold with set operations

```python
import hydra.dsl.meta.lib.sets as Sets
import hydra.dsl.meta.lib.lists as Lists

vars = P.let1("dfltVars",
    Lists.foldl(
        P.lambda_("s", P.lambda_("t",
            Sets.union(P.var("s"),
                P.apply(_self("freeVariablesInTerm"), P.var("t"))))),
        Sets.empty(),
        P.apply(_self("subterms"), P.var("term"))),
    # then match on specific cases...
    P.cases(hydra.core.TERM__NAME, P.var("term"),
        Just(P.var("dfltVars")),
        [...]))
```

### Pattern 4: Binding-aware rewriting

```python
replace_fn = P.lambda_("recurse", P.lambda_("t",
    P.cases(hydra.core.TERM__NAME, P.var("t"),
        Just(P.apply(P.var("recurse"), P.var("t"))),
        [P.field(hydra.core.TERM__FUNCTION__NAME,
            P.match(hydra.core.FUNCTION__NAME,
                Just(P.apply(P.var("recurse"), P.var("t"))),
                [P.field(hydra.core.FUNCTION__LAMBDA__NAME,
                    P.lambda_("l",
                        Logic.if_else(
                            Equality.equal_name(
                                Core.lambda_parameter(P.var("l")),
                                P.var("name")),
                            P.var("t"),
                            P.apply(P.var("recurse"), P.var("t")))))]))])))
```

## Working with generated code

Generated Python code for Hydra types uses dataclasses with nested classes for union variants.

### Example: Generated Term type

```python
# hydra/core.py (generated)
class Term:
    class Annotated:
        value: AnnotatedTerm
    class Application:
        value: Application
    class Literal:
        value: Literal
    class Variable:
        value: Name
    # ...

TERM__NAME = Name("hydra.core.Term")
TERM__LITERAL__NAME = Name("literal")
TERM__VARIABLE__NAME = Name("variable")
# ...
```

## Error handling

Hydra computations use `Either[Error, A]` for error handling (the former Flow monad
was removed in #245). A `Context` value is threaded alongside the graph and carries a
trace stack and diagnostic messages.

```python
from hydra.util import Either
from hydra.context import Context
from hydra.errors import Error

# Create a successful result
ok = Either.right("result")

# Map over a result
mapped = hydra.lib.eithers.map_.apply(lambda s: len(s), ok)

# Chain computations (bind / flatMap)
bound = hydra.lib.eithers.bind.apply(result1, lambda value: Either.right(value + " processed"))

# Create a failure (Error is a tagged-union type; construct a variant from hydra.errors)
err = Either.left(Error.other("something went wrong"))

# Inspect a result
match result:
    case Either.Right(value):
        # use value
        pass
    case Either.Left(e):
        # e is an Error value
        pass
```

## Working with FrozenDict

Hydra maps use `FrozenDict` for immutability:

```python
from hydra.dsl.frozen_dict import FrozenDict

d = FrozenDict({"key1": "value1", "key2": "value2"})
value = d["key1"]
d2 = FrozenDict({**d, "key3": "value3"})
```

## Examples in the codebase

| File | Description |
|------|-------------|
| `heads/python/src/main/python/hydra/dsl/meta/phantoms.py` | Phantom-typed DSL (all operations) |
| `heads/python/src/main/python/hydra/dsl/meta/core.py` | Core domain DSL (field accessors) |
| `heads/python/src/main/python/hydra/dsl/meta/graph.py` | Graph domain DSL |
| `heads/python/src/main/python/hydra/dsl/meta/compute.py` | Compute domain DSL |
| `heads/python/src/main/python/hydra/dsl/meta/lib/sets.py` | Sets library wrapper |
| `heads/python/src/main/python/hydra/dsl/meta/lib/lists.py` | Lists library wrapper |
| `heads/python/src/main/python/hydra/dsl/meta/examples/core_types.py` | Complete hydra.core type definitions |
| `heads/python/src/main/python/hydra/dsl/meta/examples/rewriting.py` | Partial hydra.rewriting term definitions |
| `heads/python/src/main/python/hydra/dsl/types.py` | Direct Types DSL |
| `heads/python/src/main/python/hydra/dsl/terms.py` | Direct Terms DSL |

## Related Documentation

- [DSL Guide (Haskell)](dsl-guide.md) - Comprehensive DSL guide for kernel development
- [DSL Guide (Java)](dsl-guide-java.md) - Java DSL guide
- [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) - Core Hydra concepts
- [Implementation](implementation.md) - Implementation details and architecture
- [Hydra-Python README](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-python) - Getting started
