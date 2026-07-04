# Hydra DSL Guide (Python)

This guide explains Hydra's domain-specific language (DSL) utilities for constructing types and terms in Python.

**Note**: Hydra provides DSLs in all five implementation languages (Haskell, Java, Python, Scala, and Lisp).
This guide focuses on the Python DSLs.
For the comprehensive Haskell DSL guide (including kernel development context), see [DSL Guide (Haskell)](dsl-guide.md).
For the Java DSLs, see [DSL Guide (Java)](dsl-guide-java.md).
For the Scala DSLs, see [DSL Guide (Scala)](dsl-guide-scala.md).

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
| **Direct DSLs** | `hydra.overlay.python.dsl.types`, `hydra.overlay.python.dsl.terms` | Raw construction of `Type` and `Term` instances |
| **Phantom-typed DSL** | `hydra.overlay.python.dsl.meta.phantoms` | Type safety via `TypedTerm[A]` phantom types |
| **Domain-specific DSLs** | `hydra.dsl.core`, `hydra.dsl.graph` (generated) | Typed accessors for Hydra kernel types |
| **Library wrappers** | `hydra.dsl.lib.*` | Typed wrappers around Hydra primitives (lists, sets, maps, etc.) |
| **Term references** | `hydra.dsl.strip`, `hydra.dsl.serialization`, ... (generated) | Typed, rename-safe references to kernel functions (#467) |

The Direct DSLs are suitable for casual use: constructing test fixtures, prototyping, or building types.
The Phantom-typed and Domain-specific DSLs are designed for writing Hydra kernel source code in Python,
mirroring the Haskell DSLs used in `packages/hydra-haskell/src/main/haskell/Hydra/Sources/`.

Note the two namespaces: the hand-written authoring DSLs live under `hydra.overlay.python.dsl.*`
(host-native, authored in `overlay/python/`), while the **library wrappers `hydra.dsl.lib.*` are
generated** (one module per `hydra.lib.*` library) and imported directly. The `hydra.*` namespace is
exclusively generated/translingual; `hydra.overlay.python.*` is exclusively host-native (#501).

## The DSL variants

### 1. Direct Types DSL

**Module**: `hydra.overlay.python.dsl.types`

Constructs `Type` instances directly. Used for defining Hydra data types.

```python
import hydra.overlay.python.dsl.types as T

person_type = T.record([
    T.field("name", T.string()),
    T.field("age", T.int32())])
```

### 2. Direct Terms DSL

**Module**: `hydra.overlay.python.dsl.terms`

Constructs raw `Term` instances. Useful for test data and simple term construction.

```python
import hydra.overlay.python.dsl.terms as Terms

person = Terms.record(Name("Person"), [
    Terms.field("name", Terms.string("Alice")),
    Terms.field("age", Terms.int32(30))])
```

### 3. Phantom-typed DSL

**Module**: `hydra.overlay.python.dsl.meta.phantoms`

Wraps raw `Term` construction with `TypedTerm[A]` phantom types for type tracking.

```python
# Recommended idiom: star-import for clean call sites
from hydra.overlay.python.dsl.meta.phantoms import *  # noqa: F401,F403

greeting = string("hello")
age = int32(42)
identity = lam("x", var("x"))
```

Phantom-typed functions like `cases`, `match`, `inject`, `wrap`, `field`,
`project`, etc. accept either a `str` or a `Name` for the type-name argument;
`str` is auto-coerced. Drop the redundant `Name(...)` wrapper for terseness:

```python
# Both forms are equivalent — prefer the shorter str form.
cases("hydra.core.Term", arg, ..., [field("lambda", ...)])
cases(Name("hydra.core.Term"), arg, ..., [field(Name("lambda"), ...)])
```

The `@` operator and the call operator are overloaded on `TypedTerm`, so function
application reads naturally:

```python
# All three forms are equivalent:
apply(apply(apply(f, a), b), c)   # spelled-out
f @ a @ b @ c                     # Haskell-style @@ operator
f(a, b, c)                        # Python-native call syntax (preferred)
```

### 4. Domain-specific DSLs

**Modules**: `hydra.dsl.core`, `hydra.dsl.graph`, etc. (generated, one per kernel type module).

Provide typed field accessors, constructors, and `with_*` updaters for Hydra kernel types. These are
**generated** (in the `hydra.dsl.*` namespace), so you import them directly:

```python
import hydra.dsl.core as Core

# Field accessors (project + apply, typed)
body = Core.lambda_body(var("myLambda"))         # Lambda.body
param = Core.lambda_parameter(var("myLambda"))   # Lambda.parameter

# Constructor
lam = Core.lambda_(param_term, domain_term, body_term)
```

(There is also a small *hand-written* `hydra.overlay.python.dsl.meta.core` with a few literal-type
helpers — distinct from the generated `hydra.dsl.core` above.)

### 5. Library wrappers

**Modules**: `hydra.dsl.lib.sets`, `hydra.dsl.lib.lists`, etc.

Typed wrappers around Hydra primitive functions.

```python
import hydra.dsl.lib.sets as Sets
import hydra.dsl.lib.lists as Lists

# Set operations
empty = Sets.empty()
union = Sets.union(set_a, set_b)
from_list = Sets.from_list(my_list)

# List operations
folded = Lists.foldl(fn, init, my_list)
```

### 6. Term references

**Modules**: `hydra.dsl.strip`, `hydra.dsl.serialization`, `hydra.dsl.names`, etc.

One typed, rename-safe reference function per kernel term definition (#467), derived from the
definition's inferred signature.
These replace stringly-typed `var("hydra....")` references (and the coder-private `_kernel_refs.py`
registry), which no rename catches and which fail only at inference time.

```python
import hydra.dsl.strip as Strip

stripped = Strip.deannotate_type(typ)   # references hydra.strip.deannotateType, rename-safe
```

One module is generated per curated term module — the demand set covers the modules the coder
sources reference (`strip`, `serialization`, `annotations`, `names`, `formatting`, `constants`, and
more; curated via the kernel Manifest's `dslTermModules`).
Prefer these over inline `var("hydra....")` strings in new code.

## When to use each variant

| Scenario | Recommended DSLs | Why |
|----------|----------------|-----|
| Defining Hydra types | Direct Types DSL | Constructs `Type` instances for type modules |
| Simple term construction | Direct Terms DSL | Quick and straightforward |
| Writing kernel source code | Phantom-typed + Domain-specific | Type tracking + domain accessors |
| Field access on kernel types | Domain-specific DSLs | `Core.lambda_body(t)` vs manual projection |
| Primitive function calls | Library wrappers | `Sets.union(a, b)` vs raw `primitive2(...)` |
| Referencing kernel functions | Term references | `Strip.deannotate_type(t)` vs `var("hydra.strip.deannotateType")` |

**Rule of thumb**:
- **Type modules** (defining data types): Use `hydra.overlay.python.dsl.types as T` with `T.record()`, `T.union()`, `T.wrap()`
- **Term modules** (defining functions): Use `hydra.overlay.python.dsl.meta.phantoms as P` with domain DSLs
- **Quick prototyping**: Use `hydra.overlay.python.dsl.terms` directly

## Direct DSLs (Types and Terms)

### Constructing Types

```python
import hydra.overlay.python.dsl.types as T
from hydra.core import Name

# Literal types
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
import hydra.overlay.python.dsl.terms as Terms
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
It wraps raw `Term` values in `TypedTerm[A]` to provide type tracking.

### Import pattern

```python
# Recommended idiom: star-import phantoms so DSL primitives are unqualified.
from hydra.overlay.python.dsl.meta.phantoms import *  # noqa: F401,F403
import hydra.dsl.core as Core
from hydra.core import Name
from hydra.overlay.python.dsl.python import Given, None_
```

The star import brings `var`, `lam`, `apply`, `lets`, `let_chain`, `field`,
`cases`, `match`, `inject`, `wrap`, `record`, `string`, `int32`, `nothing`,
`just`, `left`, `right`, `list_`, `pair`, etc. into the file's namespace.
Phantoms functions that take a `Name` (cases, match, inject, wrap, record,
project, field, etc.) also accept `str`, so most call sites can drop the
`Name(...)` boilerplate.

### Literals

```python
greeting = string("hello")
age = int32(42)
flag = boolean(True)
yes = true
no = false
```

### Functions

```python
# Lambda (single parameter)
id_fn = lam("x", var("x"))

# Lambda (multiple parameters — curried)
add = lambdas(["x", "y"], var("x"))  # body uses primitives or var refs

# Function application — three equivalent forms:
result_a = apply(var("f"), int32(5))   # spelled-out
result_b = var("f") @ int32(5)         # @ operator (Haskell-style @@)
result_c = var("f")(int32(5))          # native Python call syntax (preferred)

# Composition
composed = compose(var("g"), var("f"))

# Constant function
always_true = constant(true)

# Identity
id_fn2 = identity()
```

### Data structures

```python
# Lists
nums = list_([int32(1), int32(2), int32(3)])

# Pairs
kv = pair(string("key"), int32(42))

# Optional values
some = just(int32(42))
none = nothing()

# Either
ok = right(int32(42))
err = left(string("error"))
```

### Records

```python
# Construct a record — str type names are auto-coerced to Name.
person = record("my.module.Person", [
    field("name", string("Alice")),
    field("age", int32(30)),
])
```

### Union injection

```python
# Inject into a union type
circle = inject("my.module.Shape", "circle", float64(3.14))

# Unit injection (for enum-like variants)
f32 = inject_unit("hydra.core.FloatType", "float32")
```

### Pattern matching (`cases`/`match`)

```python
# match creates a case elimination (unapplied)
matcher = match("hydra.core.Term",
    Given(var("default")),                 # default case
    [field("literal",
        lam("lit", string("found a literal"))),
     field("variable",
        lam("v", string("found a variable")))])

# cases applies the match to an argument (str type name auto-coerced)
result = cases("hydra.core.Term", var("myTerm"),
    None_(),                            # no default
    [field("literal",
        lam("lit", var("lit"))),
     field(hydra.core.TERM__VARIABLE__NAME,
        lam("v", var("v")))])
```

**Note**: In Python, case fields are passed as a `list`, not as varargs.

### Let bindings

```python
# Single let binding
expr = let1("x", int32(5),
    apply(var("add"), var("x")))

# Multiple let bindings
expr2 = lets([
    field(Name("x"), int32(5)),
    field(Name("y"), int32(10))],
    apply(apply(var("add"), var("x")), var("y")))
```

### Projection (field access)

```python
# Create a field accessor function
get_name = project(hydra.core.PERSON__NAME, hydra.core.PERSON__NAME__NAME)

# Apply it
name = apply(get_name, var("person"))
```

In `hydra.sources.python.*` DSL source modules, the combined
"project a field, then apply to a named variable" pattern is expressed
via the shared `proj` helper in `_source_dsl.py`:

```python
from hydra.sources.python._source_dsl import proj as _proj
# Equivalent to: project(Name("hydra.core.Person"), Name("name"))(var("p"))
name = _proj("hydra.core.Person", "name", "p")
```

Source modules with a fixed type module-name prefix typically wrap this
with a thinner local helper (e.g., `_env`, `_pygraph`, `_meta_proj` in
`coder.py`). Prefer the helper form to the long-form `project(...)(var(...))`
in source modules — it's the idiomatic style.

If the field has a thunked type (e.g., `unit -> T`, used to defer
expression evaluation for benchmarking; see `UniversalTestCase.actual`),
the projection alone yields the thunk — *not* its forced value. Force with
an extra application to `unit()`:

```python
# field type is `unit -> string` — force the thunk
value = project(_UNIVERSAL_TEST_CASE, Name("actual"))(var("ucase"))(unit())
```

Missing the trailing `(unit())` causes inference to fail with
`cannot unify string with (unit → string)` for every binding in the
containing module, since the inferencer processes them in a shared context.

### Wrap/unwrap

```python
# Wrap a value (create a newtype instance)
hydra_name = wrap(hydra.core.NAME__NAME, string("myName"))

# Unwrap function
unwrapper = unwrap(hydra.core.NAME__NAME)
```

### Primitive functions

```python
# Reference a primitive
add_prim = primitive(Name("hydra.lib.math.add"))

# Apply primitives with 1, 2, or 3 arguments
length = primitive1(Name("hydra.lib.strings.length"), var("s"))
sum_ = primitive2(Name("hydra.lib.math.add"), var("x"), var("y"))
```

### Documentation

```python
# Attach documentation to a term
documented = doc("Adds two numbers", var("add"))
```

## Domain-specific DSLs

The domain-specific DSLs (`core`, `graph`) provide typed accessors
for Hydra's kernel types.

### Core DSL (`hydra.dsl.core`, generated)

```python
import hydra.dsl.core as Core

# Field accessors
param = Core.lambda_parameter(var("lam"))         # Lambda.parameter
body = Core.lambda_body(var("lam"))               # Lambda.body
at_body = Core.annotated_term_body(var("at"))     # AnnotatedTerm.body
ann = Core.annotated_term_annotation(var("at"))   # AnnotatedTerm.annotation
tname = Core.injection_type_name(var("inj"))      # Injection.typeName
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

### Sets (`hydra.dsl.lib.sets`)

```python
import hydra.dsl.lib.sets as Sets

empty = Sets.empty()
union = Sets.union(set_a, set_b)
delete = Sets.delete(elem, set_a)
from_list = Sets.from_list(my_list)
to_list = Sets.to_list(my_set)
```

### Lists (`hydra.dsl.lib.lists`)

```python
import hydra.dsl.lib.lists as Lists

folded = Lists.foldl(fn, init, my_list)
mapped = Lists.map_(fn, my_list)
concat = Lists.concat(list_of_lists)
```

### Logic (`hydra.dsl.lib.logic`)

```python
import hydra.dsl.lib.logic as Logic

result = Logic.if_else(condition, then_branch, else_branch)
negated = Logic.not_(condition)
```

### Equality (`hydra.dsl.lib.equality`)

```python
import hydra.dsl.lib.equality as Equality

eq = Equality.equal_name(name_a, name_b)
```

### Optionals (`hydra.dsl.lib.optionals`)

```python
import hydra.dsl.lib.optionals as Optionals

mapped = Optionals.map_(fn, optional_val)
```

## Type definitions

Type-level modules define Hydra data types using the Direct Types DSL.

### Pattern

```python
from hydra.core import Name, Type, TypeScheme
from hydra.overlay.python.dsl.python import Given, None_
from hydra.packaging import DefinitionType, ModuleName, TypeDefinition
import hydra.overlay.python.dsl.types as T

NS = ModuleName("my.namespace")

def _typeref(local: str) -> Type:
    """A TypeVariable reference within this namespace; use for forward/self references."""
    return T.variable(f"{NS.value}.{local}")

def _def(local_name: str, typ: Type) -> DefinitionType:
    """Build a DefinitionType for a named type definition."""
    name = Name(f"{NS.value}.{local_name}")
    ts = TypeScheme((), typ, None_())
    return DefinitionType(TypeDefinition(name, ts))

# Type definitions are collected into the module's `definitions` list; references to
# other definitions use _typeref (no wrapper coercion as on the Haskell side).
definitions = [
    _def("Person", T.record([
        T.field("name", T.string()),
        T.field("age", T.int32()),
        T.field("address", _typeref("Address"))])),
    _def("Address", T.record([
        T.field("street", T.string()),
        T.field("city", T.string())])),
]
```

Each `_def(...)` returns a `DefinitionType` wrapping a `TypeDefinition`, mirroring the Haskell
type-module shape (`moduleDefinitions = DefinitionType <$> definitions`).

### Complete example: hydra.python.syntax

See `packages/hydra-python/src/main/python/hydra/sources/python/syntax.py` for a complete native
Python type module using this pattern (module-local `_def`/`_typeref` helpers and a `definitions` list).

## Term definitions

Term-level modules define Hydra functions using the Phantom-typed DSL.

### Pattern

Definitions use the **fluent builder** — the blessed idiom across all Python coder sources:
`_def("name").doc("...").lam("x").to(body)`. It reads top-to-bottom (name, doc, parameters, then body)
instead of the inside-out `_def("name", doc("...", lambdas(["x"], body)))` nesting. The two forms are
exactly equivalent — `.to(body)` composes `doc(desc, lambdas([params], body))`, omitting the `doc`/
`lambdas` wrappers when none are given. `_def` (from `make_def(_PLACEHOLDER)`) accepts both the fluent
`_def(name)` and the flat `_def(name, term)` form (the latter kept for parameterized helpers). Unlike
Java, `.to(body)` is eager — Python builds each definition when its enclosing function runs.

```python
import hydra.core
import hydra.packaging
import hydra.dsl.core as Core
from hydra.overlay.python.dsl.python import Given, None_
from hydra.typed import TypedBinding

ns = hydra.packaging.ModuleName("my.namespace")
_def = make_def(Module(ns, None_(), [], []))   # or the module placeholder for this source

# Qualified self-reference helper
def _self(lname: str):
    return var("my.namespace." + lname)

# Simple function
def _deannotate_term():
    return (_def("deannotateTerm")
        .doc("Remove annotations from a term")
        .lam("term")
        .to(
            cases(hydra.core.TERM__NAME, var("term"),
                Given(var("term")),
                [field(hydra.core.TERM__ANNOTATED__NAME,
                    lam("at",
                        apply(_self("deannotateTerm"),
                            Core.annotated_term_body(var("at")))))])))
```

### Self-references in Python

**Important**: Python module-level variables cannot reference themselves during construction
(unlike Haskell's lazy bindings or Java's interface fields). Use a qualified variable
reference via `var("namespace.functionName")` pattern:

```python
# WRONG - Python error: name 'deannotate_term' not referenced yet
deannotate_term = define("deannotateTerm",
    lam("term",
        apply(ref(deannotate_term), ...)))  # NameError!

# RIGHT - use qualified variable reference
def _self(lname: str):
    return var("my.namespace." + lname)

deannotate_term = define("deannotateTerm",
    lam("term",
        apply(_self("deannotateTerm"), ...)))  # Works!
```

### Complete examples

The native Python coder package is authored with this DSL. See
`packages/hydra-python/src/main/python/hydra/sources/python/coder.py` and `utils.py` for complete
term modules demonstrating pattern matching, multiple case branches, composition with projection,
let-bindings, recursive rewriting, and qualified self-references.

## Common patterns

### Pattern 1: Simple case dispatch

Match on a union type, handle one variant, pass others through:

```python
fn = lam("term",
    cases(hydra.core.TERM__NAME, var("term"),
        Given(var("term")),                        # default: identity
        [field(hydra.core.TERM__ANNOTATED__NAME,  # handle one case
            lam("at",
                Core.annotated_term_body(var("at"))))]))
```

### Pattern 2: Let-binding with rewrite

Bind a local transform, pass it to a rewriting function:

```python
fn = lam("typ",
    let1("f",
        lam("recurse", lam("t",
            cases(hydra.core.TYPE__NAME, var("t"),
                Given(apply(var("recurse"), var("t"))),
                [field(hydra.core.TYPE__ANNOTATED__NAME,
                    lam("at",
                        apply(var("recurse"),
                            Core.annotated_type_body(var("at")))))]))),
        apply(apply(_self("rewriteType"), var("f")), var("typ"))))
```

### Pattern 3: Fold with set operations

```python
import hydra.dsl.lib.sets as Sets
import hydra.dsl.lib.lists as Lists

vars = let1("dfltVars",
    Lists.foldl(
        lam("s", lam("t",
            Sets.union(var("s"),
                apply(_self("freeVariablesInTerm"), var("t"))))),
        Sets.empty(),
        apply(_self("subterms"), var("term"))),
    # then match on specific cases...
    cases(hydra.core.TERM__NAME, var("term"),
        Given(var("dfltVars")),
        [...]))
```

### Pattern 4: Binding-aware rewriting

```python
replace_fn = lam("recurse", lam("t",
    cases(hydra.core.TERM__NAME, var("t"),
        Given(apply(var("recurse"), var("t"))),
        [field(hydra.core.TERM__FUNCTION__NAME,
            match(hydra.core.FUNCTION__NAME,
                Given(apply(var("recurse"), var("t"))),
                [field(hydra.core.FUNCTION__LAMBDA__NAME,
                    lam("l",
                        Logic.if_else(
                            Equality.equal_name(
                                Core.lambda_parameter(var("l")),
                                var("name")),
                            var("t"),
                            apply(var("recurse"), var("t")))))]))])))
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

### Constructing record values: builders and copy-update

Every generated record dataclass carries two Python-native affordances for construction, so
applications do not have to call the all-args constructor directly or re-implement builder boilerplate.
Both mirror the Java counterparts (`#465`) while reading natively in Python.

**Copy-update methods.**
Each record has one `with_<field>(...)` method per field, returning a new instance with that one field
replaced and all others copied. Since every generated record is a `@dataclass(frozen=True)`, these wrap
`dataclasses.replace`:

```python
from hydra.core import Binding, Name

b2 = b.with_name(Name("y"))   # same term + type_scheme, new name
```

Field names are snake-cased, so `typeScheme` becomes `with_type_scheme`.

**Fluent builder.**
Each record also exposes a static `builder()` factory and a nested `Builder` class with one setter per
field (named after the field) and a `build()` that returns the immutable record:

```python
from hydra.core import Lambda, Name

lam = (Lambda.builder()
       .parameter(Name("x"))
       .body(my_term)
       .build())          # domain omitted -> None
```

The `Builder` is itself a frozen dataclass, so each setter returns a new builder (the chain is
immutable and reusable). Generic records reuse the record's `Generic[...]` parameters on the builder.

Notes:
- `with_*` methods and the builder are emitted for **every** generated record (no opt-in flag).
- The builder stores in-progress state in underscore-prefixed fields (`_parameter`, `_body`, …) so the
  storage does not collide with the same-named setter methods; these are internal, not user-facing.
- Builder fields default to `None`, so a partially-populated builder is allowed; `build()` does no
  null-check (Hydra-generated code never passes nulls, and the keyword constructor remains available).
- A field literally named `build` or `builder` would collide with the generated methods and is escaped
  to `build_` / `builder_` (matching the Java convention).

## Error handling

Hydra computations use `Either[Error, A]` for error handling (the former Flow monad
was removed in #245). An `InferenceContext` value is threaded alongside the graph
and carries the fresh-type-variable counter and the current subterm-path trace.

```python
from hydra.util import Either
from hydra.typing import InferenceContext
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
from hydra.overlay.python.dsl.python import FrozenDict

d = FrozenDict({"key1": "value1", "key2": "value2"})
value = d["key1"]
d2 = FrozenDict({**d, "key3": "value3"})
```

## Examples in the codebase

(The Python `hydra/dsl/` runtime was relocated to `overlay/python/hydra-kernel/` by #418.)

| File | Description |
|------|-------------|
| `overlay/python/hydra-kernel/src/main/python/hydra/dsl/meta/phantoms.py` | Phantom-typed DSL (all operations) |
| `overlay/python/hydra-kernel/src/main/python/hydra/dsl/meta/core.py` | Core domain DSL (field accessors) |
| `overlay/python/hydra-kernel/src/main/python/hydra/dsl/meta/graph.py` | Graph domain DSL |
| `overlay/python/hydra-kernel/src/main/python/hydra/dsl/meta/lib/sets.py` | Sets library wrapper |
| `overlay/python/hydra-kernel/src/main/python/hydra/dsl/meta/lib/lists.py` | Lists library wrapper |
| `overlay/python/hydra-kernel/src/main/python/hydra/dsl/types.py` | Direct Types DSL |
| `overlay/python/hydra-kernel/src/main/python/hydra/dsl/terms.py` | Direct Terms DSL |

## Related Documentation

- [DSL Guide (Haskell)](dsl-guide.md) - Comprehensive DSL guide for kernel development
- [DSL Guide (Java)](dsl-guide-java.md) - Java DSL guide
- [DSL Guide (Scala)](dsl-guide-scala.md) - Scala DSL guide
- [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) - Core Hydra concepts
- [Implementation](implementation.md) - Implementation details and architecture
- [Hydra-Python README](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-python) - Getting started
