# Tail-call optimization in Hydra

> **Audience:** Hydra contributors working on the Python or Java coders, or
> interested in how Hydra avoids stack overflow in generated code.

## Overview

Hydra is a typed, functional meta-programming language whose core data model
is defined in `hydra.core.Term` — a System-F-based term algebra with
lambdas, applications, let-bindings, case statements, and primitive
references. The Hydra compiler generates code for multiple target
languages including Haskell, Python, and Java.

Because Hydra's core language is purely functional and relies heavily on
recursion, the generated code must cope with the fact that most target
languages (Python, Java, JavaScript, …) do *not* guarantee tail-call
optimization at the runtime level. Unbounded recursive calls in generated
code would exhaust the call stack.

Hydra solves this at *code-generation time*: the Python and Java coders
detect self-tail-recursive functions in the Hydra term graph and emit
iterative `while` loops in the target language, replacing tail calls with
parameter reassignment and `continue`.

## The Hydra core term model

The term algebra is defined in
`packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Types/Core.hs`.
The variants relevant to TCO are:

```haskell
term = define "Term" $ T.union [
    "application"    -- function application  (f x)
    "function"       -- lambda, elimination, or primitive
    "let"            -- let-bindings with body
    "variable"       -- variable reference
    -- ...other variants elided...
  ]

function = define "Function" $ T.union [
    "elimination"    -- case statement, projection, unwrap
    "lambda"         -- λ x . body
    "primitive"      -- built-in function reference
  ]

elimination = define "Elimination" $ T.union [
    "record"         -- field projection
    "union"          -- case statement (DSL: `match`)
    "wrap"           -- newtype unwrap
  ]
```

There is **no** explicit `TailCall` or `Loop` variant in the core term
type. TCO is a *code-generation concern*, not a language-level construct.

## Detection: `isSelfTailRecursive`

Detection lives in the kernel `Analysis` module
(`packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Analysis.hs`),
exposed as `hydra.analysis.isSelfTailRecursive` and
`hydra.analysis.isTailRecursiveInTailPosition`. Both the Python and Java
coders call into the kernel function rather than duplicating the algorithm.

### Algorithm

```
isSelfTailRecursive(funcName, body) -> Bool

1. Check if funcName appears as a free variable in body.
   (Uses Rewriting.isFreeVariableInTerm — returns True when NOT free,
    so Logic.not means "IS present".)
2. If funcName is NOT present: return False (not recursive at all).
3. If funcName IS present: delegate to isTailRecursiveInTailPosition.
```

### Tail-position analysis

`isTailRecursiveInTailPosition(funcName, term)` walks the term structure
and verifies that *every* occurrence of `funcName` is in a valid tail
position. The analysis handles:

- **Application (`Term.application`).** Gathers the application chain
  `f arg1 arg2 …` and checks:
  - If `f` is `Variable(funcName)` — a self-call — the arguments must:
    - Not contain `funcName` (no nested self-calls in arguments).
    - Not contain lambda terms (closures over parameters would break TCO
      because Python closures capture by reference, not by value).
  - If `f` is a union elimination (case statement), each branch is checked
    recursively for tail position, and the arguments to the case statement
    must not contain `funcName`.
- **Lambda (`Term.function.lambda`).** The tail position of a lambda is
  its body. Recurse into the body.
- **Let (`Term.let`).** The bindings must *not* contain `funcName`; the
  tail position is the `let` body.
- **Default.** Any other term variant: `funcName` must not appear free
  (i.e., the term is a non-recursive base case).

## Transformation: recursion to iteration

When `isSelfTailRecursive` returns `True`, the coder wraps the function
body in a `while` loop and replaces tail calls with parameter reassignment
followed by `continue`.

### Python transformation

The Python coder
(`packages/hydra-python/src/main/python/hydra/sources/python/coder.py`)
implements this in two functions:

- **`encodeFunctionDefinition`** — after detecting TCO applicability:

  ```python
  "isTCO" <~ (Logic.and
      (Logic.not $ Lists.null (var "args"))
      (isSelfTailRecursive @@ var "name" @@ var "body"))
  ```

  When `isTCO` is `True`, the body is wrapped in `while True:` and encoded
  via `encodeTermMultilineTCO`.

- **`encodeTermMultilineTCO`** — walks the term body and handles two
  cases:

  1. **Direct self-call:** `funcName(arg1, arg2, …)` is replaced with:

     ```python
     param1 = <encoded arg1>
     param2 = <encoded arg2>
     continue
     ```

  2. **Case statement in tail position:** a `match` statement where
     individual branches may contain tail calls. Each branch is recursively
     encoded via `encodeTermMultilineTCO`, so branches with self-calls get
     `continue` and branches without get `return`.

#### Generated Python example

A Hydra function like `deannotateAndDetypeTerm` that pattern-matches and
recurses on sub-terms generates:

```python
def deannotate_and_detype_term(t: hydra.core.Term) -> hydra.core.Term:
    r"""Strip type annotations from the top levels of a term."""

    while True:
        match t:
            case hydra.core.TermAnnotated(value=at):
                t = at.body
                continue

            case hydra.core.TermTypeApplication(value=tt):
                t = tt.body
                continue

            case hydra.core.TermTypeLambda(value=ta):
                t = ta.body
                continue

            case _:
                return t
```

Other examples from generated output in
`dist/python/hydra-kernel/src/main/python/hydra/`:

- `rewriting.py`: `deannotate_term`, `deannotate_type`,
  `deannotate_type_parameters`, `is_lambda`, `unshadow_variables`
  (inner helper).
- `reduction.py`, `schemas.py`, `annotations.py`, `checking.py`,
  `lexical.py`, `grammars.py`, `decoding.py`, `coder_utils.py`,
  `show/core.py`.

### Java transformation

The Java coder
(`packages/hydra-java/src/main/java/hydra/sources/java/Coder.java`)
implements the same pattern with Java-specific syntax.

- **`encodeTermDefinition`** — after detecting TCO:

  ```java
  "isTCO" <~ (Logic.and
      (Logic.not $ Lists.null (var "params"))
      (isSelfTailRecursive @@ var "name" @@ var "body"))
  ```

  When `isTCO` is `True`, the body is wrapped in `while(true) { … }`
  (represented as `WhileStatement` with `cond = Nothing`) and encoded via
  `encodeTermTCO`.

- **`encodeTermTCO`** — structurally identical to the Python version, but
  generates Java AST nodes:

  1. **Direct self-call:** emits assignment statements and a `continue`
     statement (as a `ContinueStatement` Java AST node).
  2. **Case statement:** generates `if (arg instanceof VariantType) { … }`
     chains instead of Python `match` statements, since Java uses the
     visitor pattern for union types. Each branch that is a tail call gets
     assignment + continue; non-tail branches get a normal `return`.

The Java TCO code generates `while` with `cond = Nothing`, serialized as
`while (true)` by the Java serde module.

#### Java TCO pseudocode

A tail-recursive function `foo(x, acc)` would generate:

```java
static ReturnType foo(ParamType x, AccType acc) {
    while (true) {
        // let-binding prefixes re-evaluated each iteration
        LocalType local = computeLocal(x, acc);

        if (x instanceof BaseCase) {
            return acc;
        }
        if (x instanceof RecursiveCase rc) {
            RecursiveCase v = (RecursiveCase) x;
            // Tail call: reassign + continue
            x = v.next;
            acc = combine(acc, v.value);
            continue;
        }
        return acc; // default
    }
}
```

## Design details

### Why prefixes go inside the loop

Both coders place let-binding prefix statements *inside* the `while` loop
body. This is essential because let-bindings in the original Hydra term
may depend on the function parameters. After reassignment (the TCO
transformation of a tail call), the bindings must be recomputed with the
new parameter values.

### Lambda-in-argument guard

The tail-position analysis rejects functions where arguments to tail calls
contain lambda expressions. Python closures capture variables by
*reference*: if a lambda in an argument closes over a parameter that gets
reassigned by the while-loop transformation, the closure would observe
the mutated value rather than the value at the time of the call. That
would change semantics, so the guard disqualifies these cases from TCO.

### Supported patterns

| Pattern | Treatment |
|---------|-----------|
| Direct self-tail-call | Reassign parameters, `continue` |
| Case statement with tail calls in branches | Python: `match` with `continue` in recursive branches, `return` in base cases. Java: `if/instanceof` chain with the same logic. |
| Let-bindings before tail call | Bindings placed inside loop body |
| Nested lambdas wrapping tail calls | Transparent: lambda body is the tail position |

### Unsupported patterns (fall back to recursion)

- Mutual recursion (only *self*-recursion is detected).
- Continuation-passing style where the continuation is not statically
  resolved.
- Tail calls where arguments contain lambdas (closure-capture issue).
- Tail calls where arguments contain nested self-references.

## Comparison with general recursion elimination

The blog post
[Recursion Elimination by grgz](https://blog.grgz.me/posts/recursion_elimination.html)
describes a more general four-stage transformation:

| Stage | Description | Hydra's approach |
|-------|-------------|------------------|
| 1. CPS conversion | Make continuations explicit by passing a `cont` parameter | **Not needed.** Hydra only handles self-tail-recursion where no continuation is required — the tail call *is* the return. |
| 2. Defunctionalization | Replace closure-valued parameters with data structures + `apply()` dispatch | **Not needed.** Since only self-tail-calls are transformed, there are no higher-order continuation values to defunctionalize. |
| 3. Inlining | Inline the `apply()` function to expose optimization opportunities | **Not applicable.** The transformation is direct: tail call becomes assignment + `continue`. |
| 4. TCO | Convert tail calls to loops | **This is what Hydra implements.** The while-loop + `continue` pattern is exactly stage 4 of the general pipeline. |

Hydra's approach is a *specialization* of the general technique. By
restricting itself to self-tail-recursion (the most common pattern in
functional code), Hydra avoids the complexity of CPS conversion and
defunctionalization while still eliminating stack overflow for the vast
majority of recursive functions in the generated code.

The blog post's benchmarks also reveal an important insight: iterative
transformations do not universally improve performance. For functions
called very frequently (`dup` in the blog's example), heap allocation for
explicit stack structures can outweigh the benefit. Hydra's approach
sidesteps this by not allocating any auxiliary data structures — it
reuses the existing local variables via reassignment.

## Key source files

| Component | Path |
|-----------|------|
| Core term types | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Types/Core.hs` |
| Detection (`isSelfTailRecursive`, `isTailRecursiveInTailPosition`) | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Analysis.hs` |
| Term reduction (beta, eta) | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Reduction.hs` |
| Term rewriting (free vars, annotation stripping) | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Rewriting.hs` |
| Let-binding hoisting | `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Hoisting.hs` |
| Python coder | `packages/hydra-python/src/main/python/hydra/sources/python/coder.py` (`encodeTermMultilineTCO`) |
| Java coder | `packages/hydra-java/src/main/java/hydra/sources/java/Coder.java` (`encodeTermTCO`) |
| Generated Python (shows TCO output) | `dist/python/hydra-kernel/src/main/python/hydra/rewriting.py` |

## Summary

Hydra implements tail-call optimization as a code-generation
transformation, not a language-level feature. The kernel `Analysis`
module exposes a common detection algorithm (`isSelfTailRecursive` /
`isTailRecursiveInTailPosition`) that validates all self-references are
in tail position; the Python and Java coders call into it and emit
iterative `while` loops with parameter reassignment and `continue` in
place of recursive calls. This approach is a specialization of general
recursion elimination (CPS + defunctionalization + TCO), achieving the
practical benefit — stack safety — without the complexity of the full
pipeline.
