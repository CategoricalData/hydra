# Adding new primitives to Hydra

A step-by-step guide for adding new primitive functions and constants to Hydra's standard library across all implementations.

## Prerequisites

- Familiarity with Hydra's type system (see [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts))
- Understanding of the target language implementations (Haskell, Java, Python)
- Knowledge of which library the primitive belongs to (e.g. `hydra.lib.strings`, `hydra.lib.math`)

**Important:** Every new primitive must include test cases in the common test suite. Tests ensure consistent behavior across all language implementations and catch regressions. Adding a primitive without tests is incomplete work.

## Overview

Primitive functions are native implementations of operations which may not be expressible in Hydra's term language. They bridge Hydra to the host language's capabilities. Each primitive must be:

1. Implemented natively in each language (Haskell, Java, Python)
2. Registered in each language's primitive registry
3. Exposed through a typed DSL wrapper

Most primitives are functions (*primitive functions*), but there are also *primitive constants* like `hydra.lib.math.pi`
and `hydra.lib.sets.empty`.

**Important:** Hydra-Haskell is the source of truth for Hydra's standard library.
If a primitive is defined in Haskell, it must be implemented in each language variant (Java, Python, etc.).
Most of Hydra's primitives are, in turn, inherited from the Haskell language itself;
they are based on functions from Haskell's Prelude and base package.
Before adding a new primitive, check whether an appropriate function exists in Haskell already,
and adopt its name and semantics if possible.
This ensures consistency and leverages well-understood behavior.
It also helps generate more error-free code, as typical large language models have been trained on large amounts of Haskell code.

### Naming conventions

Primitive functions should conform to the case convention of the implementation language:
- **Haskell**: camelCase (e.g. `isAlphaNum`, `toLower`)
- **Java**: PascalCase for class names (e.g. `IsAlphaNum`, `ToLower`)
- **Python**: snake_case (e.g. `is_alpha_num`, `to_lower`)

Note that every primitive has a native Hydra name which is always in camelCase (e.g. `hydra.lib.chars.isAlphaNum`), regardless of the implementation language's convention.

### Implementation guidelines

Keep primitive implementations simple - they should typically delegate to the host language's standard library functions. The implementation should be straightforward and readable, as it bridges Hydra's abstract operations to the concrete capabilities of each language.

### Library DSLs

Each primitive must be exposed through a typed DSL wrapper. In Haskell, these use phantom types to provide compile-time type safety when constructing Hydra terms. Python also requires DSL wrappers for type-safe term construction.

## Adding a primitive to Haskell

### 1. Implement the function

Add the implementation in `/hydra-haskell/src/main/haskell/Hydra/Lib/<Library>.hs`:

```haskell
-- In Hydra/Lib/Chars.hs
module Hydra.Lib.Chars where

import qualified Data.Char as C

isAlphaNum :: Int -> Bool
isAlphaNum = C.isAlphaNum . C.chr

toLower :: Int -> Int
toLower = C.ord . C.toLower . C.chr
```

**Guidelines:**
- Include type signatures for all functions

### 2. Register the primitive

Update `/hydra-haskell/src/main/haskell/Hydra/Sources/Libraries.hs`:

```haskell
-- Add import at the top
import qualified Hydra.Lib.Chars as Chars

-- Define name constants
_chars_isAlphaNum = qname _hydra_lib_chars "isAlphaNum" :: Name
_chars_toLower    = qname _hydra_lib_chars "toLower" :: Name

-- Add to library's primitive list
hydraLibChars :: Library
hydraLibChars = standardLibrary _hydra_lib_chars [
  prim1 _chars_isAlphaNum Chars.isAlphaNum [] int32 boolean,
  prim1 _chars_toLower Chars.toLower [] int32 int32]
```

**Key points:**
- Use `prim1` for single-argument functions, `prim2` for two arguments, etc.
- The third parameter (`[]`) is for type parameters - leave empty for monomorphic functions
- Specify input and output types using DSL type constructors

### Higher-order primitives and eval elements

Higher-order primitives (functions that take other functions as arguments) require additional "eval elements" to work with Hydra's interpreter. These eval elements define how the primitive operates on unevaluated Hydra terms.

**When are eval elements needed?**
- Any primitive that accepts function arguments (e.g., `map`, `filter`, `bimap`, `either`)
- The eval element applies the function arguments to values and returns unevaluated application terms

**Adding an eval element:**

1. Create or update the Sources module in `/hydra-haskell/src/main/haskell/Hydra/Sources/Eval/Lib/<Library>.hs`:

```haskell
module Hydra.Sources.Eval.Lib.Eithers where

import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
-- ... other imports

ns :: Namespace
ns = Namespace "hydra.eval.lib.eithers"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Monads.module_, ShowCore.module_]
    kernelTypesModules $
    Just "Evaluation-level implementations of Either functions."
  where
    elements = [toBinding bimap_]

-- | Interpreter-friendly bimap for Either terms.
bimap_ :: TBinding (Term -> Term -> Term -> Flow s Term)
bimap_ = define "bimap" $
  doc "Interpreter-friendly bimap for Either terms." $
  "leftFun" ~> "rightFun" ~> "eitherTerm" ~>
  cases _Term (var "eitherTerm")
    (Just (Monads.unexpected @@ string "either value" @@ (ShowCore.term @@ var "eitherTerm"))) [
    _Term_either>>: "e" ~>
      produce $ Eithers.either_
        ("val" ~> Core.termEither $ left $ Core.termApplication $ Core.application (var "leftFun") (var "val"))
        ("val" ~> Core.termEither $ right $ Core.termApplication $ Core.application (var "rightFun") (var "val"))
        (var "e")]
```

2. Add the module to `/hydra-haskell/src/main/haskell/Hydra/Sources/Eval/Lib/All.hs`:

```haskell
import qualified Hydra.Sources.Eval.Lib.Eithers as EvalEithers

evalLibModules :: [Module]
evalLibModules = [
  EvalEithers.module_,
  -- ... other modules
  ]
```

3. Generate the Haskell runtime code. The generated module goes in `/hydra-haskell/src/gen-main/haskell/Hydra/Eval/Lib/<Library>.hs`.

   **Note:** The generated eval module is a bootstrap file. If you add a new eval element, you may need to manually add the function to the generated file initially, then regenerate. The export list in the generated file must include your new function for the `Libraries.hs` import to work.

4. Update the primitive registration in `Libraries.hs` to use `prim3Eval` instead of `prim3`:

```haskell
import qualified Hydra.Eval.Lib.Eithers as EvalEithers

hydraLibEithers :: Library
hydraLibEithers = standardLibrary _hydra_lib_eithers [
    prim3Eval _eithers_bimap EvalEithers.bimap ["x", "y", "z", "w"] ...,
    -- Use prim3Eval for higher-order, prim3 for first-order
    ]
```

**Key differences between `primN` and `primNEval`:**
- `prim3` uses the native Haskell implementation directly
- `prim3Eval` uses the eval element, which returns unevaluated application terms suitable for the interpreter

### 3. Create DSL wrapper

Add typed wrapper in `/hydra-haskell/src/main/haskell/Hydra/Dsl/Lib/<Library>.hs`:

```haskell
-- In Hydra/Dsl/Lib/Chars.hs
module Hydra.Dsl.Meta.Lib.Chars where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries

isAlphaNum :: TTerm Int -> TTerm Bool
isAlphaNum = primitive1 _chars_isAlphaNum

toLower :: TTerm Int -> TTerm Int
toLower = primitive1 _chars_toLower
```

**Guidelines:**
- Use phantom types (`TTerm`) to ensure type safety
- Use `primitive1` for unary, `primitive2` for binary functions
- Reference the name constants defined in `Libraries.hs`

## Adding a primitive to Java

### 1. Implement the PrimitiveFunction class

Create `/hydra-java/src/main/java/hydra/lib/<library>/<FunctionName>.java`:

```java
package hydra.lib.chars;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;

public class IsAlphaNum extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.chars.isAlphaNum");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), boolean_()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(
                Expect.int32(args.get(0)),
                c -> Terms.boolean_(apply(c)));
    }

    public static boolean apply(int codePoint) {
        return Character.isLetterOrDigit(codePoint);
    }
}
```

**Structure:**
- `name()`: Returns the fully qualified Hydra name
- `type()`: Declares the type scheme (use type parameters for polymorphic functions)
- `implementation()`: Flow-based wrapper that extracts arguments and wraps results
- `apply()`: Static method(s) for direct Java usage

### 2. Register in Libraries

Update `/hydra-java/src/main/java/hydra/lib/Libraries.java`:

```java
// Add import
import hydra.lib.chars.IsAlphaNum;
import hydra.lib.chars.ToLower;

// In standardPrimitives(), ensure library is included
public static List<PrimitiveFunction> standardPrimitives() {
    List<PrimitiveFunction> prims = new ArrayList<>();
    prims.addAll(charsPrimitives());  // Add if new library
    // ... other libraries
    return prims;
}

// Create or update library's primitive list
private static List<PrimitiveFunction> charsPrimitives() {
    return Arrays.asList(
            new IsAlphaNum(),
            new ToLower());
}
```

**Important:** Before adding a new primitive, verify that:
1. The library's primitives method exists (e.g., `charsPrimitives()`)
2. The library is included in `standardPrimitives()`

If the library's primitives method doesn't exist, you'll need to create it and add it to `standardPrimitives()`.


## Adding a primitive to Python

### 1. Implement the function

Add to `/hydra-python/src/main/python/hydra/lib/<library>.py`:

```python
"""Python implementations of hydra.lib.chars primitives."""

def is_alpha_num(value: int) -> bool:
    """Check if a character (as int) is alphanumeric."""
    return chr(value).isalnum()

def to_lower(value: int) -> int:
    """Convert a character (as int) to lowercase, returning the int value."""
    return ord(chr(value).lower())
```

**Guidelines:**
- Include type hints for all functions
- Add docstrings to document behavior

### 2. Register the primitive

Update `/hydra-python/src/main/python/hydra/sources/libraries.py`:

```python
def register_chars_primitives() -> dict[Name, Primitive]:
    """Register all chars primitive functions."""
    from hydra.lib import chars

    primitives: dict[Name, Primitive] = {}
    # Add primitive registrations here
    # Note: This registry pattern is still being finalized
    return primitives

def standard_library() -> dict[Name, Primitive]:
    """Get all standard library primitives."""
    primitives: dict[Name, Primitive] = {}
    primitives.update(register_chars_primitives())
    # ... other libraries
    return primitives
```

### Higher-order primitives in Python

For higher-order primitives (those that take function arguments), you need:

1. An implementation in `hydra/lib/<library>.py` for direct Python use
2. An eval-level implementation in `hydra/eval/lib/<library>.py` that works with Hydra terms
3. Registration using `prim2_interp` or `prim3_interp` instead of `prim2` or `prim3`

Example registration for a higher-order primitive:

```python
from hydra.eval.lib import eithers as eval_eithers

# map :: (x -> y) -> Either z x -> Either z y
primitives[qname(namespace, "map")] = prims.prim2_interp(
    qname(namespace, "map"), Just(eval_eithers.map), ["x", "y", "z"],
    prims.function(x, y), prims.either(z, x), prims.either(z, y)
)
```

### 3. Create DSL wrapper

DSL wrappers for Python are still being developed. The pattern and implementation details will be finalized as the Python DSL matures.

## Testing

After adding primitives to all three languages:

### Haskell
```bash
cd hydra-haskell
stack test
```

### Java
```bash
./gradlew :hydra-java:test
```

### Python
```bash
cd hydra-python
pytest
```

### Common test suite

**All primitives must have test cases in the common test suite.** This ensures consistent behavior across all language implementations and prevents regressions. Tests are defined in `/hydra-haskell/src/main/haskell/Hydra/Sources/Test/Lib/<Library>.hs`.

**Adding test cases:**

1. Open or create the test file for your library (e.g., `Test/Lib/Chars.hs`)

2. Add a test group following the existing pattern:
```haskell
charsIsAlphaNum :: TTerm TestGroup
charsIsAlphaNum = subgroup "isAlphaNum" [
  test "lowercase letter" (ord 'a') true,
  test "uppercase letter" (ord 'Z') true,
  test "digit" (ord '5') true,
  test "space" (ord ' ') false]
  where
    test name x result = primCase name _chars_isAlphaNum [int32 x] result
```

3. Add the test group to the `allTests` binding in the same file

4. **Regenerate the test suite for all implementations** (this step is required!):
```bash
# From hydra-haskell - regenerate kernel tests and generation tests
stack exec update-kernel-tests
stack exec update-generation-tests

# From hydra-ext (for Java and Python)
stack exec update-python-generation-tests
```

5. Run tests in each language to verify the new test cases pass

**Test coverage guidelines:**
- Include edge cases: empty collections, single elements, boundary values
- Test both positive and negative cases (e.g., element present vs absent)
- For higher-order primitives, test with different predicate/function behaviors
- Look at similar existing primitives for test patterns to follow

## Checklist

When adding a new primitive function:

- [ ] **Haskell**
  - [ ] Implementation in `Hydra.Lib.<Library>`
  - [ ] Name constant in `Hydra.Staging.Lib.Names` (e.g., `_lists_partition`)
  - [ ] Registration in `hydraLib<Library>` list in `Libraries.hs`
  - [ ] DSL wrapper in `Hydra.Dsl.Meta.Lib.<Library>`
  - [ ] **(If higher-order)** Eval element in `Hydra.Sources.Eval.Lib.<Library>`
  - [ ] **(If higher-order)** Generated runtime in `Hydra.Eval.Lib.<Library>` (may need manual bootstrap)
  - [ ] **(If higher-order)** Use `primNEval` instead of `primN` in registration
- [ ] **Java**
  - [ ] PrimitiveFunction class in `hydra.lib.<library>`
  - [ ] Import and registration in `Libraries.java`
- [ ] **Python**
  - [ ] Function implementation in `hydra.lib.<library>`
  - [ ] **(If higher-order)** Eval implementation in `hydra.eval.lib.<library>`
  - [ ] Registration in `hydra.sources.libraries`
- [ ] **Common Test Suite** (required!)
  - [ ] Test group added to `Hydra.Sources.Test.Lib.<Library>`
  - [ ] Test group registered in `allTests`
  - [ ] Kernel and generation tests regenerated (`stack exec update-kernel-tests && stack exec update-generation-tests`)
- [ ] **Tests pass** in all three languages
- [ ] **Documentation** updated if needed

## Common pitfalls

1. **Mismatched names**: Ensure the primitive's Hydra name (always camelCase) is exactly the same across all implementations, even though the implementation follows each language's naming convention

2. **Type mismatches**: The type signature must be identical across languages
   - Use Hydra's type constructors consistently
   - Pay attention to parameter order

3. **Missing imports**: Don't forget to import the implementation module in the registry

4. **Registry order**: List primitives in alphabetical order for consistency

5. **Forgetting test group registration**: After adding test cases to a subgroup (e.g., `listsFind`), remember to also add it to the `allTests` list in the same file. Both steps are required for tests to run.

## Example: Adding a complete primitive

See the `hydra.lib.chars` library (added in version 0.13.0) for a complete example spanning all three languages:
- Haskell: [Hydra/Lib/Chars.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Lib/Chars.hs)
- Java: [hydra/lib/chars/](https://github.com/CategoricalData/hydra/tree/main/hydra-java/src/main/java/hydra/lib/chars)
- Python: [hydra/lib/chars.py](https://github.com/CategoricalData/hydra/blob/main/hydra-python/src/main/python/hydra/lib/chars.py)

## Further reading

- [Testing documentation](https://github.com/CategoricalData/hydra/wiki/Testing) - Understanding the common test suite
- [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) - Hydra's type system and data structures
- [Creating a new Hydra implementation](new-implementation.md) - For adding an entirely new language target
