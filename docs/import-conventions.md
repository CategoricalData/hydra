# Import conventions for Hydra Haskell DSL source files

This document specifies the canonical import blocks for every kind of DSL source module
written in **Haskell** in the Hydra codebase. It is the authoritative reference; the
[Coding-style wiki page](https://github.com/CategoricalData/hydra/wiki/Coding-style)
links here for import details.

Hydra also has, or will soon have, DSL source modules written in **Python** and **Java**
(see [docs/dsl-guide-python.md](dsl-guide-python.md) and
[docs/dsl-guide-java.md](dsl-guide-java.md)). Those have their own import/visibility
conventions and are out of scope for this document.

## Overarching rule

**Default to qualified.** Import every module qualified, with a short PascalCase alias.
Unqualify a module only if it is the **central DSL the file is written in** —
the vocabulary of types, term constructors, and operators that dominates the body.
Everything else, including supporting libraries with familiar names like `Strings`,
`Lists`, `Maps`, gets a qualified prefix.

Two consequences:

1. The unqualified set is small and predictable per file kind.
2. A reader scanning a body knows that any bare identifier comes from one of the few
   unqualified modules, while `Foo.bar` references a clearly named module.

## Source module categories

A file is a **DSL source module** if it contains a top-level `module_ :: Module`
definition. Such files are subject to the conventions below. Files without a `Module`
definition (manifests, top-level `*All.hs` aggregators, `Libraries.hs`,
`Kernel/Manifest.hs`) are infrastructure with bespoke imports — see
[Uncategorized files](#uncategorized-files) at the end.

Each category has a canonical comment tag placed above its import block.

| # | Tag                                              | Defines                                     |
|---|--------------------------------------------------|---------------------------------------------|
| 1 | `kernel type modules`                            | Hydra-kernel types                          |
| 2 | `type-level sources outside of the kernel`       | Per-language types (host/target ASTs)       |
| 3 | `kernel terms modules`                           | Hydra-kernel term constants                 |
| 4 | `kernel terms modules with DeepCore`             | Encoders/decoders/extractors                |
| 5 | `term-level sources outside of the kernel`       | Per-language term constants (coders, etc.)  |
| 6 | `tests`                                          | Test groups whose `@@` is binding application |
| 7 | `term-encoded tests`                             | Test groups whose `@@` builds Hydra-runtime term applications (lambdas, primitives) |
| 8 | `kernel test fixtures`                           | Named term/type/binding fixtures for tests  |

## Naming conventions for aliases

- **Trailing PascalCase.** A Hydra-module alias is the trailing component of the dotted
  module path, in PascalCase. `Hydra.Dsl.Meta.Lib.Lists` is `Lists`,
  `Hydra.Sources.Kernel.Terms.Lexical` is `Lexical`. The full kernel-types aggregator
  `Hydra.Sources.Kernel.Types.All` is `KernelTypes` (a deliberate two-word exception
  because `All` would be uninformative).
- **On collision, both grow.** When two imports in the same file would receive the same
  alias, both take a longer disambiguating form. Example: a file that imports both
  `Hydra.Java.Syntax` (runtime) and `Hydra.Sources.Java.Syntax` (DSL source) does NOT
  use bare `Syntax` for either — both grow to `JavaSyntax` (runtime) and
  `JavaSyntaxSource` (DSL source). When only one of the pair is imported, the bare alias
  is preferred.
- **Standard library aliases are single letters**: `Data.List` is `L`, `Data.Map` is `M`,
  `Data.Set` is `S`, `Data.Maybe` is `Y`, `Data.Int` is `I`, `Data.ByteString` is `B`,
  `Data.ByteString.Char8` is `BC`, `Data.Scientific` is `Sci`.
- **`T` is overloaded by category** — see the per-category blocks below. In categories
  1–5 (term-/type-level production), `T = Hydra.Dsl.Types`. In categories 6–8 (tests
  and fixtures), `T = Hydra.Dsl.Meta.Types`. The full alias `MetaTypes = Hydra.Dsl.Meta.Types`
  is used when both are needed in the same file.

## `reify` and `reify2`

A common operation in categories 3, 4, 5, 6, 7 is to convert a Haskell-level
meta-function `TTerm a -> TTerm b` into a first-class term-level function
`TTerm (a -> b)`. Use `reify` (unary) and `reify2` (binary), exported from
`Hydra.Dsl.Meta.Phantoms`:

```haskell
ShowCore.list_ @@ reify Literals.showInt32 @@ xs
ShowCore.map_ @@ reify Literals.showInt32 @@ reify Literals.showString @@ m
binPrim :: TTerm (a -> b -> c)
binPrim = reify2 (\x y -> Maths.add x (Maths.mul x y))
```

---

## Category 1 — Kernel TYPE module

**Tag**: `kernel type modules`

**Definition shape**: `define :: String -> Type -> Binding`

**Centrally important (unqualified)**:
- `Hydra.Kernel`
- `Hydra.Dsl.Annotations` (for `doc`)
- `Hydra.Dsl.Bootstrap`
- `Hydra.Dsl.Types` operators only — `(>:)`, `(@@)`, `(~>)` and helpers like
  `record`, `union`, `enum`
- `Hydra.Sources.Kernel.Types.All`

**Qualified**:
- `T = Hydra.Dsl.Types`
- `M = Data.Map`, `L = Data.List`, `S = Data.Set`, `Y = Data.Maybe`

```haskell
module Hydra.Sources.Kernel.Types.<X> where

-- Standard imports for kernel type modules
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import           Hydra.Sources.Kernel.Types.All

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Maybe as Y
import qualified Data.Set  as S
```

`Hydra.Sources.Kernel.Types.All` is unqualified to expose `kernelTypesModuleNames` and
the constants like `_Term`, `_Field` directly. (Cross-references to other type modules
in the same package are written as e.g. `Core.binding` — `Core` being already in scope
via `Hydra.Sources.Kernel.Types.All`.)

## Category 2 — Type-level source outside the kernel

**Tag**: `type-level sources outside of the kernel`

**Definition shape**: same as category 1.

**Delta from category 1**: `Hydra.Sources.Kernel.Types.All` is **qualified** as
`KernelTypes` (since the file is in a downstream package and references kernel types
via that alias).

```haskell
module Hydra.Sources.<Lang>.<X> where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.All  as KernelTypes
import qualified Hydra.Sources.Kernel.Types.Core as Core

import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Maybe as Y
import qualified Data.Set   as S
```

## Category 3 — Kernel TERM module (phantom DSL)

**Tag**: `kernel terms modules`

**Definition shape**: `define :: String -> TTerm a -> TTermDefinition a`

**Centrally important (unqualified)**:
- `Hydra.Kernel`
- `Hydra.Sources.Libraries` — exports the `_strings_toUpper`, `_lists_length`, ...
  primitive name constants used pervasively in term bodies
- `Hydra.Dsl.Meta.Phantoms as Phantoms` — the term-construction DSL (`var`, `lambda`,
  `lambdas`, `record`, `match`, `cases`, polymorphic `(@@)`). Aliased so files may use
  `Phantoms.x` for disambiguation when needed.
- `Hydra.Sources.Kernel.Types.All`
- `Prelude hiding ((++))` — Hydra redefines `(++)` for string concatenation

**Qualified**:
- `T = Hydra.Dsl.Types` (use `T.` for type signatures and type expressions)
- Library DSLs: `Strings`, `Lists`, `Maps`, `Math`, `Maybes`, `Sets`, `Eithers`,
  `Equality`, `Pairs`, `Logic`, `Literals` (= `Hydra.Dsl.Meta.Lib.Literals`),
  `Chars`
- Kernel meta wrappers: `Core = Hydra.Dsl.Meta.Core`, `Graph = Hydra.Dsl.Meta.Graph`
- Other Hydra DSLs as needed: `Annotations`, `Ast`, `Bootstrap`, `Coders`, `Util`,
  `Packaging`, `Paths`, `Json`, `MetaBase`, `Tabular`, `Testing`, `Tests`,
  `Topology`, `Variants`, `Variants`
- `MetaTerms = Hydra.Dsl.Meta.Terms` when needed
- Std lib: `L`, `M`, `S`, `Y`, `I`

```haskell
module Hydra.Sources.Kernel.Terms.<X> where

-- Standard imports for kernel terms modules
import           Hydra.Kernel
import           Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Phantoms        as Phantoms
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))

import qualified Hydra.Dsl.Annotations          as Annotations
import qualified Hydra.Dsl.Ast                  as Ast
import qualified Hydra.Dsl.Bootstrap            as Bootstrap
import qualified Hydra.Dsl.Coders               as Coders
import qualified Hydra.Dsl.Errors               as Error
import qualified Hydra.Dsl.Json.Model           as Json
import qualified Hydra.Dsl.Literals             as Literals
import qualified Hydra.Dsl.LiteralTypes         as LiteralTypes
import qualified Hydra.Dsl.Meta.Base            as MetaBase
import qualified Hydra.Dsl.Meta.Core            as Core
import qualified Hydra.Dsl.Meta.Graph           as Graph
import qualified Hydra.Dsl.Meta.Lib.Chars       as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers     as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality    as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists       as Lists
import qualified Hydra.Dsl.Meta.Lib.LibLiterals as LibLiterals
import qualified Hydra.Dsl.Meta.Lib.Logic       as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps        as Maps
import qualified Hydra.Dsl.Meta.Lib.Math        as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes      as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs       as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets        as Sets
import qualified Hydra.Dsl.Meta.Lib.Strings     as Strings
import qualified Hydra.Dsl.Meta.Tabular         as Tabular
import qualified Hydra.Dsl.Meta.Terms           as MetaTerms
import qualified Hydra.Dsl.Meta.Testing         as Testing
import qualified Hydra.Dsl.Meta.Variants        as Variants
import qualified Hydra.Dsl.Packaging            as Packaging
import qualified Hydra.Dsl.Parsing              as Parsing
import qualified Hydra.Dsl.Paths                as Paths
import qualified Hydra.Dsl.Prims                as Prims
import qualified Hydra.Dsl.Tests                as Tests
import qualified Hydra.Dsl.Topology             as Topology
import qualified Hydra.Dsl.Types                as T
import qualified Hydra.Dsl.Util                 as Util

import qualified Data.Int                       as I
import qualified Data.List                      as L
import qualified Data.Map                       as M
import qualified Data.Maybe                     as Y
import qualified Data.Set                       as S
```

Most files won't need every import in this list — copy this block as a template
and trim the imports your file doesn't reference. The aliases are stable across files
so a reader recognizes `Lists.map` or `Strings.cat` without checking the import block.

Note: when a file imports both `Hydra.Dsl.Literals` and `Hydra.Dsl.Meta.Lib.Literals`,
the latter is aliased `LibLiterals` to disambiguate.

## Category 4 — Kernel TERM module with DeepCore

**Tag**: `kernel terms modules with DeepCore`

**Same as category 3, plus**:
- Qualified: `DeepCore = Hydra.Dsl.Meta.DeepCore`
- Unqualified: the DeepCore operator `(@@@)`

```haskell
-- Standard imports for kernel terms modules with DeepCore
... (same as category 3) ...
import qualified Hydra.Dsl.Meta.DeepCore as DeepCore
import           Hydra.Dsl.Meta.DeepCore ((@@@))
```

## Category 5 — Term-level source outside the kernel

**Tag**: `term-level sources outside of the kernel`

**Same as category 3, with one delta**: `Hydra.Sources.Kernel.Types.All` is **qualified**
as `KernelTypes` (since the file is in a downstream package).

Per-language imports follow the trailing-PascalCase + collision rule. When a file
imports both the runtime `Hydra.<Lang>.<X>` and the source `Hydra.Sources.<Lang>.<X>`,
both grow to disambiguate: the source side is suffixed `Source` (e.g. `JavaSyntaxSource`)
and the runtime side takes the language prefix (e.g. `JavaSyntax`). When only one is
imported, the bare alias is preferred.

```haskell
module Hydra.Sources.<Lang>.<X> where

-- Standard imports for term-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Phantoms             as Phantoms
import           Prelude hiding ((++))

import qualified Hydra.Sources.Kernel.Types.All      as KernelTypes
import qualified Hydra.Dsl.Annotations               as Annotations
... (rest as in category 3) ...

-- Per-language imports (follow collision rule):
import qualified Hydra.<Lang>.Syntax                 as <Lang>Syntax       -- runtime
import qualified Hydra.Sources.<Lang>.Syntax         as <Lang>SyntaxSource -- DSL source
```

## Category 6 — Tests

**Tag**: `tests`

**Definition shape**: defines `TTermDefinition TestGroup` values. Test cases call
**kernel function bindings** via the polymorphic `(@@)` from Phantoms — e.g.
`validateCoreTermProfiledRef @@ profile @@ graph @@ input`. Composes fixtures
(`TestGraph`, `TestTerms`, `TestTypes`).

**Distinguishing feature**: bare `(@@)` is *binding application* (Phantoms.@@),
not Hydra-runtime term application.

**Centrally important (unqualified)**:
- `Hydra.Kernel`
- `Hydra.Sources.Libraries`
- `Hydra.Dsl.Meta.Phantoms as Phantoms`, including the `(@@)` operator
  (brought in unqualified explicitly so bare `@@` is the polymorphic
  `Phantoms.@@`, NOT the monomorphic `Terms.@@`).
- `Hydra.Dsl.Meta.Testing as Testing` — provides `supergroup`, `subgroup`, `testCase`,
  the `(===)` operator
- `Hydra.Testing` — runtime types `TestGroup`, `TestCase`, etc.
- `Hydra.Sources.Kernel.Types.All`
- `Hydra.Dsl.Meta.Terms as Terms hiding ((@@))` — bring `match`, `record`, `inject`,
  etc. unqualified, but NOT `(@@)`. Test bodies use `match`/`record`/`inject` directly,
  with `(@@)` referring to Phantoms's polymorphic version.

**Qualified**:
- `T = Hydra.Dsl.Meta.Types` (in tests, `T.` is the meta-types DSL)
- Library DSLs and meta wrappers same as category 3.
- Fixture imports: `TestGraph`, `TestTerms`, `TestTypes` — by their bare names.
- Std lib.

```haskell
module Hydra.Sources.Test.<X> where

-- Standard imports for tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms hiding ((@@))
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import           Hydra.Dsl.Meta.Phantoms                ((@@))
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing
import Hydra.Sources.Libraries
```

When a test file references kernel-term primitives directly, add the relevant
`Hydra.Sources.Kernel.Terms.<X>` qualified imports (e.g. `ShowCore`, `Lexical`).

## Category 7 — Term-encoded tests

**Tag**: `term-encoded tests`

**Definition shape**: same as category 6, but test cases construct **Hydra-runtime
term applications** (encoded `Term` values containing lambda applications, primitive
applications, etc.) — e.g. `lambda "x" body @@ arg`, `primitive _strings_toUpper @@ string "hello"`.

**Distinguishing feature**: bare `(@@)` is *term application* (Terms.@@), the
monomorphic `TTerm Term -> TTerm Term -> TTerm Term`. The polymorphic Phantoms.@@
is qualified at use sites where binding application is also needed.

These tests typically test kernel infrastructure for handling encoded terms —
reduction, eta-expansion, validation of encoded programs, term inference, etc.

**Same as category 6, except**: bare `(@@)` is Terms.@@, not Phantoms.@@. Drop the
`hiding ((@@))` from `Meta.Terms` and don't import `(@@)` from Phantoms.

```haskell
module Hydra.Sources.Test.<X> where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing
import Hydra.Sources.Libraries
```

When binding application is needed in a term-encoded test (e.g. applying
a kernel-side ref like `validateCoreTermRef`), use `Phantoms.@@` qualified.

## Category 8 — Kernel test fixtures

**Tag**: `kernel test fixtures`

**Definition shape**: defines `TTermDefinition Term`, `TTermDefinition Type`, or
`TTermDefinition Binding` values — named test data used as inputs to category-6/7 files.

**Same as category 7, with two deltas**:
- `Hydra.Testing` is **NOT** imported (no `TestGroup`/`TestCase` constructors needed).
- `Hydra.Sources.Libraries` is **NOT** imported (fixtures don't reference primitive
  name constants).

```haskell
module Hydra.Sources.Test.<Fixture> where

-- Standard imports for kernel test fixtures
import           Hydra.Kernel
import           Hydra.Dsl.Meta.Phantoms       as Phantoms
import           Hydra.Dsl.Meta.Testing        as Testing
import           Hydra.Sources.Kernel.Types.All

import qualified Hydra.Dsl.Meta.Core           as Core
import qualified Hydra.Dsl.Meta.Types          as T
import qualified Hydra.Dsl.Packaging           as Packaging

import qualified Data.List                     as L
import qualified Data.Map                      as M
```

(Files: `Test/TestGraph.hs`, `Test/TestTerms.hs`, `Test/TestTypes.hs`, `Test/TestEnv.hs`.)

## Uncategorized files

The following kinds of file under `Sources/` are not subject to the standardized blocks
above. Their imports should be hand-tuned to whatever they actually need, and they
should self-tag with a comment when their imports diverge significantly from the
nearest category.

- **Manifests** (`*/Manifest.hs`, one per package). Build a `Package` value from member
  modules' `module_` values. Imports are bespoke — typically the kernel manifest
  helpers plus all member-module imports.
- **Top-level aggregators** (`Sources/All.hs`, `Sources/Test/All.hs`,
  `Sources/Kernel/Terms/All.hs`, `Sources/Kernel/Types/All.hs`). Re-export and concatenate
  member modules' content.
- **Sub-suite aggregators** (`Sources/Test/Checking/All.hs`, etc.). These DO contain a
  `module_` and follow category 6, but most of the body is `concatMap` over child
  suites' `module_` definitions; the import block is dominated by qualified imports of
  the children.
- **`Hydra.Sources.Libraries`**. Primitive name registry. Bespoke.
- **`Hydra.Sources.Kernel.Manifest`**. Kernel-package manifest.
- **`Hydra.Sources.Json.Bootstrap`**. Builds a `Module` from kernel-type metadata using
  raw `TermDefinition` constructors rather than the DSL. Self-tags with a comment
  explaining why imports differ.

## Migration notes for existing files

A few non-obvious rules emerged from a survey of the existing codebase:

1. **`Phantoms.@@` was an anti-pattern in tests that apply kernel bindings.**
   Files that imported `Hydra.Dsl.Meta.Terms` unqualified-with-alias and `Phantoms`
   qualified had to write `Phantoms.@@` everywhere — because the unqualified `(@@)`
   from `Meta.Terms` is the narrower `TTerm Term -> TTerm Term -> TTerm Term`, while
   the polymorphic `Phantoms.@@ :: AsTerm f (a -> b) => f -> g -> TTerm b` is what
   binding-application call sites actually want. Category 6 (`tests`) inverts this:
   `Meta.Terms hiding ((@@))`, then `Phantoms ((@@))` brought unqualified, so bare
   `@@` is the polymorphic version. Category 7 (`term-encoded tests`) keeps the
   monomorphic `Terms.@@` bare because it constructs Hydra-runtime term applications.
2. **`Strings` is qualified everywhere.** Earlier convention had `Strings`
   imported unqualified-with-alias, but in practice 1,100+ call sites already used
   `Strings.cat`/`Strings.intercalate`/etc. Drop the unqualified import; the bare
   form was rarely used in earnest.
3. **`unaryFunction`/`binaryFunction` were renamed to `reify`/`reify2`** in
   `Hydra.Dsl.Meta.Phantoms`.
4. **`DeepCore` is the canonical alias for `Hydra.Dsl.Meta.DeepCore`.** Earlier
   files used `DC`; standardize on `DeepCore`.
5. **`AsName` typeclass.** Phantoms's `inject`, `injectUnit`, `match`, `project`,
   `record`, `unwrap`, `wrap` accept any `AsName n => n` for type-name arguments —
   `Name`, `TBinding Name`, or `TTermDefinition Name`. This lets call sites pass
   pre-defined kernel name constants like `_Foo` directly, without explicit lifts.
6. **`annot` vs `annots`.** `Phantoms.annot` attaches a single `(key, value)`
   annotation to a term. `MetaTerms.annots` (formerly `MetaTerms.annot`) attaches
   an entire `Map Name Term` to a term. The rename disambiguates the two.

## See also

- [Coding-style wiki page](https://github.com/CategoricalData/hydra/wiki/Coding-style) —
  the broader style guide; links here for import details.
- [DSL guide](dsl-guide.md) — comprehensive reference for DSL operators and patterns.
