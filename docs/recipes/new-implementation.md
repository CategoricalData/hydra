# Creating a new Hydra implementation

Hydra currently has three complete implementations:
[Hydra-Haskell](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell),
[Hydra-Java](https://github.com/CategoricalData/hydra/tree/main/hydra-java), and
[Hydra-Python](https://github.com/CategoricalData/hydra/tree/main/hydra-python).
All three implement the entire [Hydra Kernel](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Kernel.hs),
support the full [Hydra standard library](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Lib),
and pass the [common test suite](https://github.com/CategoricalData/hydra/wiki/Testing).
The three implementations are mutually self-hosting: each can load Hydra modules from a
language-independent JSON representation and regenerate code for any of the three target languages
(see the [bootstrapping demo](https://github.com/CategoricalData/hydra/tree/main/hydra-ext/demos/bootstrapping)).
Hydra-Haskell serves as the source of truth for the kernel, but the generated code in all three
languages is semantically equivalent.

The following is a guide to creating a Hydra implementation in a new host language, like [Scala](https://github.com/CategoricalData/hydra/tree/main/hydra-scala) or [C#](https://github.com/CategoricalData/hydra/issues/139).
For this guide, we'll use a hypothetical language called `NewLang` as our example.

## Step 1: Create the syntax model

The very first thing you should do when undertaking a new implementation is to define the syntax of the host language using Hydra's native data model, Hydra Core.
This syntax model will be the foundation of everything which follows, so it is essential to get it right.
Some considerations to keep in mind:

* Always rely on an existing source of truth for the syntax, such as an existing BNF grammar or reference documentation. These existing sources of truth can always be found for mainstream languages. Don't reinvent the wheel.
* There may be different sources of truth for a given language. Choose the one which is the most standard, or the best fit for your application. Also consider whether the source of truth will be supported and maintained over time.
* Languages may change over time. Be sure to note the version (if any) or timestamp at which the source of truth was retrieved, along with a URL, in the documentation for your new syntax model.

Look at other syntax models for inspiration:

**Kernel languages** (in hydra-haskell):
- [Haskell syntax](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Types/Haskell.hs) - Based on Haskell language specification
- [JSON syntax](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Types/Json.hs) - Simple data format

**Extended languages** (in hydra-ext):
- [Java syntax](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Java/Syntax.hs) - Based on standardized grammar
- [Python syntax](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Python/Syntax.hs) - Python 3 grammar
- [Scala syntax](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Scala/Meta.hs) - Scala meta model

Note that the Java and Python syntax models follow the best practice of using specific versions of standardized grammars as the source of truth.

It is a good idea to copy the entire source of truth verbatim, then enclose it in comments and add the Hydra type definitions inline beneath the corresponding productions in the source of truth; this will be useful later when cross-referencing or making updates.

Programming language grammars are usually quite large, so creating the Hydra syntax model may be one of the most time-consuming steps in this process.
Utilizing an LLM-based tool like ChatGPT or Claude Code can greatly speed things up, although you must carefully check and correct its output at every point; you will find that there are many design decisions which only you can make.

### Where to place your syntax model

- **For kernel integration**: Place in `hydra-haskell/src/main/haskell/Hydra/Sources/NewLang/Syntax.hs`
- **For extended features**: Place in `hydra-ext/src/main/haskell/Hydra/Ext/Sources/NewLang/Syntax.hs`

The distinction: kernel languages are essential to Hydra's core functionality (like Haskell itself), while extended languages are additional targets for code generation.

## Step 2: Define language constraints

Characterizing a target language in terms of constraints (supported and unsupported type/term expressions) allows Hydra to do much of the work of adapting types and terms so they can be expressed in that language.
A language is given a unique name like "haskell" or "java", and is characterized according to:

* **Supported type variants** (type constructors). E.g. Haskell supports both record types like `LatLon {lat :: Double, lon :: Double}` and product types like `(Double, Double)`, whereas Java only supports the former.
* **Supported term variants**. Similar to supported type variants. While some data languages, like JSON or Protobuf, may be very constrained in their term expressions (e.g. no functions, no variables, etc.) a Hydra host language will need to support all of the lambda calculus constructors, plus term variants (like list, map, etc.) for all supported type variants.
* **Supported function variants**. A Hydra host language must support all three (eliminations, lambdas, and primitives).
* **Supported elimination variants** (e.g. for projections, case statements, etc.). These will generally mirror the supported term variants.
* **Supported literal variants**. The options are binary, boolean, integer, float, and string. Only the last of these is strictly required, but a typical host language would include all five.
* **Supported integer variants**. The options are 8- to 64-bit signed and unsigned integers, as well as big integers. None are required; include as many as have a natural counterpart in the host language. For example, 16-bit signed integers in Java are called `short`, while big integers are called `BigInteger`.
* **Supported floating-point variants**. Similar to integer variants, but there are only three options: 32-bit and 64-bit floating-point numbers (i.e. float and double), as well as big float.

Language constraints are defined for every language coder in Hydra. It is not necessary to get the constraints 100% correct before creating the coder; you can start by copying an existing set of language constraints, then refining them as you get to know the target language.

### Examples of language constraints

**Kernel languages**:
- [Haskell constraints](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Haskell/Language.hs) - Written in DSL
- [JSON constraints](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Json/Language.hs) - Very constrained (no functions)

**Extended languages**:
- [Java constraints](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Java/Language.hs) - OOP language
- [Python constraints](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Python/Language.hs) - Dynamic language

It is preferable to use the DSL, as that allows the language constraints to be propagated into each Hydra implementation through code generation.

**Note**: It is conventional in Hydra to define a list of reserved words for the target language in the same module as the language constraints.
See for example the `reservedWordsDef` element in the [Java constraints](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Java/Language.hs), or `reservedWords` in the [Haskell constraints](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Haskell/Language.hs).

## Step 3: Generate Haskell sources

When you create your coder and serializer in steps 4 and 5, you will use Haskell sources (assuming you are using Hydra-Haskell as your jumping-off point) which are generated from the syntax model and language constraints you have just defined in steps 1 and 2.
These generated Haskell sources provide strongly-typed representations of your target language's syntax, which your coder will construct and your serializer will render to text.

### For kernel languages

Create sources in:
- `hydra-haskell/src/main/haskell/Hydra/Sources/NewLang/Syntax.hs`
- `hydra-haskell/src/main/haskell/Hydra/Sources/NewLang/Language.hs`

Add them to [Sources/All.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/All.hs), then use the [Hydra-Haskell README](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell) instructions to generate the Haskell code:

```haskell
-- In hydra-haskell REPL
import Hydra.Generation
-- writeHaskell takes: output dir, universe modules, modules to generate
writeHaskell "src/gen-main/haskell" mainModules [newLangSyntaxModule, newLangLanguageModule]
```

### For extended languages

Create sources in:
- `hydra-ext/src/main/haskell/Hydra/Ext/Sources/NewLang/Syntax.hs`
- `hydra-ext/src/main/haskell/Hydra/Ext/Sources/NewLang/Language.hs`

Add them to the appropriate registry in hydra-ext, then generate:

```haskell
-- In hydra-ext REPL or script
import Hydra.Ext.Generation
let universeModules = kernelModules ++ hydraExtModules
writeHaskell "src/gen-main/haskell" universeModules [newLangSyntaxModule, newLangLanguageModule]
```

After generation, you will be able to import the generated code:

```haskell
import qualified Hydra.Ext.NewLang.Syntax as NL
import Hydra.Ext.NewLang.Language
```

## Step 4: Create a coder

Now that you have a model for your target language, and have defined the language constraints, you need a way of mapping native Hydra expressions into that language.
You can split this task into two parts: a coder (this step), and a serializer (step 5).

A central idea of Hydra is that the kernel should be defined once, in a Hydra DSL, and then mapped into various host languages using language-specific *coders*.
Currently, the Hydra kernel and also the language coders are written in Haskell.

### Examples of coders

**Kernel language coders**:
- [Haskell coder](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Haskell/Coder.hs) - ~600 lines, very close to Hydra Core
- [JSON coder](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Json/Coder.hs) - Data format coder

**Extended language coders** (in hydra-ext/src/main/haskell/Hydra/Ext/Staging):
- [Java coder](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Staging/Java/Coder.hs) - ~1500 lines, OOP patterns
- [Python coder](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Staging/Python/Coder.hs) - Dynamic typing

Note that there are also many coders for [other languages](https://github.com/CategoricalData/hydra/tree/main/hydra-ext/src/main/haskell/Hydra/Ext) in which we do not have full Hydra implementations (Avro, Protobuf, GraphQL, etc.).

### Purpose

In terms of implementing Hydra in a new language, the purpose of creating a language coder is that all of the Hydra kernel code which is written in a Hydra DSL can be automatically ported to the new language, saving work and ensuring parity across implementations.
The coder can then also be used to port other, non-kernel code.

### Implementation notes

While step 1 might have been very time-consuming but relatively simple, step 4 is more complex.
LLMs will be of more limited usefulness here.
Your coder must operate both at the type level (mapping Hydra type expressions into equivalent class or interface definitions in the target language) and also at the term level (mapping Hydra term expressions into equivalent values, data structures, and executable code).
Writing such a coder is one of the most challenging development tasks in Hydra, and this simple guide will not attempt to break it down into a step-by-step recipe. However, some notes:

* The top-level structure of these coders conforms to a standard API; see how coders are registered in [Hydra/Ext/Generation.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Generation.hs).
* The coder only needs to be unidirectional; for programming language coders, we are usually not interested in mapping native programs in the target language back into Hydra Core.
* There will usually be a helper object, e.g. called "namespaces" or "aliases", which is passed into each function that deals with qualified names.
* The greater the semantic distance between Hydra Core and the target language, the larger and more complex the coder will need to be. For example, Haskell is very close to Hydra Core, so the Haskell coder is about 600 lines of code. Java is more remote, and requires more than 1500 lines of code.

### Where to place your coder

- **For kernel languages**: `hydra-haskell/src/main/haskell/Hydra/Sources/NewLang/Coder.hs` (written in DSL, will be code-generated)
- **For extended languages**: `hydra-ext/src/main/haskell/Hydra/Ext/Staging/NewLang/Coder.hs` (written in native Haskell)

## Step 5: Create the serializer

Now that you have the coder and the language constraints which allow you to map Hydra Core expressions into an abstract syntax tree for your language, you need to also map the abstract syntax tree into a concrete syntax.
Compared to some of the steps above, this is one of the conceptually simplest tasks, and requires little explanation.

It will be helpful to re-use the built-in [Hydra.Ast](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/gen-main/haskell/Hydra/Ast.hs) module here.

### Examples of serializers

**Kernel languages**:
- [Haskell SerDe](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Haskell/Serde.hs) - Written in DSL
- [JSON SerDe](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Json/Serde.hs) - Simple format

**Extended languages** (in hydra-ext/src/main/haskell/Hydra/Ext/Staging):
- [Java SerDe](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Staging/Java/Serde.hs) - Handles complex syntax
- [Python SerDe](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Staging/Python/Serde.hs) - Indentation-based syntax

Note that while "SerDe" stands for "serializer and deserializer", you will only need the former; we have no need to map *from* expressions in the target language, as noted above.

## Step 6: Register and generate code

Now the fun part.
You have your coder and serializer, and you're ready to translate Hydra's kernel code and test suite from the Haskell DSL into the compile-time environment of the host language.

### Register code generation functions

Add your language's code generation functions to the appropriate module:

**For kernel languages**, add to [Hydra/Generation.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Generation.hs):

```haskell
writeNewLang :: FilePath -> [Module] -> [Module] -> IO ()
writeNewLang = generateSources moduleToNewLang newLangLanguage doExpand doHoistCase doHoistPoly
```

**For extended languages**, add to [Hydra/Ext/Generation.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Generation.hs):

```haskell
writeNewLang :: FilePath -> [Module] -> [Module] -> IO ()
writeNewLang = generateSources moduleToNewLang newLangLanguage doExpand doHoistCase doHoistPoly
```

The three boolean flags (`doExpand`, `doHoistCase`, `doHoistPoly`) control term transformations
applied before code generation. See the existing definitions of `writeJava` and `writePython` for
typical settings.

See existing examples:
- `writeHaskell` in [Hydra/Generation.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Generation.hs)
- `writeJava` and `writePython` in [Hydra/Ext/Generation.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Generation.hs)

### Generate native sources

Start by creating a new top-level directory for your implementation: `hydra-newlang`.

Using the `writeNewLang` function you just registered:

```haskell
-- In hydra-ext REPL (or hydra-haskell for kernel languages)
import Hydra.Ext.Generation
let universeModules = kernelModules ++ hydraExtModules
writeNewLang "../hydra-newlang/src/gen-main/newlang" universeModules mainModules
writeNewLang "../hydra-newlang/src/gen-test/newlang" universeModules testModules
```

Examples for existing implementations:
- **Java**: See [Hydra-Java README](https://github.com/CategoricalData/hydra/tree/main/hydra-java) for generation examples
- **Python**: Uses `writePython` from Hydra/Ext/Generation.hs

Once you have generated the sources, make sure they compile.
If not, iterate on your coder and/or serializer.
Expect many iterations: compilation errors often point to gaps in the coder's handling of specific type or term patterns (e.g. lambda cast types, polymorphic method signatures, type annotations on intermediate expressions).
Type inference issues in the Hydra kernel can also surface as codegen errors; if a generated type looks wrong, trace the issue back to the inferred type annotations on the Hydra IR before assuming the coder is at fault.

## Step 7: Implement standard primitives

As noted above, Hydra has a [standard library](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Lib) of primitive, or built-in functions.
There are many calls to these functions (though not all of them) in the Hydra kernel code which will be mapped into your new implementation, and you will get compile-time or runtime errors if they are not present when you attempt to compile or run the generated kernel code.

Luckily, most of them are pretty simple and straightforward.
Primitive functions may be implemented in different ways in different Hydra implementations, though their behavior must be the same across implementations.

### How primitives are organized

**Haskell**:
- Metadata: [Hydra/Sources/Libraries.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Libraries.hs) (DSL)
- Implementations: [Hydra/Lib](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Lib) (native Haskell)

**Java**:
- Metadata + implementations: [hydra/lib](https://github.com/CategoricalData/hydra/tree/main/hydra-java/src/main/java/hydra/lib) (bundled together)
- Registry: [hydra/lib/Libraries.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/main/java/hydra/lib/Libraries.java)

**Python**:
- Implementations: [hydra/lib](https://github.com/CategoricalData/hydra/tree/main/hydra-python/src/main/python/hydra/lib) (hand-written)

### Requirements for primitives

1. The primitives are available to be referenced by compile-time code in the host language
2. The primitives are available to be called in an interpreted environment
3. All of the required metadata, including the type signature, of each primitive can be looked up using the primitive's name
4. The runtime behavior of the primitive conforms to Hydra's language-independent test suite (see step 9)

See [Graph.hs:57](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Types/Graph.hs#L57) for the required fields: `name`, `type`, and `implementation`.

Note that while in principle, every Hydra implementation ought to implement every primitive in the standard library, currently there are small differences (a handful of primitives are not yet implemented in all languages).
At a bare minimum, all of the primitives which are referenced in the Hydra kernel need to be implemented.

**Important**: Simply having implementation files is not enough — each primitive must also be *registered* in a central registry (e.g. `Libraries.java` in Java) so it can be looked up by name at runtime.
Periodically compare your registry against the authoritative list in [Hydra/Sources/Libraries.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Libraries.hs) to catch any missing registrations.

### Primitive infrastructure

In addition to the implementations themselves, you need supporting infrastructure:

- A **base class or protocol** defining the `Primitive` interface (name, type scheme, implementation)
- **TermCoder factories** that bridge between host-language values and Hydra `Term`s
- A **registry** function that assembles all primitives into a `Graph`

| Language | Key files |
|----------|-----------|
| Java | [PrimitiveFunction.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/main/java/hydra/tools/PrimitiveFunction.java), [Libraries.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/main/java/hydra/lib/Libraries.java) |
| Python | [hydra/dsl/prims.py](https://github.com/CategoricalData/hydra/blob/main/hydra-python/src/main/python/hydra/dsl/prims.py), [hydra/sources/libraries.py](https://github.com/CategoricalData/hydra/blob/main/hydra-python/src/main/python/hydra/sources/libraries.py) |

## Step 8: Implement runtime foundation types

The generated kernel code imports a small set of types that must exist before anything compiles.
These are language-specific representations of Hydra's core algebraic types:

- **Maybe/Optional** (with explicit `Just`/`Nothing` or equivalent)
- **Either** (with `Left`/`Right`)
- **Unit** (the empty product type)
- **Lazy** (memoized deferred evaluation — needed for recursive let bindings)
- **Immutable collections** (if your language's standard collections are mutable)

| Language | Module |
|----------|--------|
| Java | [hydra/util/](https://github.com/CategoricalData/hydra/tree/main/hydra-java/src/main/java/hydra/util) (`Maybe`, `Either`, `Lazy`, `Unit`, `Pair`, `Tuple`) |
| Python | [hydra/dsl/python.py](https://github.com/CategoricalData/hydra/blob/main/hydra-python/src/main/python/hydra/dsl/python.py) (`Just`/`Nothing`, `Left`/`Right`, `FrozenDict`, `frozenlist`, `Node`) |

## Step 9: Create test runners

Hydra includes a common test suite which is intended to be evaluated in the same way across
implementations, ensuring parity. The sources for the test suite can be found in the Haskell DSL at
[Hydra/Sources/Test](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Sources/Test).

See the [Testing wiki page](https://github.com/CategoricalData/hydra/wiki/Testing) for comprehensive documentation.

### Kernel tests vs generation tests

The common test suite gives rise to two kinds of tests:

- **Kernel tests** validate that Hydra's runtime works correctly in your implementation. The test
  cases are code-generated into the target language, and your implementation runs them against its
  own kernel (primitives, type checker, reducer, etc.). This answers: *"Does the Hydra kernel behave
  correctly in this language?"*

- **Generation tests** validate that Hydra's code generators produce correct output for your
  language. The same test cases are used to generate code in the target language, then verify that
  the generated code compiles and produces the expected results. This answers: *"Does code generated
  by Hydra work correctly in the target language?"*

In other words, kernel tests exercise Hydra-as-interpreter, while generation tests exercise
Hydra-as-compiler. Both are generated to `src/gen-test/` and both need a hand-written test runner.

**Passing all test cases in the common test suite (both kernel and generation tests) is the criterion
for a complete Hydra implementation.**

### Examples of test runners

**Haskell**:
- Generated suite: [src/gen-test/haskell/Hydra/Test/TestSuite.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/gen-test/haskell/Hydra/Test/TestSuite.hs)
- Runner: [src/test/haskell/Hydra/TestSuiteSpec.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/test/haskell/Hydra/TestSuiteSpec.hs) (~100 lines)

**Java**:
- Generated suite: [src/gen-test/java/hydra/test/testSuite/TestSuite.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/gen-test/java/hydra/test/testSuite/TestSuite.java)
- Runner: [src/test/java/hydra/TestSuiteRunner.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/test/java/hydra/TestSuiteRunner.java) (~100 lines)

**Python**:
- Generated suite: Generated to `hydra-python/src/gen-test/python/hydra/test/`
- Runner: [src/test/python/test_suite_runner.py](https://github.com/CategoricalData/hydra/blob/main/hydra-python/src/test/python/test_suite_runner.py)

The generated test data should already be available (from step 6). The kernel test runner is
typically straightforward (around 100 lines in Haskell and Java), though it can be more substantial
depending on the host language's test framework. The generation test runner is similar in structure.

### Build system

As you add a test runner, you will also need an implementation-specific build system to compile
generated code, manage dependencies, and run tests. Examples:

| Language | Build system |
|----------|-------------|
| Haskell | [Stack](https://docs.haskellstack.org/) (`stack build`, `stack test`) with `package.yaml` |
| Java | [Gradle](https://gradle.org/) (`./gradlew build`, `./gradlew test`) with `build.gradle` |
| Python | [uv](https://github.com/astral-sh/uv) + [pytest](https://pytest.org/) with `pyproject.toml` |

## Step 10: Create native DSLs and build applications

OK, now for the *really* fun part.
This is where you depart from the script and create whatever domain-specific languages and/or additional utilities will be useful in your new implementation, then start building real applications to validate that everything works correctly.

### Essential DSLs

You should start with these three DSLs, and then add others as desired:

1. **Type construction DSL**: Allows developers to build type-level expressions
   - [Haskell Types.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Dsl/Types.hs)
   - [Java Types.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/main/java/hydra/dsl/Types.java)

2. **Term construction DSL**: Allows developers to build term-level expressions
   - [Haskell Terms.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Dsl/Terms.hs)
   - [Java Terms.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/main/java/hydra/dsl/Terms.java)

3. **Term decoding DSL ("expect")**: Allows developers to decode Hydra terms to native programming constructs
   - [Haskell Expect.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Dsl/Expect.hs)
   - [Java Expect.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/main/java/hydra/dsl/Expect.java)

### Additional useful DSLs

You may find it useful to define:
- [Shorthand type constructors](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Dsl/ShorthandTypes.hs)
- [Shorthand primitives](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Dsl/Prims.hs)
- Individual DSLs for specific Hydra kernel modules, e.g. the [hydra.core DSL](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Dsl/Core.hs) in Haskell

The last of these are only necessary if you plan to define new source-of-truth modules in your implementation, rather than just receiving generated code from Hydra-Haskell.

### Build applications

Time to start using the new Hydra implementation, surfacing any bugs, and filling in gaps.
Build real applications to validate that your implementation works correctly and has good ergonomics for developers in the target language.
Implementations are allowed and expected to vary in the DSLs and utilities they provide, so as to make the best use of the programming constructs and libraries available in the particular host language.

## Step 11: Implement code generation I/O (for self-hosting)

Now let's turn it up to 11.

If you want your implementation to generate code (i.e. participate in Hydra's signature bootstrapping functionality),
you need an I/O wrapper that loads modules from JSON, assembles a bootstrap `Graph` with all
standard primitives, runs the code generation pipeline, and writes output files.

| Language | Module |
|----------|--------|
| Java | [hydra/Generation.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/main/java/hydra/Generation.java), [hydra/Bootstrap.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/main/java/hydra/Bootstrap.java) |
| Python | [hydra/generation.py](https://github.com/CategoricalData/hydra/blob/main/hydra-python/src/main/python/hydra/generation.py), [hydra/bootstrap.py](https://github.com/CategoricalData/hydra/blob/main/hydra-python/src/main/python/hydra/bootstrap.py) |
| Haskell | [Hydra/Generation.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Generation.hs) |

## Summary

Creating a new Hydra implementation involves:

1. ✅ Define syntax model in Hydra DSL
2. ✅ Define language constraints
3. ✅ Generate Haskell sources from your DSL definitions
4. ✅ Create a coder (AST mapping)
5. ✅ Create a serializer (AST to concrete syntax)
6. ✅ Register code generation functions and generate native sources
7. ✅ Implement standard library primitives
8. ✅ Implement runtime foundation types
9. ✅ Create test runners for kernel tests and generation tests
10. ✅ Create native DSLs and build applications
11. ✅ Implement code generation I/O for self-hosting

The key insight: most of the implementation is **automatically generated** from DSL sources, ensuring parity across all Hydra implementations. The hand-written code falls into well-defined categories: runtime foundation types, primitive implementations and registry, DSLs, test runners, and (optionally) code generation I/O for self-hosting.

## A note on maintaining hand-written code

As the generated API evolves (e.g. method signatures change, packages are reorganized), hand-written
code must be updated to match. When the generated code is regenerated, run the full build (not just
compilation) to catch breakage early.
