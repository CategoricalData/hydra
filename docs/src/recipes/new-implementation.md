# Creating a New Hydra Implementation

Hydra currently has implementations in Haskell ([Hydra-Haskell](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell)) and Java ([Hydra-Java](https://github.com/CategoricalData/hydra/tree/main/hydra-java)).
Hydra-Haskell is more *complete* than Hydra-Java, in that it implements the entire [Hydra Kernel](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Kernel.hs), although Hydra-Java has a number of utilities which Hydra-Haskell does not.
Another criterion for completeness is that the implementation supports the entire [Hydra standard library](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Lib), which both Hydra-Haskell and Hydra-Java do.

Prior to the 1.0 release, Hydra will have at least two *complete* implementations, but possibly more. The following is a rough guide to creating a Hydra implementation in a new host language, like [Python](https://github.com/CategoricalData/hydra/issues/66) or [C#](https://github.com/CategoricalData/hydra/issues/139).
The examples will assume that you are creating a new Python implementation, called `Hydra-Python`.


## Step 1: create the syntax model

The very first thing you should do when undertaking a new implementation is to define the syntax of the host language using Hydra's native data model, Hydra Core.
This syntax model will be the foundation of everything which follows, so it is essential to get it right.
Some considerations to keep in mind:
* Always rely on an existing source of truth for the syntax, such as an existing BNF grammar or reference documentation. These existing sources of truth can always be found for mainstream languages. Don't reinvent the wheel.
* There may be different sources of truth for a given language. Choose the one which is the most standard, or the best fit for your application. Also consider whether the source of truth will be supported and maintained over time.
* Languages may change over time. Be sure to note the version (if any) or timestamp at which the source of truth was retrieved, along with a URL, in the documentation for your new syntax model.

Look at other syntax models for inspiration, such as the one [for Haskell](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Tier3/Ext/Haskell/Ast.hs) and the one [for Java](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Tier3/Ext/Java/Syntax.hs).
Note that the Haskell syntax model was written first, and was based on a third-party library ([haskell-tools-ast](https://hackage.haskell.org/package/haskell-tools-as)) which has not been updated since 2019.
The Java syntax model is newer, and follows the above advice of using a specific version of a standardized grammar as the source of truth.

It is a good idea to copy the entire source of truth verbatim, then enclose it in comments and add the Hydra type definitions inline beneath the corresponding productions in the SoT; this will be useful later when cross-referencing the SoT or making updates.

Programming language grammars are usually quite large, so creating the Hydra syntax model may be one of the most time-consuming steps in this process.
Utilizing an LLM-based tool like ChatGPT or GitHub Copilot can greatly speed things up, although you must carefully check and correct its output at every point; you will find that there are many design decisions which only you can make.

## Step 2: define language constraints

Characterizing a target language in terms of constraints (supported and unsupported type/term expressions) allows Hydra to do much of the work of adapting types and terms so they can be expressed in that language.
A language is given a unique name like "haskell" or "java", and is and characterized according to:
* Supported type variants (type constructors). E.g. Haskell supports both record types like `LatLon {lat :: Double, lon :: Double}` and product types like `(Double, Double)`, whereas Java only supports the former.
* Supported term variants. Similar to supported type variants. While some data languages, like JSON or Protobuf, may be very constrained in their term expressions (e.g. no functions, no variables, etc.) a Hydra host language will need to support all of the lambda calculus constructors, plus term variants (like list, map, etc.) for all supported type variants (like list, map, etc.).
* Supported function variants. A Hydra host language must support all three (eliminations, lambdas, and primitives).
* Supported elimination variants (e.g. for projections, injections, folds, etc.). These will generally mirror the supported term variants.
* Supported literal variants. The options are binary, boolean, integer, float, and string. Only the last of these is strictly required, but a typical host language would include all five.
* Supported integer variants. The options are 8- to 64-bit signed and unsigned integers, as well as big integers. None are required; include as many as have a natural counterpart in the host language. For example, 16-bit signed integers in Java are called `short`, while big integers are called `BigInteger`.
* Supported floating-point variants. Similar to integer variants, but there are only three options: 32-bit and 64-bit floating-point numbers (i.e. float and double), as well as big float.

Language constraints are defined for every language coder in Hydra; see the pointers under step #3 (create a coder).
It is not necessary to get the constraints 100% correct before creating the coder; you can start by copying an existing set of language constraints, then refining them as you get to know the target language.

The Haskell language constraints are defined [here](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Ext/Haskell/Language.hs), and the Java language constraints are defined using the Haskell DSL [here](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Tier3/Ext/Java/Language.hs).
It is preferable to use the DSL, as that allows the language constraints to be propagated into each Hydra implementation.

Note: it is conventional in Hydra to define a list of reserved words for the target language in the same module as the language constraints.
See for example the `reservedWordsDef` element in the [Java constraints](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Tier3/Ext/Java/Language.hs), or `reservedWords` in the [Haskell constraints](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Ext/Haskell/Language.hs).

## Step 3: generate Haskell sources

When you create your coder and serializer in steps 4 and 5, you will use Haskell sources (assuming you are using Hydra-Haskell as your jumping-off point) which are generated from the syntax model and language constraints you have just defined in step 1 and 2.

Create these sources in a conventional location, like `hydra-haskell/src/main/haskell/Hydra/Sources/Tier3/Ext/Python/Syntax.hs` and `hydra-haskell/src/main/haskell/Hydra/Sources/Tier3/Ext/Python/Language.hs` (if your target language is Python).
When the sources are ready, add them to a registry like [Tier3/All.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Tier3/All.hs), then follow the instructions in the [Haskell README](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell) to generate the Haskell code, e.g.

```bash
writeHaskell "src/gen-main/haskell" [pythonSyntaxModule, pythonLanguageModule]
```

In steps 4 and 5, you will now be able to import the generated code using something like:

```haskell
import qualified Hydra.Ext.Python.Syntax as Py
import Hydra.Ext.Python.Language
```

## Step 4: create a coder

Now that you have a model for your target language, and have defined the language constraints, you need a way of mapping native Hydra expressions into that language.
You can split this task into two parts: a coder (this step), and a serializer (step #5).

A central idea of Hydra is that the kernel should be defined once, in a Hydra DSL, and then mapped into various host languages using language-specific *coders*.
Currently, the Hydra kernel and also the language coders are written in Haskell.
See the [Haskell coder](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Ext/Haskell/Coder.hs) and the [Java coder](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Ext/Java/Coder.hs), and note that there are also many coders for [other languages](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Ext) in which we do not have Hydra implementations; even more can be found in [Hydra-Ext](https://github.com/CategoricalData/hydra/tree/main/hydra-ext/src/main/haskell/Hydra/Ext).

In terms of implementing Hydra in a new language, the purpose of creating a language coder is that all of the Hydra kernel code which is written in a Hydra DSL can be automatically ported to the new language, saving work and ensuring parity across implementations.
The coder can then also be used to port other, non-kernel code.

While step #1 might have been very time-consuming but relatively simple, step #3 is more complex.
LLMs will be of more limited usefulness here.
Your coder must operate both at the type level (mapping Hydra type expressions into equivalent class or interface definitions in the target language) and also at the term level (mapping Hydra term expressions into equivalent values, data structures, and executable code).
Writing such a coder is one of the most challenging development tasks in Hydra, and this simple guide will not attempt to break it down into a step-by-step recipe. However, some notes:
* The top-level structure of these coders conforms to a standard API; see how the coders are called from [Codegen.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Codegen.hs).
* The coder only needs to be unidirectional; for programming language coders, we are usually not interested in mapping native programs in the target language back into Hydra Core.
* There will usually be a helper object, e.g. called "namespaces" or "aliases", which is passed into each function that deals with qualified names.
* The greater the semantic distance between Hydra Core and the target language, the larger and more complex the coder will need to be. For example, Haskell is very close to Hydra Core, so the Haskell coder is just over 500 lines of code. Java is more remote, and requires more than 1500 lines of code.

## Step 5: create the serializer

Now that you have the coder and the language constraints which allow you to map Hydra Core expressions into an abstract syntax tree for your language, you need to also map the abstract syntax tree into a concrete syntax.
Compared to some of the steps above, this is one of the conceptually simplest tasks, and requires little explanation.
It will be helpful to re-use the built-in [Hydra.Ast](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/gen-main/haskell/Hydra/Ast.hs) and [Hydra.Staging.Serialization](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Staging/Serialization.hs) modules here.
See the [Haskell SerDe](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Ext/Haskell/Serde.hs) and the [Java SerDe](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Ext/Java/Serde.hs) as helpful examples.
Note that while "SerDe" stands for "serializer and deserializer", you will only need the former; we have no need to map *from* expressions in the target language, as noted above.

## Step 6: generate native sources

Now the fun part.
You have your code generation solution for the new host language, and you have your primitive functions.
You are ready to translate Hydra's kernel code and test suite from the Haskell DSL into the compile-time environment of the host language.
Examples for generating main and test sources are available on the [Hydra-Haskell README](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell), e.g. for Java:

```haskell
writeJava "../hydra-java/src/gen-main/java" mainModules
writeJava "../hydra-java/src/gen-test/java" testModules
```

Start by creating a new top-level directory for your implementation, like `hydra-python`.
Assuming that you have added a `writePython` function to `Codegen.hs` as mentioned above, you can generate the main and test code using:

```haskell
writePython "../hydra-python/src/gen-main/python" mainModules
writePython "../hydra-python/src/gen-test/python" testModules
```

Once you have generated the sources, make sure they compile.
If not, iterate on your coder and/or standard library.

## Step 7: implement standard primitives

As noted above, Hydra has a [standard library](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Lib) of primitive, or built-in functions.
There are many calls to these functions (though not all of them) in the Hydra kernel code which will be mapped into your new implementation, and you will get compile-time or runtime errors if they are not present when you attempt to compile or run the generated kernel code.
Luckily, most of them are pretty simple and straightforward.
Primitive functions may be implemented in different ways in different Hydra language variants, though their behavior must be the same across variants.
In Haskell, they are implemented [here](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Lib) (compile-time implementations) and [here](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Libraries.hs) (metadata about the primitives).
In Java, the metadata about a primitive is bundled together with its implementation [here](https://github.com/CategoricalData/hydra/tree/main/hydra-java/src/main/java/hydra/lib), and a registry of primitive functions is constructed [here](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/main/java/hydra/lib/Libraries.java).
The main requirements for primitives are that:
1. The primitives are available to be referenced by compile-time code in the host language
1. The primitives are available to be called in an interpreted environment
1. All of the required metadata, including the type signature, of each primitive can be looked up using the primitive's name.
1. The runtime behavior of the primitive conforms to Hydra's language-independent test suite (see below)

See [Graph.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Tier1/Graph.hs#L69) for the required fields: `name`, `type`, and `implementation`.
Note that while in principle, every Hydra language variant ought to implement every primitive in the standard library, currently there are small differences (a handful of primitives are not yet implemented in Java).
At a bare minimum, all of the primitives which are referenced in the Hydra kernel need to be implemented.

## Step 8: fill in the gaps

Hydra has not yet "closed the loop", i.e. not all of its kernel code is available in the DSL, and can be generated directly into your new language variant.
After Hydra does close the loop, this step will go away completely, but for now you need to be aware of the holes in the kernel sources and patch them with hand-written code as needed for your anticipated applications.
You will know what is needed, because your application will not compile and/or run if certain code is missing from the kernel.
However, this code can be written "lazily", since if you do not yet need it, it's best not to spend the effort reimplementing it in your new language, since it will eventually be generated for you.
One example is the Hydra interpreter, which is needed for running the test suite (see the next step).
You can find the interpreter in Haskell [here](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Staging/Reduction.hs) and in Java [here](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/main/java/hydra/Reduction.java).

## Step 9: create a test runner

Hydra includes a suite of test cases which are intended to be evaluating in the same way across language variants, ensuring parity.
The sources for the test suite can be found in the Haskell DSL [here](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Sources/Tier3/Test).
At this time, the coverage of the tests is quite minimal (it just tests a few list and string primitives), but it can be expected to expand in the future, covering all of the standard library as well as various kernel code such as core coders and type inference.
A Hydra implementation **must** include a runner for the test suite which executes every time unit tests are run.
You can see the test suite in Haskell [here](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/gen-test/haskell/Hydra/Test/TestSuite.hs) (generated code) and [here](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/test/haskell/Hydra/TestSuiteSpec.hs) (runner) and in Java [here](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/gen-test/java/hydra/test/testSuite/TestSuite.java) and [here](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/test/java/hydra/TestSuiteRunner.java).
The generated code should already be available (from the previous step), and you can see that the new code you need to write by hand is quite simple -- less than 100 lines of code in each language variant.

## Step 10: create native DSLs

OK, now for the *really* fun part.
This is where you depart from the script and create whatever domain-specific languages and/or additional utilities will be useful in your new language variant.
While there are a handful of more or less mandatory DSLs, language variants are allowed and expected to vary widely in the DSLs they provide, so as to make the best use of the programming constructs and/or libraries available in the particular host language.
You should probably start with these three DSLs, and then add others as desired:
* Type construction DSL: allows developers to build type-level expressions. See [Types.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Dsl/Types.hs) and [Types.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/main/java/hydra/dsl/Types.java).
* Term construction DSL: allows developers to build term-level expressions. See [Terms.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Dsl/Terms.hs) and [Terms.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/main/java/hydra/dsl/Terms.java).
* Term decoding DSL ("expect"): allows developers to decode Hydra terms to native programming constructs. See [Expect.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Dsl/Expect.hs) and [Expect.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/main/java/hydra/dsl/Expect.java).

In addition, you may find it useful to define [shorthand constructors](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Dsl/ShorthandTypes.hs) for types and/or terms, [shorthand primitives](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Dsl/Prims.hs), [phantom term constructors](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Dsl/PhantomLiterals.hs), and also individual DSLs for specific Hydra kernel modules, e.g. the [hydra/core DSL](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Dsl/Core.hs) in Haskell.
The last of these are only necessary if you plan to define new source-of-truth modules in your language variant, rather than just receiving generated code from Hydra-Haskell; see for example the [CoreDecoding source](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Tier1/CoreEncoding.hs).

## Step 11: build applications

Time to start using the new Hydra implementation, surfacing any bugs, and filling in gaps.
