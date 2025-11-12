# LLM-assisted development with Hydra

Here you will find some guidelines and resources for using Large Language Models (LLMs) effectively for generating Hydra schemas and programs.

## The Hydra lexicon

The Hydra lexicon ([`docs/hydra-lexicon.txt`](../../hydra-lexicon.txt)) is a comprehensive reference file that provides LLMs with the complete API surface of Hydra's kernel and primitive functions.
It contains:

- **Types**: All type definitions from Hydra's kernel, showing their structure
- **Terms**: Type signatures for all kernel constants and functions
- **Primitives**: Built-in primitive functions with their type signatures

The lexicon serves as a compact reference that can be included in an LLM's context window, enabling it to understand and generate correct Hydra code.

Note: most of Hydra's primitive functions are intentionally aligned with Haskell, so that LLMs familiar with Haskell can leverage that knowledge when generating Hydra code.

### Structure

The lexicon is organized into three sections:

```
Primitives:
  ...
  hydra.lib.logic.and : ((boolean → boolean → boolean))
  hydra.lib.logic.ifElse : (∀[x].(boolean → x → x → x))
  hydra.lib.logic.not : ((boolean → boolean))
  ...
  
Types:
  ...
  hydra.core.Term = union{annotated:hydra.core.AnnotatedTerm, application:hydra.core.Application, either:either<hydra.core.Term, hydra.core.Term>, function:hydra.core.Function, let:hydra.core.Let, list:list<hydra.core.Term>, literal:hydra.core.Literal, map:map<hydra.core.Term, hydra.core.Term>, maybe:maybe<hydra.core.Term>, pair:hydra.core.Term×hydra.core.Term, product:list<hydra.core.Term>, record:hydra.core.Record, set:set<hydra.core.Term>, sum:hydra.core.Sum, typeApplication:hydra.core.TypeApplicationTerm, typeLambda:hydra.core.TypeLambda, union:hydra.core.Injection, unit:unit, variable:hydra.core.Name, wrap:hydra.core.WrappedTerm}
  hydra.core.TupleProjection = record{arity:int32, index:int32, domain:maybe<list<hydra.core.Type>>}
  hydra.core.Type = union{annotated:hydra.core.AnnotatedType, application:hydra.core.ApplicationType, either:hydra.core.EitherType, forall:hydra.core.ForallType, function:hydra.core.FunctionType, list:hydra.core.Type, literal:hydra.core.LiteralType, map:hydra.core.MapType, maybe:hydra.core.Type, pair:hydra.core.PairType, product:list<hydra.core.Type>, record:hydra.core.RowType, set:hydra.core.Type, sum:list<hydra.core.Type>, union:hydra.core.RowType, unit:unit, variable:hydra.core.Name, wrap:hydra.core.WrappedType}
  ...

Terms:
  ...
  hydra.inference.freshVariableType : (∀[t0].(hydra.compute.Flow @ t0 @ hydra.core.Type))
  hydra.inference.generalize : ((hydra.typing.InferenceContext → hydra.core.Type → hydra.core.TypeScheme))
  hydra.inference.inferGraphTypes : (∀[t0].(hydra.graph.Graph → (hydra.compute.Flow @ t0 @ hydra.graph.Graph)))
  ...
```

Type definitions use `=` to show their actual structure (union, record, wrap, etc.), while terms and primitives use `:` to show their type signatures with inferred type schemes.

### Generating the lexicon

The lexicon is automatically generated from Hydra's kernel graph. To regenerate it (for example, after adding new kernel functions):

```bash
cd hydra-haskell
echo -e "import Hydra.Generation\nwriteLexiconToStandardPath" | stack ghci
```

This will update `docs/hydra-lexicon.txt` with the current kernel API.

### Using the lexicon with LLMs

When working with an LLM to generate Hydra code:

1. **Include the lexicon in your prompt**: Provide the lexicon file as context so the LLM understands the available API
2. **Reference specific modules**: Point the LLM to relevant sections (e.g., "use functions from hydra.lib.lists")
3. **Specify the source language**: Indicate whether you want to use the Haskell, Java, or Python DSLs for expressing your code
4. **Provide examples**: Show the LLM examples of the code style you want

## Property graph generation demo

### Overview

An end-to-end demonstration of LLM-assisted Hydra development is the property graph schema generation workflow, which shows how to:

1. Use an LLM to generate property graph schemas on the basis of sample data
2. Define mappings from tabular sources into the graph schema
3. Import tabular data to create a graph

### Resources

**Video walkthroughs:**

- **[Part 1: Schema Generation](https://www.linkedin.com/posts/joshuashinavier_in-case-you-were-wondering-what-i-have-been-activity-7358601538463830017-U5YE)** - Demonstrates using an LLM to generate property graph schemas in Hydra's DSL, including vertex and edge types with properties
- **[Part 2: Schema Mappings](https://www.linkedin.com/posts/joshuashinavier_here-is-part-2-of-the-hydra-property-graph-activity-7358601988755910657-HnCh)** - Shows how to generate mappings between different graph schemas, enabling data transformation and integration

**Source code:**

- **[GenPG Demo directory](https://github.com/CategoricalData/hydra/tree/main/hydra-ext/src/main/haskell/Hydra/Ext/Demos/GenPG)** - Complete implementation of the property graph generation demo
  - [Demo.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Demos/GenPG/Demo.hs) - Main entry point for running the demo
  - [ExampleGraphSchema.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Demos/GenPG/ExampleGraphSchema.hs) - Property graph schema definition
  - [ExampleDatabaseSchema.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Demos/GenPG/ExampleDatabaseSchema.hs) - Tabular source schema
  - [ExampleMapping.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Demos/GenPG/ExampleMapping.hs) - Mappings from tables to graph
  - [Transform.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Demos/GenPG/Transform.hs) - Data transformation logic

### See also

- **[Introducing Hydra](https://gdotv.com/blog/introducing-hydra/)** by Amber Lennox (G.V()) - An excellent introduction to Hydra's capabilities and design philosophy
- [Extending Hydra Core](extending-hydra-core.md) - For understanding Hydra's internal structure when generating complex extensions
- [Adding Primitives](adding-primitives.md) - When you need to add custom primitive functions that LLMs can then use

## Contributing

As LLM capabilities and best practices evolve, this guide will be updated. If you discover effective patterns or techniques for LLM-assisted Hydra development, please consider contributing examples or improvements to this documentation.
