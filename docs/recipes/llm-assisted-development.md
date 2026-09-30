# LLM-assisted development with Hydra

Here you will find some guidelines and resources for using Large Language Models (LLMs)
effectively for generating Hydra schemas and programs.

## The Hydra lexicon

The Hydra lexicon ([`docs/hydra-lexicon.txt`](../hydra-lexicon.txt))
is a comprehensive reference file that provides LLMs with the complete API surface of Hydra's kernel and primitive
functions.
It contains:

- **Types**: All type definitions from Hydra's kernel, showing their structure
- **Terms**: Type schemes for all kernel constants and functions
- **Primitives**: Built-in primitive functions with their type signatures

The lexicon serves as a compact reference that can be included in an LLM's context window,
enabling it to understand and generate correct Hydra code.

Note: most of Hydra's primitive functions are intentionally aligned with Haskell,
so that LLMs familiar with Haskell can use that knowledge when generating Hydra code.

### Structure

The lexicon is organized into three sections:

```
Primitives:
  ...
  hydra.core.lib.logic.and : ((boolean → boolean → boolean))
  hydra.core.lib.logic.ifElse : (forall x. (boolean → x → x → x))
  hydra.core.lib.logic.not : ((boolean → boolean))
  ...
  
Types:
  ...
  hydra.core.model.Term = union{annotated:hydra.core.model.AnnotatedTerm, application:hydra.core.model.Application, cases:hydra.core.model.CaseStatement, either:either<hydra.core.model.Term, hydra.core.model.Term>, inject:hydra.core.model.Injection, lambda:hydra.core.model.Lambda, let:hydra.core.model.Let, list:list<hydra.core.model.Term>, literal:hydra.core.model.Literal, map:map<hydra.core.model.Term, hydra.core.model.Term>, optional:optional<hydra.core.model.Term>, pair:(hydra.core.model.Term, hydra.core.model.Term), project:hydra.core.model.Projection, record:hydra.core.model.Record, set:set<hydra.core.model.Term>, typeApplication:hydra.core.model.TypeApplicationTerm, typeLambda:hydra.core.model.TypeLambda, unit:unit, unwrap:hydra.core.model.Name, variable:hydra.core.model.Name, wrap:hydra.core.model.WrappedTerm}
  hydra.core.model.Type = union{annotated:hydra.core.model.AnnotatedType, application:hydra.core.model.ApplicationType, effect:hydra.core.model.Type, either:hydra.core.model.EitherType, forall:hydra.core.model.ForallType, function:hydra.core.model.FunctionType, list:hydra.core.model.Type, literal:hydra.core.model.LiteralType, map:hydra.core.model.MapType, optional:hydra.core.model.Type, pair:hydra.core.model.PairType, record:list<hydra.core.model.FieldType>, set:hydra.core.model.Type, union:list<hydra.core.model.FieldType>, unit:unit, variable:hydra.core.model.Name, void:unit, wrap:hydra.core.model.Type}
  hydra.core.model.TypeScheme = record{variables:list<hydra.core.model.Name>, body:hydra.core.model.Type, constraints:map<hydra.core.model.Name, hydra.core.model.TypeVariableConstraints>}
  ...

Terms:
  ...
  hydra.core.inference.freshVariableType : ((hydra.core.typing.InferenceContext → (hydra.core.model.Type, hydra.core.typing.InferenceContext)))
  hydra.core.inference.generalize : ((hydra.core.graph.Graph → hydra.core.model.Type → hydra.core.model.TypeScheme))
  hydra.core.inference.inferGraphTypes : ((hydra.core.typing.InferenceContext → list<hydra.core.model.Binding> → hydra.core.graph.Graph → either<hydra.core.errors.Error, ((hydra.core.graph.Graph, list<hydra.core.model.Binding>), hydra.core.typing.InferenceContext)>))
  ...
```

Type definitions use `=` to show their actual structure (union, record, wrap, etc.),
while terms use `:` to show their inferred type schemes, and primitives show their type signatures.

### Generating the lexicon

The lexicon is generated from Hydra's kernel graph on demand.
To regenerate it (for example, after adding new kernel functions):

```bash
bin/regenerate-lexicon.sh
```

This will update `docs/hydra-lexicon.txt` with the current kernel API.
The `/lexicon` shorthand also runs this script. Lexicon regeneration is
deliberately decoupled from the regular sync flow (it takes ~4 minutes and
is not consumed by any build step); run it on demand or as part of the
pre-release preparation flow (`bin/prepare-release.sh`).

### Using the lexicon with LLMs

When working with an LLM to generate Hydra code:

1. **Include the lexicon in your prompt**: Provide the lexicon file as context so the LLM understands the available API
2. **Reference specific modules**: Point the LLM to relevant sections (e.g., "use functions from hydra.core.lib.lists")
3. **Specify the source language**: Indicate whether you want to use the Haskell, Java,
   or Python DSLs for expressing your code
4. **Provide examples**: Show the LLM examples of the code style you want

## Property graph generation demo

### Overview

An end-to-end demonstration of LLM-assisted Hydra development is the property graph schema generation workflow,
which shows how to:

1. Use an LLM to generate property graph schemas on the basis of sample data
2. Define mappings from tabular sources into the graph schema
3. Import tabular data to create a graph

### Resources

**Video walkthroughs:**

- **[Part 1: Schema Generation](https://www.linkedin.com/posts/joshuashinavier_in-case-you-were-wondering-what-i-have-been-activity-7358601538463830017-U5YE)** -
  Demonstrates using an LLM to generate property graph schemas in Hydra's DSL,
  including vertex and edge types with properties
- **[Part 2: Schema Mappings](https://www.linkedin.com/posts/joshuashinavier_here-is-part-2-of-the-hydra-property-graph-activity-7358601988755910657-HnCh)** -
  Shows how to generate mappings between different graph schemas,
  enabling data transformation and integration

**Source code:**

- **[GenPG Demo directory](https://github.com/CategoricalData/hydra/tree/main/demos/src/main/haskell/Hydra/Demos/Genpg)** -
  Complete implementation of the property graph generation demo
  - [Demo.hs](https://github.com/CategoricalData/hydra/blob/main/demos/src/main/haskell/Hydra/Demos/Genpg/Demo.hs) -
    Main entry point for running the demo
  - [ExampleGraphSchema.hs](https://github.com/CategoricalData/hydra/blob/main/demos/src/main/haskell/Hydra/Demos/Genpg/ExampleGraphSchema.hs) -
    Property graph schema definition
  - [ExampleDatabaseSchema.hs](https://github.com/CategoricalData/hydra/blob/main/demos/src/main/haskell/Hydra/Demos/Genpg/ExampleDatabaseSchema.hs) -
    Tabular source schema
  - [ExampleMapping.hs](https://github.com/CategoricalData/hydra/blob/main/demos/src/main/haskell/Hydra/Demos/Genpg/ExampleMapping.hs) -
    Mappings from tables to graph
  - [Transform.hs](https://github.com/CategoricalData/hydra/blob/main/demos/src/main/haskell/Hydra/Demos/Genpg/Transform.hs) -
    Data transformation logic

### See also

- **[Introducing Hydra](https://gdotv.com/blog/introducing-hydra/)** by Amber Lennox (G.V())
  - An excellent introduction to Hydra's capabilities and design philosophy
- [Extending Hydra Core](extending-hydra-core.md)
  - For understanding Hydra's internal structure when generating complex extensions
- [Adding Primitives](adding-primitives.md) - When you need to add custom primitive functions that LLMs can then use

## Contributing

As LLM capabilities and best practices evolve, this guide will be updated.
If you discover effective patterns or techniques for LLM-assisted Hydra development,
please consider contributing examples or improvements to this documentation.
