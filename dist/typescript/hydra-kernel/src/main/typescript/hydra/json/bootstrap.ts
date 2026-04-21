// Note: this is an automatically generated file. Do not edit.

/**
 * A module which provides a minimal typing environment for decoding other modules from JSON. This avoids certain problems with generating entire source modules into target languages like Java, which is subject to method size limits for large modules like hydra.core.
 */



import * as Core from "../core.js";

export const typesByName: ReadonlyMap<Core.Name, Core.Type> = new Map([["hydra.coders.Adapter", ({ tag: "annotated", value: ({
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "forall", value: ({
    parameter: "t2",
    body: ({ tag: "forall", value: ({
    parameter: "v1",
    body: ({ tag: "forall", value: ({
    parameter: "v2",
    body: ({ tag: "record", value: [({
    name: "isLossy",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Whether information may be lost in the course of this adaptation" }) })]])
  }) })
  }), ({
    name: "source",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "t1" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The source type" }) })]])
  }) })
  }), ({
    name: "target",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "t2" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The target type" }) })]])
  }) })
  }), ({
    name: "coder",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.coders.Coder" }),
    argument: ({ tag: "variable", value: "v1" })
  }) }),
    argument: ({ tag: "variable", value: "v2" })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The coder for transforming instances of the source type to instances of the target type" }) })]])
  }) })
  })] })
  }) })
  }) })
  }) })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A two-level bidirectional encoder which adapts types to types and terms to terms" }) })]])
  }) })], ["hydra.coders.AdapterContext", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "graph",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.graph.Graph" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The underlying graph of elements and primitives" }) })]])
  }) })
  }), ({
    name: "language",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.coders.Language" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The language being encoded or decoded" }) })]])
  }) })
  }), ({
    name: "adapters",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "hydra.core.Name" }),
    values: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.coders.Adapter" }),
    argument: ({ tag: "variable", value: "hydra.core.Type" })
  }) }),
    argument: ({ tag: "variable", value: "hydra.core.Type" })
  }) }),
    argument: ({ tag: "variable", value: "hydra.core.Term" })
  }) }),
    argument: ({ tag: "variable", value: "hydra.core.Term" })
  }) })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A map of type names to adapters for those types" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An evaluation context together with a source language and a target language" }) })]])
  }) })], ["hydra.coders.Bicoder", ({ tag: "annotated", value: ({
    body: ({ tag: "forall", value: ({
    parameter: "t1",
    body: ({ tag: "forall", value: ({
    parameter: "t2",
    body: ({ tag: "forall", value: ({
    parameter: "v1",
    body: ({ tag: "forall", value: ({
    parameter: "v2",
    body: ({ tag: "record", value: [({
    name: "encode",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t1" }),
    codomain: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.coders.Adapter" }),
    argument: ({ tag: "variable", value: "t1" })
  }) }),
    argument: ({ tag: "variable", value: "t2" })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) }),
    argument: ({ tag: "variable", value: "v2" })
  }) })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A function from source types to adapters" }) })]])
  }) })
  }), ({
    name: "decode",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "t2" }),
    codomain: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.coders.Adapter" }),
    argument: ({ tag: "variable", value: "t2" })
  }) }),
    argument: ({ tag: "variable", value: "t1" })
  }) }),
    argument: ({ tag: "variable", value: "v2" })
  }) }),
    argument: ({ tag: "variable", value: "v1" })
  }) })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A function from target types to adapters" }) })]])
  }) })
  })] })
  }) })
  }) })
  }) })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A two-level encoder and decoder, operating both at a type level and an instance (data) level" }) })]])
  }) })], ["hydra.coders.Coder", ({ tag: "annotated", value: ({
    body: ({ tag: "forall", value: ({
    parameter: "v1",
    body: ({ tag: "forall", value: ({
    parameter: "v2",
    body: ({ tag: "record", value: [({
    name: "encode",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.context.Context" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "v1" }),
    codomain: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "hydra.errors.Error" }),
    right: ({ tag: "variable", value: "v2" })
  }) })
  }) })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A function which encodes source values as target values in a given context" }) })]])
  }) })
  }), ({
    name: "decode",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.context.Context" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "v2" }),
    codomain: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "hydra.errors.Error" }),
    right: ({ tag: "variable", value: "v1" })
  }) })
  }) })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A function which decodes target values as source values in a given context" }) })]])
  }) })
  })] })
  }) })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An encoder and decoder; a bidirectional transformation between two types" }) })]])
  }) })], ["hydra.coders.CoderDirection", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "encode",
    type: ({ tag: "unit" })
  }), ({
    name: "decode",
    type: ({ tag: "unit" })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Indicates either the 'out' or the 'in' direction of a coder" }) })]])
  }) })], ["hydra.coders.Language", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.coders.LanguageName" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The unique name of the language" }) })]])
  }) })
  }), ({
    name: "constraints",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.coders.LanguageConstraints" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The constraints which characterize the language" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A named language together with language-specific constraints" }) })]])
  }) })], ["hydra.coders.LanguageConstraints", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "eliminationVariants",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "set", value: ({ tag: "variable", value: "hydra.variants.EliminationVariant" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "All supported elimination variants" }) })]])
  }) })
  }), ({
    name: "literalVariants",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "set", value: ({ tag: "variable", value: "hydra.variants.LiteralVariant" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "All supported literal variants" }) })]])
  }) })
  }), ({
    name: "floatTypes",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "set", value: ({ tag: "variable", value: "hydra.core.FloatType" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "All supported float types" }) })]])
  }) })
  }), ({
    name: "functionVariants",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "set", value: ({ tag: "variable", value: "hydra.variants.FunctionVariant" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "All supported function variants" }) })]])
  }) })
  }), ({
    name: "integerTypes",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "set", value: ({ tag: "variable", value: "hydra.core.IntegerType" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "All supported integer types" }) })]])
  }) })
  }), ({
    name: "termVariants",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "set", value: ({ tag: "variable", value: "hydra.variants.TermVariant" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "All supported term variants" }) })]])
  }) })
  }), ({
    name: "typeVariants",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "set", value: ({ tag: "variable", value: "hydra.variants.TypeVariant" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "All supported type variants" }) })]])
  }) })
  }), ({
    name: "types",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.core.Type" }),
    codomain: ({ tag: "literal", value: ({ tag: "boolean" }) })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A logical set of types, as a predicate which tests a type for inclusion" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A set of constraints on valid type and term expressions, characterizing a language" }) })]])
  }) })], ["hydra.coders.LanguageName", ({ tag: "annotated", value: ({
    body: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The unique name of a language" }) })]])
  }) })], ["hydra.coders.SymmetricAdapter", ({ tag: "annotated", value: ({
    body: ({ tag: "forall", value: ({
    parameter: "t",
    body: ({ tag: "forall", value: ({
    parameter: "v",
    body: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.coders.Adapter" }),
    argument: ({ tag: "variable", value: "t" })
  }) }),
    argument: ({ tag: "variable", value: "t" })
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) }),
    argument: ({ tag: "variable", value: "v" })
  }) })
  }) })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A bidirectional encoder which maps between the same type and term languages on either side" }) })]])
  }) })], ["hydra.coders.TraversalOrder", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "pre",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Pre-order traversal" }) })]])
  }) })
  }), ({
    name: "post",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Post-order traversal" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Specifies either a pre-order or post-order traversal" }) })]])
  }) })], ["hydra.coders.TypeAdapter", ({ tag: "annotated", value: ({
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.coders.AdapterContext" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.core.Type" }),
    codomain: ({ tag: "either", value: ({
    left: ({ tag: "literal", value: ({ tag: "string" }) }),
    right: ({ tag: "application", value: ({
    function: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: "hydra.coders.SymmetricAdapter" }),
    argument: ({ tag: "variable", value: "hydra.core.Type" })
  }) }),
    argument: ({ tag: "variable", value: "hydra.core.Term" })
  }) })
  }) })
  }) })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A function which maps a Hydra type to a symmetric adapter between types and terms" }) })]])
  }) })], ["hydra.context.Context", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "trace",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A stack of context labels describing the current execution path" }) })]])
  }) })
  }), ({
    name: "messages",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A log of warnings and/or info messages" }) })]])
  }) })
  }), ({
    name: "other",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "hydra.core.Name" }),
    values: ({ tag: "variable", value: "hydra.core.Term" })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A map of string keys to arbitrary terms as values, for application-specific use" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An execution context for tracing and diagnostics, threaded through function calls" }) })]])
  }) })], ["hydra.context.InContext", ({ tag: "annotated", value: ({
    body: ({ tag: "forall", value: ({
    parameter: "e",
    body: ({ tag: "record", value: [({
    name: "object",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "e" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A domain object; typically an error" }) })]])
  }) })
  }), ({
    name: "context",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.context.Context" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The execution context at the point of capture" }) })]])
  }) })
  })] })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A particular domain object (such as an error) together with an execution context" }) })]])
  }) })], ["hydra.core.AnnotatedTerm", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "body",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Term" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The term being annotated" }) })]])
  }) })
  }), ({
    name: "annotation",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "hydra.core.Name" }),
    values: ({ tag: "variable", value: "hydra.core.Term" })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The annotation as a map from keys to values" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A term together with an annotation" }) })]])
  }) })], ["hydra.core.AnnotatedType", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "body",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type being annotated" }) })]])
  }) })
  }), ({
    name: "annotation",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "hydra.core.Name" }),
    values: ({ tag: "variable", value: "hydra.core.Term" })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The annotation as a map from keys to values" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A type together with an annotation" }) })]])
  }) })], ["hydra.core.Application", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "function",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Term" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The left-hand side of the application" }) })]])
  }) })
  }), ({
    name: "argument",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Term" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The right-hand side of the application" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A term which applies a function to an argument" }) })]])
  }) })], ["hydra.core.ApplicationType", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "function",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The left-hand side of the application" }) })]])
  }) })
  }), ({
    name: "argument",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The right-hand side of the application" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type-level analog of an application term" }) })]])
  }) })], ["hydra.core.Binding", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The name of the bound variable" }) })]])
  }) })
  }), ({
    name: "term",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Term" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The term to which the variable is bound" }) })]])
  }) })
  }), ({
    name: "type",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "maybe", value: ({ tag: "variable", value: "hydra.core.TypeScheme" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The optional type of the bound term" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A field with an optional type scheme, used to bind variables to terms in a 'let' expression" }) })]])
  }) })], ["hydra.core.CaseStatement", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "typeName",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The name of the union type" }) })]])
  }) })
  }), ({
    name: "default",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "maybe", value: ({ tag: "variable", value: "hydra.core.Term" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An optional default case, used if none of the explicit cases match" }) })]])
  }) })
  }), ({
    name: "cases",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "variable", value: "hydra.core.Field" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A list of case alternatives, one per union field" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A union elimination; a case statement" }) })]])
  }) })], ["hydra.core.EitherType", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "left",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The 'left' alternative" }) })]])
  }) })
  }), ({
    name: "right",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The 'right' alternative" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A type which provides a choice between a 'left' type and a 'right' type" }) })]])
  }) })], ["hydra.core.Field", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The name of the field" }) })]])
  }) })
  }), ({
    name: "term",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Term" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The term value of the field" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A name/term pair" }) })]])
  }) })], ["hydra.core.FieldType", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The name of the field" }) })]])
  }) })
  }), ({
    name: "type",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type of the field" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A name/type pair" }) })]])
  }) })], ["hydra.core.FloatType", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "bigfloat",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An arbitrary-precision floating-point type" }) })]])
  }) })
  }), ({
    name: "float32",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 32-bit floating-point type" }) })]])
  }) })
  }), ({
    name: "float64",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 64-bit floating-point type" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A floating-point type" }) })]])
  }) })], ["hydra.core.FloatValue", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "bigfloat",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "bigfloat" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An arbitrary-precision floating-point value" }) })]])
  }) })
  }), ({
    name: "float32",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float32" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 32-bit floating-point value" }) })]])
  }) })
  }), ({
    name: "float64",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "float", value: ({ tag: "float64" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 64-bit floating-point value" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A floating-point literal value" }) })]])
  }) })], ["hydra.core.ForallType", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "parameter",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The variable which is bound by the lambda" }) })]])
  }) })
  }), ({
    name: "body",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The body of the lambda" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A universally quantified type; the System F equivalent of a type scheme, and the type-level equivalent of a lambda term." }) })]])
  }) })], ["hydra.core.FunctionType", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "domain",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The domain (input) type of the function" }) })]])
  }) })
  }), ({
    name: "codomain",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The codomain (output) type of the function" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A function type, also known as an arrow type" }) })]])
  }) })], ["hydra.core.Injection", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "typeName",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The name of the union type" }) })]])
  }) })
  }), ({
    name: "field",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Field" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The field being injected, including its name and value" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An instance of a union type; i.e. a string-indexed generalization of inl() or inr()" }) })]])
  }) })], ["hydra.core.IntegerType", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "bigint",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An arbitrary-precision integer type" }) })]])
  }) })
  }), ({
    name: "int8",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An 8-bit signed integer type" }) })]])
  }) })
  }), ({
    name: "int16",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 16-bit signed integer type" }) })]])
  }) })
  }), ({
    name: "int32",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 32-bit signed integer type" }) })]])
  }) })
  }), ({
    name: "int64",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 64-bit signed integer type" }) })]])
  }) })
  }), ({
    name: "uint8",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An 8-bit unsigned integer type" }) })]])
  }) })
  }), ({
    name: "uint16",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 16-bit unsigned integer type" }) })]])
  }) })
  }), ({
    name: "uint32",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 32-bit unsigned integer type" }) })]])
  }) })
  }), ({
    name: "uint64",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 64-bit unsigned integer type" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An integer type" }) })]])
  }) })], ["hydra.core.IntegerValue", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "bigint",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "bigint" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An arbitrary-precision integer value" }) })]])
  }) })
  }), ({
    name: "int8",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int8" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An 8-bit signed integer value" }) })]])
  }) })
  }), ({
    name: "int16",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int16" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 16-bit signed integer value (short value)" }) })]])
  }) })
  }), ({
    name: "int32",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 32-bit signed integer value (int value)" }) })]])
  }) })
  }), ({
    name: "int64",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int64" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 64-bit signed integer value (long value)" }) })]])
  }) })
  }), ({
    name: "uint8",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint8" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An 8-bit unsigned integer value (byte)" }) })]])
  }) })
  }), ({
    name: "uint16",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint16" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 16-bit unsigned integer value" }) })]])
  }) })
  }), ({
    name: "uint32",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint32" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 32-bit unsigned integer value (unsigned int)" }) })]])
  }) })
  }), ({
    name: "uint64",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "uint64" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 64-bit unsigned integer value (unsigned long)" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An integer literal value" }) })]])
  }) })], ["hydra.core.Lambda", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "parameter",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The parameter of the lambda" }) })]])
  }) })
  }), ({
    name: "domain",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "maybe", value: ({ tag: "variable", value: "hydra.core.Type" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An optional domain type for the lambda" }) })]])
  }) })
  }), ({
    name: "body",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Term" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The body of the lambda" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A function abstraction (lambda)" }) })]])
  }) })], ["hydra.core.Let", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "bindings",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "variable", value: "hydra.core.Binding" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The list of variable bindings" }) })]])
  }) })
  }), ({
    name: "body",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Term" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The body term in which the variables are bound" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A set of (possibly recursive) 'let' bindings together with a body in which they are bound" }) })]])
  }) })], ["hydra.core.Literal", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "binary",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "binary" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A binary literal" }) })]])
  }) })
  }), ({
    name: "boolean",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "boolean" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A boolean literal" }) })]])
  }) })
  }), ({
    name: "float",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.FloatValue" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A floating-point literal" }) })]])
  }) })
  }), ({
    name: "integer",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.IntegerValue" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An integer literal" }) })]])
  }) })
  }), ({
    name: "string",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "string" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A string literal" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A term constant; an instance of a literal type" }) })]])
  }) })], ["hydra.core.LiteralType", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "binary",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type of a binary (byte string) value" }) })]])
  }) })
  }), ({
    name: "boolean",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type of a boolean (true/false) value" }) })]])
  }) })
  }), ({
    name: "float",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.FloatType" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type of a floating-point value" }) })]])
  }) })
  }), ({
    name: "integer",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.IntegerType" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type of an integer value" }) })]])
  }) })
  }), ({
    name: "string",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type of a string value" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants" }) })]])
  }) })], ["hydra.core.MapType", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "keys",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type of keys in the map" }) })]])
  }) })
  }), ({
    name: "values",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type of values in the map" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A map type" }) })]])
  }) })], ["hydra.core.Name", ({ tag: "annotated", value: ({
    body: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A unique identifier in some context; a string-valued key" }) })]])
  }) })], ["hydra.core.PairType", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "first",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The first component of the pair" }) })]])
  }) })
  }), ({
    name: "second",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The second component of the pair" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A type which pairs a 'first' type and a 'second' type" }) })]])
  }) })], ["hydra.core.Projection", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "typeName",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The name of the record type" }) })]])
  }) })
  }), ({
    name: "field",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The name of the projected field" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A record elimination; a projection" }) })]])
  }) })], ["hydra.core.Record", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "typeName",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The name of the record type" }) })]])
  }) })
  }), ({
    name: "fields",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "variable", value: "hydra.core.Field" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The fields of the record, as a list of name/term pairs" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A record, or labeled tuple; a map of field names to terms" }) })]])
  }) })], ["hydra.core.Term", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "annotated",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.AnnotatedTerm" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A term annotated with metadata" }) })]])
  }) })
  }), ({
    name: "application",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Application" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A function application" }) })]])
  }) })
  }), ({
    name: "cases",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.CaseStatement" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A union elimination; a case statement" }) })]])
  }) })
  }), ({
    name: "either",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "hydra.core.Term" }),
    right: ({ tag: "variable", value: "hydra.core.Term" })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An either value" }) })]])
  }) })
  }), ({
    name: "inject",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Injection" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An injection; an instance of a union type" }) })]])
  }) })
  }), ({
    name: "lambda",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Lambda" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A function abstraction (lambda)" }) })]])
  }) })
  }), ({
    name: "let",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Let" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A 'let' term, which binds variables to terms" }) })]])
  }) })
  }), ({
    name: "list",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "variable", value: "hydra.core.Term" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A list" }) })]])
  }) })
  }), ({
    name: "literal",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Literal" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A literal value" }) })]])
  }) })
  }), ({
    name: "map",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "hydra.core.Term" }),
    values: ({ tag: "variable", value: "hydra.core.Term" })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A map of keys to values" }) })]])
  }) })
  }), ({
    name: "maybe",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "maybe", value: ({ tag: "variable", value: "hydra.core.Term" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An optional value" }) })]])
  }) })
  }), ({
    name: "pair",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "hydra.core.Term" }),
    second: ({ tag: "variable", value: "hydra.core.Term" })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A pair (2-tuple)" }) })]])
  }) })
  }), ({
    name: "project",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Projection" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A record elimination; a projection" }) })]])
  }) })
  }), ({
    name: "record",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Record" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A record term" }) })]])
  }) })
  }), ({
    name: "set",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "set", value: ({ tag: "variable", value: "hydra.core.Term" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A set of values" }) })]])
  }) })
  }), ({
    name: "typeApplication",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.TypeApplicationTerm" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A System F type application term" }) })]])
  }) })
  }), ({
    name: "typeLambda",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.TypeLambda" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A System F type abstraction term" }) })]])
  }) })
  }), ({
    name: "unit",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A unit value; a term with no value" }) })]])
  }) })
  }), ({
    name: "unwrap",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An unwrap elimination; the inverse of a wrap" }) })]])
  }) })
  }), ({
    name: "variable",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A variable reference" }) })]])
  }) })
  }), ({
    name: "wrap",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.WrappedTerm" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A wrapped term; an instance of a wrapper type (newtype)" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A data term" }) })]])
  }) })], ["hydra.core.Type", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "annotated",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.AnnotatedType" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An annotated type" }) })]])
  }) })
  }), ({
    name: "application",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.ApplicationType" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A type application" }) })]])
  }) })
  }), ({
    name: "either",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.EitherType" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An either (sum) type" }) })]])
  }) })
  }), ({
    name: "forall",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.ForallType" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A universally quantified (polymorphic) type" }) })]])
  }) })
  }), ({
    name: "function",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.FunctionType" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A function type" }) })]])
  }) })
  }), ({
    name: "list",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A list type" }) })]])
  }) })
  }), ({
    name: "literal",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.LiteralType" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A literal type" }) })]])
  }) })
  }), ({
    name: "map",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.MapType" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A map type" }) })]])
  }) })
  }), ({
    name: "maybe",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An optional type" }) })]])
  }) })
  }), ({
    name: "pair",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.PairType" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A pair (2-tuple) type" }) })]])
  }) })
  }), ({
    name: "record",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "variable", value: "hydra.core.FieldType" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A record type" }) })]])
  }) })
  }), ({
    name: "set",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A set type" }) })]])
  }) })
  }), ({
    name: "union",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "variable", value: "hydra.core.FieldType" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A union type with field names" }) })]])
  }) })
  }), ({
    name: "unit",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The unit type" }) })]])
  }) })
  }), ({
    name: "variable",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A type variable" }) })]])
  }) })
  }), ({
    name: "void",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The void (uninhabited, or bottom) type" }) })]])
  }) })
  }), ({
    name: "wrap",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A wrapped type (newtype)" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A data type" }) })]])
  }) })], ["hydra.core.TypeApplicationTerm", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "body",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Term" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The term being applied to a type" }) })]])
  }) })
  }), ({
    name: "type",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type argument" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A term applied to a type; a type application" }) })]])
  }) })], ["hydra.core.TypeLambda", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "parameter",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type variable introduced by the abstraction" }) })]])
  }) })
  }), ({
    name: "body",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Term" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The body of the abstraction" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A System F type abstraction term" }) })]])
  }) })], ["hydra.core.TypeScheme", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "variables",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "variable", value: "hydra.core.Name" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The free type variables" }) })]])
  }) })
  }), ({
    name: "type",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type expression" }) })]])
  }) })
  }), ({
    name: "constraints",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "maybe", value: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "hydra.core.Name" }),
    values: ({ tag: "variable", value: "hydra.core.TypeVariableMetadata" })
  }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Optional metadata for type variables, including typeclass constraints. The map keys are type variable names." }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A type expression together with free type variables occurring in the expression" }) })]])
  }) })], ["hydra.core.TypeVariableMetadata", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "classes",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "set", value: ({ tag: "variable", value: "hydra.core.Name" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The set of typeclass constraints on this type variable" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Metadata associated with a type variable, including typeclass constraints" }) })]])
  }) })], ["hydra.core.WrappedTerm", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "typeName",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The name of the wrapper type" }) })]])
  }) })
  }), ({
    name: "body",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Term" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The wrapped term" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A term wrapped in a type name" }) })]])
  }) })], ["hydra.errors.DecodingError", ({ tag: "annotated", value: ({
    body: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An error that occurred during decoding of a term" }) })]])
  }) })], ["hydra.errors.EmptyListError", ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An empty list was encountered where a non-empty list was required" }) })]])
  }) })], ["hydra.errors.Error", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "checking",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.error.checking.CheckingError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A type checking error" }) })]])
  }) })
  }), ({
    name: "decoding",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.DecodingError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An error that occurred during decoding of a term" }) })]])
  }) })
  }), ({
    name: "duplicateBinding",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.error.core.DuplicateBindingError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A duplicate binding name error" }) })]])
  }) })
  }), ({
    name: "duplicateField",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.error.core.DuplicateFieldError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A duplicate field name error" }) })]])
  }) })
  }), ({
    name: "extraction",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.ExtractionError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An error that occurred while extracting a value from a term" }) })]])
  }) })
  }), ({
    name: "inference",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.InferenceError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A type inference error" }) })]])
  }) })
  }), ({
    name: "other",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.OtherError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Any other error" }) })]])
  }) })
  }), ({
    name: "resolution",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.ResolutionError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A name-resolution error" }) })]])
  }) })
  }), ({
    name: "undefinedField",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.error.core.UndefinedFieldError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A reference to an undefined field" }) })]])
  }) })
  }), ({
    name: "undefinedTermVariable",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.error.core.UndefinedTermVariableError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A reference to an undefined term variable" }) })]])
  }) })
  }), ({
    name: "untypedTermVariable",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.error.core.UntypedTermVariableError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A term variable whose type is not known" }) })]])
  }) })
  }), ({
    name: "unexpectedTermVariant",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.error.core.UnexpectedTermVariantError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An unexpected term variant" }) })]])
  }) })
  }), ({
    name: "unexpectedTypeVariant",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.error.core.UnexpectedTypeVariantError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An unexpected type variant" }) })]])
  }) })
  }), ({
    name: "unification",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.UnificationError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A type unification error" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An error of any kind, with kernel errors particularly differentiated" }) })]])
  }) })], ["hydra.errors.ExtractionError", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "emptyList",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.EmptyListError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An empty list was encountered where a non-empty list was required" }) })]])
  }) })
  }), ({
    name: "multipleBindings",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.MultipleBindingsError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Multiple let bindings were found with the same name" }) })]])
  }) })
  }), ({
    name: "multipleFields",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.MultipleFieldsError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Multiple record fields were found with the same field name" }) })]])
  }) })
  }), ({
    name: "noMatchingField",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.NoMatchingFieldError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "No field with the expected name was found in a record" }) })]])
  }) })
  }), ({
    name: "noSuchBinding",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.NoSuchBindingError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "No let binding with the expected name was found" }) })]])
  }) })
  }), ({
    name: "notEnoughCases",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.NotEnoughCasesError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A case statement did not contain enough cases to match the target" }) })]])
  }) })
  }), ({
    name: "unexpectedShape",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.UnexpectedShapeError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A term, type, literal, or other value had an unexpected shape" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An error that occurred while extracting a typed value from a term" }) })]])
  }) })], ["hydra.errors.InferenceError", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "checking",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.error.checking.CheckingError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A type checking error encountered during inference" }) })]])
  }) })
  }), ({
    name: "other",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.OtherInferenceError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A generic inference error carrying a message and a subterm path. Placeholder arm; sites should migrate to typed variants." }) })]])
  }) })
  }), ({
    name: "unification",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.UnificationInferenceError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A unification failure encountered while inferring types" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An error that occurred during type inference" }) })]])
  }) })], ["hydra.errors.MultipleBindingsError", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The binding name which was duplicated" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Multiple let bindings with the same name were found" }) })]])
  }) })], ["hydra.errors.MultipleFieldsError", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "fieldName",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The field name which appeared more than once" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Multiple fields with the same name were found in a record" }) })]])
  }) })], ["hydra.errors.NoMatchingFieldError", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "fieldName",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The field name which was not found" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "No field with the expected name was present" }) })]])
  }) })], ["hydra.errors.NoSuchBindingError", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The binding name which was not found" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "No let binding with the expected name was present" }) })]])
  }) })], ["hydra.errors.NoSuchPrimitiveError", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The primitive name which was not found" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "No primitive function with the expected name was registered in the graph" }) })]])
  }) })], ["hydra.errors.NotEnoughCasesError", ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A case statement was missing a case for the requested variant" }) })]])
  }) })], ["hydra.errors.OtherError", ({ tag: "annotated", value: ({
    body: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Any other error" }) })]])
  }) })], ["hydra.errors.OtherInferenceError", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "path",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.paths.SubtermPath" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The subterm path at which the error was observed" }) })]])
  }) })
  }), ({
    name: "message",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "string" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A human-readable error message" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A generic inference error: message + subterm path" }) })]])
  }) })], ["hydra.errors.OtherResolutionError", ({ tag: "annotated", value: ({
    body: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A generic resolution error: message" }) })]])
  }) })], ["hydra.errors.ResolutionError", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "noSuchBinding",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.NoSuchBindingError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "No binding with the expected name was found in the graph" }) })]])
  }) })
  }), ({
    name: "noSuchPrimitive",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.NoSuchPrimitiveError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "No primitive function with the expected name was found in the graph" }) })]])
  }) })
  }), ({
    name: "noMatchingField",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.NoMatchingFieldError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "No field with the expected name was present in a record or case statement" }) })]])
  }) })
  }), ({
    name: "other",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.OtherResolutionError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A generic resolution error carrying a message" }) })]])
  }) })
  }), ({
    name: "unexpectedShape",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.UnexpectedShapeError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A term had a shape other than the one expected (e.g. a record, an injection)" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An error that occurred while resolving a name, primitive, or record/union shape in a graph" }) })]])
  }) })], ["hydra.errors.UnexpectedShapeError", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "expected",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "string" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A description of the expected shape" }) })]])
  }) })
  }), ({
    name: "actual",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "string" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A description of the shape actually encountered" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A term, type, literal, or related value had a shape other than the one expected" }) })]])
  }) })], ["hydra.errors.UnificationError", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "leftType",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The left-hand type in the unification" }) })]])
  }) })
  }), ({
    name: "rightType",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The right-hand type in the unification" }) })]])
  }) })
  }), ({
    name: "message",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "string" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A human-readable error message" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An error that occurred during type unification" }) })]])
  }) })], ["hydra.errors.UnificationInferenceError", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "path",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.paths.SubtermPath" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The subterm path at which the unification failure was observed" }) })]])
  }) })
  }), ({
    name: "cause",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.errors.UnificationError" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The underlying unification error" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A unification failure at a specific subterm locus during inference" }) })]])
  }) })], ["hydra.graph.Graph", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "boundTerms",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "hydra.core.Name" }),
    values: ({ tag: "variable", value: "hydra.core.Term" })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The terms bound by all term variables in scope" }) })]])
  }) })
  }), ({
    name: "boundTypes",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "hydra.core.Name" }),
    values: ({ tag: "variable", value: "hydra.core.TypeScheme" })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type schemes of all term variables in scope" }) })]])
  }) })
  }), ({
    name: "classConstraints",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "hydra.core.Name" }),
    values: ({ tag: "variable", value: "hydra.core.TypeVariableMetadata" })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A mutable map from type variable names to their accumulated class constraints. This is populated during type inference when operations requiring Eq or Ord are encountered." }) })]])
  }) })
  }), ({
    name: "lambdaVariables",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "set", value: ({ tag: "variable", value: "hydra.core.Name" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The set of term variables introduced by specifically by lambdas" }) })]])
  }) })
  }), ({
    name: "metadata",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "hydra.core.Name" }),
    values: ({ tag: "variable", value: "hydra.core.Term" })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Any additional metadata bound to term variables in scope" }) })]])
  }) })
  }), ({
    name: "primitives",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "hydra.core.Name" }),
    values: ({ tag: "variable", value: "hydra.graph.Primitive" })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "All primitive functions and constants by name" }) })]])
  }) })
  }), ({
    name: "schemaTypes",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "hydra.core.Name" }),
    values: ({ tag: "variable", value: "hydra.core.TypeScheme" })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "All schema types (type schemes) in scope" }) })]])
  }) })
  }), ({
    name: "typeVariables",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "set", value: ({ tag: "variable", value: "hydra.core.Name" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The set of type variables introduced specifically by type lambdas" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A graph, or lexical environment which binds names to terms, types, primitives, and metadata" }) })]])
  }) })], ["hydra.graph.Primitive", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The unique name of the primitive function" }) })]])
  }) })
  }), ({
    name: "type",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.TypeScheme" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type signature of the primitive function" }) })]])
  }) })
  }), ({
    name: "implementation",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.context.Context" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.graph.Graph" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "list", value: ({ tag: "variable", value: "hydra.core.Term" }) }),
    codomain: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "hydra.errors.Error" }),
    right: ({ tag: "variable", value: "hydra.core.Term" })
  }) })
  }) })
  }) })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A concrete implementation of the primitive function. The Context and Graph parameters are needed by higher-order primitives (e.g. lists.map, lists.foldl, eithers.bind) which must evaluate function arguments via term reduction; the Graph provides variable and primitive bindings, while the Context supports tracing and error reporting." }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A built-in function or constant" }) })]])
  }) })], ["hydra.graph.TermCoder", ({ tag: "annotated", value: ({
    body: ({ tag: "forall", value: ({
    parameter: "a",
    body: ({ tag: "record", value: [({
    name: "type",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Type" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The Hydra type of encoded terms" }) })]])
  }) })
  }), ({
    name: "encode",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.context.Context" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.graph.Graph" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.core.Term" }),
    codomain: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "hydra.errors.Error" }),
    right: ({ tag: "variable", value: "a" })
  }) })
  }) })
  }) })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An encode function from terms to native values" }) })]])
  }) })
  }), ({
    name: "decode",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "hydra.context.Context" }),
    codomain: ({ tag: "function", value: ({
    domain: ({ tag: "variable", value: "a" }),
    codomain: ({ tag: "either", value: ({
    left: ({ tag: "variable", value: "hydra.errors.Error" }),
    right: ({ tag: "variable", value: "hydra.core.Term" })
  }) })
  }) })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A decode function from native values to terms" }) })]])
  }) })
  })] })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms." }) })]])
  }) })], ["hydra.packaging.Definition", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "term",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.packaging.TermDefinition" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A term definition" }) })]])
  }) })
  }), ({
    name: "type",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.packaging.TypeDefinition" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A type definition" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A definition, which may be either a term or type definition" }) })]])
  }) })], ["hydra.packaging.FileExtension", ({ tag: "annotated", value: ({
    body: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A file extension (without the dot), e.g. \"json\" or \"py\"" }) })]])
  }) })], ["hydra.packaging.Library", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "namespace",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.packaging.Namespace" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A common prefix for all primitive function names in the library" }) })]])
  }) })
  }), ({
    name: "prefix",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "string" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A preferred namespace prefix for function names in the library" }) })]])
  }) })
  }), ({
    name: "primitives",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "variable", value: "hydra.graph.Primitive" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The primitives defined in this library" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A library of primitive functions" }) })]])
  }) })], ["hydra.packaging.Module", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "namespace",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.packaging.Namespace" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A common prefix for all element names in the module" }) })]])
  }) })
  }), ({
    name: "definitions",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "variable", value: "hydra.packaging.Definition" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The definitions in this module" }) })]])
  }) })
  }), ({
    name: "termDependencies",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "variable", value: "hydra.packaging.Namespace" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Any modules which the term expressions of this module directly depend upon" }) })]])
  }) })
  }), ({
    name: "typeDependencies",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "variable", value: "hydra.packaging.Namespace" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Any modules which the type expressions of this module directly depend upon" }) })]])
  }) })
  }), ({
    name: "description",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An optional human-readable description of the module" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A logical collection of elements in the same namespace, having dependencies on zero or more other modules" }) })]])
  }) })], ["hydra.packaging.Namespace", ({ tag: "annotated", value: ({
    body: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A prefix for element names" }) })]])
  }) })], ["hydra.packaging.Namespaces", ({ tag: "annotated", value: ({
    body: ({ tag: "forall", value: ({
    parameter: "n",
    body: ({ tag: "record", value: [({
    name: "focus",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "pair", value: ({
    first: ({ tag: "variable", value: "hydra.packaging.Namespace" }),
    second: ({ tag: "variable", value: "n" })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The namespace in focus, together with its associated value" }) })]])
  }) })
  }), ({
    name: "mapping",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "map", value: ({
    keys: ({ tag: "variable", value: "hydra.packaging.Namespace" }),
    values: ({ tag: "variable", value: "n" })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A mapping of namespaces to values" }) })]])
  }) })
  })] })
  }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A mapping from namespaces to values of type n, with a focus on one namespace" }) })]])
  }) })], ["hydra.packaging.Package", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.packaging.PackageName" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The name of the package" }) })]])
  }) })
  }), ({
    name: "modules",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "variable", value: "hydra.packaging.Module" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The modules in this package" }) })]])
  }) })
  }), ({
    name: "dependencies",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "list", value: ({ tag: "variable", value: "hydra.packaging.PackageName" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The packages which this package depends on" }) })]])
  }) })
  }), ({
    name: "description",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "maybe", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An optional human-readable description of the package" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A package, which is a named collection of modules with metadata and dependencies" }) })]])
  }) })], ["hydra.packaging.PackageName", ({ tag: "annotated", value: ({
    body: ({ tag: "wrap", value: ({ tag: "literal", value: ({ tag: "string" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The unique name of a package, e.g. \"hydra-kernel\" or \"hydra-python\"" }) })]])
  }) })], ["hydra.packaging.QualifiedName", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "namespace",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "maybe", value: ({ tag: "variable", value: "hydra.packaging.Namespace" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The optional namespace" }) })]])
  }) })
  }), ({
    name: "local",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "string" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The local name" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A qualified name consisting of an optional namespace together with a mandatory local name" }) })]])
  }) })], ["hydra.packaging.TermDefinition", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The name of the term" }) })]])
  }) })
  }), ({
    name: "term",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Term" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The term being defined" }) })]])
  }) })
  }), ({
    name: "type",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "maybe", value: ({ tag: "variable", value: "hydra.core.TypeScheme" }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type scheme of the term, including any class constraints" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A term-level definition, including a name, a term, and the type scheme of the term" }) })]])
  }) })], ["hydra.packaging.TypeDefinition", ({ tag: "annotated", value: ({
    body: ({ tag: "record", value: [({
    name: "name",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.Name" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The name of the type" }) })]])
  }) })
  }), ({
    name: "type",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "variable", value: "hydra.core.TypeScheme" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "The type scheme being defined" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A type-level definition, including a name and the type scheme" }) })]])
  }) })], ["hydra.util.CaseConvention", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "camel",
    type: ({ tag: "unit" })
  }), ({
    name: "pascal",
    type: ({ tag: "unit" })
  }), ({
    name: "lowerSnake",
    type: ({ tag: "unit" })
  }), ({
    name: "upperSnake",
    type: ({ tag: "unit" })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "A naming convention for symbols, such as camelCase or snake_case" }) })]])
  }) })], ["hydra.util.Comparison", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "lessThan",
    type: ({ tag: "unit" })
  }), ({
    name: "equalTo",
    type: ({ tag: "unit" })
  }), ({
    name: "greaterThan",
    type: ({ tag: "unit" })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "An equality judgement: less than, equal to, or greater than" }) })]])
  }) })], ["hydra.util.Precision", ({ tag: "annotated", value: ({
    body: ({ tag: "union", value: [({
    name: "arbitrary",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "unit" }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Arbitrary precision" }) })]])
  }) })
  }), ({
    name: "bits",
    type: ({ tag: "annotated", value: ({
    body: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32" }) }) }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Precision to a specified number of bits" }) })]])
  }) })
  })] }),
    annotation: new Map([["description", ({ tag: "literal", value: ({ tag: "string", value: "Numeric precision: arbitrary precision, or precision to a specified number of bits" }) })]])
  }) })]]);
