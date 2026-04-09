(module
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.json.bootstrap.types_by_name" (func $hydra.json.bootstrap.types_by_name) )
  (func $hydra.json.bootstrap.types_by_name (result i32)
  i32.const 68
  ;; map entries follow
  i32.const 0 ;; string: "hydra.coders.Adapter"
  i32.const 0 ;; string: "t1"
  i32.const 0 ;; string: "t2"
  i32.const 0 ;; string: "v1"
  i32.const 0 ;; string: "v2"
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "isLossy"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Whether information may be lost in the course of this adaptation"
  i32.const 0 ;; string: "source"
  i32.const 0 ;; string: "t1"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The source type"
  i32.const 0 ;; string: "target"
  i32.const 0 ;; string: "t2"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The target type"
  i32.const 0 ;; string: "coder"
  i32.const 0 ;; string: "hydra.coders.Coder"
  i32.const 0 ;; string: "v1"
  i32.const 0 ;; string: "v2"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The coder for transforming instances of the source type to instances of the target type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A two-level bidirectional encoder which adapts types to types and terms to terms"
  i32.const 0 ;; string: "hydra.coders.AdapterContext"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "graph"
  i32.const 0 ;; string: "hydra.graph.Graph"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The underlying graph of elements and primitives"
  i32.const 0 ;; string: "language"
  i32.const 0 ;; string: "hydra.coders.Language"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The language being encoded or decoded"
  i32.const 0 ;; string: "adapters"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 0 ;; string: "hydra.coders.Adapter"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A map of type names to adapters for those types"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An evaluation context together with a source language and a target language"
  i32.const 0 ;; string: "hydra.coders.Bicoder"
  i32.const 0 ;; string: "t1"
  i32.const 0 ;; string: "t2"
  i32.const 0 ;; string: "v1"
  i32.const 0 ;; string: "v2"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "encode"
  i32.const 0 ;; string: "t1"
  i32.const 0 ;; string: "hydra.coders.Adapter"
  i32.const 0 ;; string: "t1"
  i32.const 0 ;; string: "t2"
  i32.const 0 ;; string: "v1"
  i32.const 0 ;; string: "v2"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A function from source types to adapters"
  i32.const 0 ;; string: "decode"
  i32.const 0 ;; string: "t2"
  i32.const 0 ;; string: "hydra.coders.Adapter"
  i32.const 0 ;; string: "t2"
  i32.const 0 ;; string: "t1"
  i32.const 0 ;; string: "v2"
  i32.const 0 ;; string: "v1"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A function from target types to adapters"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A two-level encoder and decoder, operating both at a type level and an instance (data) level"
  i32.const 0 ;; string: "hydra.coders.Coder"
  i32.const 0 ;; string: "v1"
  i32.const 0 ;; string: "v2"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "encode"
  i32.const 0 ;; string: "hydra.context.Context"
  i32.const 0 ;; string: "v1"
  i32.const 0 ;; string: "hydra.context.InContext"
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "v2"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A function which encodes source values as target values in a given context"
  i32.const 0 ;; string: "decode"
  i32.const 0 ;; string: "hydra.context.Context"
  i32.const 0 ;; string: "v2"
  i32.const 0 ;; string: "hydra.context.InContext"
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "v1"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A function which decodes target values as source values in a given context"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An encoder and decoder; a bidirectional transformation between two types"
  i32.const 0 ;; string: "hydra.coders.CoderDirection"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "encode"
  i32.const 0
  i32.const 0 ;; string: "decode"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Indicates either the 'out' or the 'in' direction of a coder"
  i32.const 0 ;; string: "hydra.coders.Language"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "name"
  i32.const 0 ;; string: "hydra.coders.LanguageName"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The unique name of the language"
  i32.const 0 ;; string: "constraints"
  i32.const 0 ;; string: "hydra.coders.LanguageConstraints"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The constraints which characterize the language"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A named language together with language-specific constraints"
  i32.const 0 ;; string: "hydra.coders.LanguageConstraints"
  i32.const 8
  ;; list elements follow
  i32.const 0 ;; string: "eliminationVariants"
  i32.const 0 ;; string: "hydra.variants.EliminationVariant"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "All supported elimination variants"
  i32.const 0 ;; string: "literalVariants"
  i32.const 0 ;; string: "hydra.variants.LiteralVariant"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "All supported literal variants"
  i32.const 0 ;; string: "floatTypes"
  i32.const 0 ;; string: "hydra.core.FloatType"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "All supported float types"
  i32.const 0 ;; string: "functionVariants"
  i32.const 0 ;; string: "hydra.variants.FunctionVariant"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "All supported function variants"
  i32.const 0 ;; string: "integerTypes"
  i32.const 0 ;; string: "hydra.core.IntegerType"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "All supported integer types"
  i32.const 0 ;; string: "termVariants"
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "All supported term variants"
  i32.const 0 ;; string: "typeVariants"
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "All supported type variants"
  i32.const 0 ;; string: "types"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A logical set of types, as a predicate which tests a type for inclusion"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A set of constraints on valid type and term expressions, characterizing a language"
  i32.const 0 ;; string: "hydra.coders.LanguageName"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The unique name of a language"
  i32.const 0 ;; string: "hydra.coders.SymmetricAdapter"
  i32.const 0 ;; string: "t"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "hydra.coders.Adapter"
  i32.const 0 ;; string: "t"
  i32.const 0 ;; string: "t"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "v"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A bidirectional encoder which maps between the same type and term languages on either side"
  i32.const 0 ;; string: "hydra.coders.TraversalOrder"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "pre"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Pre-order traversal"
  i32.const 0 ;; string: "post"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Post-order traversal"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Specifies either a pre-order or post-order traversal"
  i32.const 0 ;; string: "hydra.coders.TypeAdapter"
  i32.const 0 ;; string: "hydra.coders.AdapterContext"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0
  i32.const 0 ;; string: "hydra.coders.SymmetricAdapter"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A function which maps a Hydra type to a symmetric adapter between types and terms"
  i32.const 0 ;; string: "hydra.context.Context"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "trace"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A stack of context labels describing the current execution path"
  i32.const 0 ;; string: "messages"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A log of warnings and/or info messages"
  i32.const 0 ;; string: "other"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A map of string keys to arbitrary terms as values, for application-specific use"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An execution context for tracing and diagnostics, threaded through function calls"
  i32.const 0 ;; string: "hydra.context.InContext"
  i32.const 0 ;; string: "e"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "object"
  i32.const 0 ;; string: "e"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A domain object; typically an error"
  i32.const 0 ;; string: "context"
  i32.const 0 ;; string: "hydra.context.Context"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The execution context at the point of capture"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A particular domain object (such as an error) together with an execution context"
  i32.const 0 ;; string: "hydra.core.AnnotatedTerm"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "body"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The term being annotated"
  i32.const 0 ;; string: "annotation"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The annotation as a map from keys to values"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A term together with an annotation"
  i32.const 0 ;; string: "hydra.core.AnnotatedType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "body"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type being annotated"
  i32.const 0 ;; string: "annotation"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The annotation as a map from keys to values"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A type together with an annotation"
  i32.const 0 ;; string: "hydra.core.Application"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "function"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The left-hand side of the application"
  i32.const 0 ;; string: "argument"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The right-hand side of the application"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A term which applies a function to an argument"
  i32.const 0 ;; string: "hydra.core.ApplicationType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "function"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The left-hand side of the application"
  i32.const 0 ;; string: "argument"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The right-hand side of the application"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type-level analog of an application term"
  i32.const 0 ;; string: "hydra.core.Binding"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "name"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The name of the bound variable"
  i32.const 0 ;; string: "term"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The term to which the variable is bound"
  i32.const 0 ;; string: "type"
  i32.const 0 ;; string: "hydra.core.TypeScheme"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The optional type of the bound term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A field with an optional type scheme, used to bind variables to terms in a 'let' expression"
  i32.const 0 ;; string: "hydra.core.CaseStatement"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "typeName"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The name of the union type"
  i32.const 0 ;; string: "default"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An optional default case, used if none of the explicit cases match"
  i32.const 0 ;; string: "cases"
  i32.const 0 ;; string: "hydra.core.Field"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A list of case alternatives, one per union field"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A union elimination; a case statement"
  i32.const 0 ;; string: "hydra.core.EitherType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "left"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The 'left' alternative"
  i32.const 0 ;; string: "right"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The 'right' alternative"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A type which provides a choice between a 'left' type and a 'right' type"
  i32.const 0 ;; string: "hydra.core.Elimination"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "record"
  i32.const 0 ;; string: "hydra.core.Projection"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Eliminates a record by projecting a given field"
  i32.const 0 ;; string: "union"
  i32.const 0 ;; string: "hydra.core.CaseStatement"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Eliminates a union term by matching over the fields of the union. This is a case statement."
  i32.const 0 ;; string: "wrap"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Unwrap a wrapped term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A corresponding elimination for an introduction term"
  i32.const 0 ;; string: "hydra.core.Field"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "name"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The name of the field"
  i32.const 0 ;; string: "term"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The term value of the field"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A name/term pair"
  i32.const 0 ;; string: "hydra.core.FieldType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "name"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The name of the field"
  i32.const 0 ;; string: "type"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type of the field"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A name/type pair"
  i32.const 0 ;; string: "hydra.core.FloatType"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "bigfloat"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An arbitrary-precision floating-point type"
  i32.const 0 ;; string: "float32"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 32-bit floating-point type"
  i32.const 0 ;; string: "float64"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 64-bit floating-point type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A floating-point type"
  i32.const 0 ;; string: "hydra.core.FloatValue"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "bigfloat"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An arbitrary-precision floating-point value"
  i32.const 0 ;; string: "float32"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 32-bit floating-point value"
  i32.const 0 ;; string: "float64"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 64-bit floating-point value"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A floating-point literal value"
  i32.const 0 ;; string: "hydra.core.ForallType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "parameter"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The variable which is bound by the lambda"
  i32.const 0 ;; string: "body"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The body of the lambda"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A universally quantified type; the System F equivalent of a type scheme, and the type-level equivalent of a lambda term."
  i32.const 0 ;; string: "hydra.core.Function"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "elimination"
  i32.const 0 ;; string: "hydra.core.Elimination"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An elimination for any of a few term variants"
  i32.const 0 ;; string: "lambda"
  i32.const 0 ;; string: "hydra.core.Lambda"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A function abstraction (lambda)"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A function"
  i32.const 0 ;; string: "hydra.core.FunctionType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "domain"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The domain (input) type of the function"
  i32.const 0 ;; string: "codomain"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The codomain (output) type of the function"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A function type, also known as an arrow type"
  i32.const 0 ;; string: "hydra.core.Injection"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "typeName"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The name of the union type"
  i32.const 0 ;; string: "field"
  i32.const 0 ;; string: "hydra.core.Field"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The field being injected, including its name and value"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An instance of a union type; i.e. a string-indexed generalization of inl() or inr()"
  i32.const 0 ;; string: "hydra.core.IntegerType"
  i32.const 9
  ;; list elements follow
  i32.const 0 ;; string: "bigint"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An arbitrary-precision integer type"
  i32.const 0 ;; string: "int8"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An 8-bit signed integer type"
  i32.const 0 ;; string: "int16"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 16-bit signed integer type"
  i32.const 0 ;; string: "int32"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 32-bit signed integer type"
  i32.const 0 ;; string: "int64"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 64-bit signed integer type"
  i32.const 0 ;; string: "uint8"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An 8-bit unsigned integer type"
  i32.const 0 ;; string: "uint16"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 16-bit unsigned integer type"
  i32.const 0 ;; string: "uint32"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 32-bit unsigned integer type"
  i32.const 0 ;; string: "uint64"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 64-bit unsigned integer type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An integer type"
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 9
  ;; list elements follow
  i32.const 0 ;; string: "bigint"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An arbitrary-precision integer value"
  i32.const 0 ;; string: "int8"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An 8-bit signed integer value"
  i32.const 0 ;; string: "int16"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 16-bit signed integer value (short value)"
  i32.const 0 ;; string: "int32"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 32-bit signed integer value (int value)"
  i32.const 0 ;; string: "int64"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 64-bit signed integer value (long value)"
  i32.const 0 ;; string: "uint8"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An 8-bit unsigned integer value (byte)"
  i32.const 0 ;; string: "uint16"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 16-bit unsigned integer value"
  i32.const 0 ;; string: "uint32"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 32-bit unsigned integer value (unsigned int)"
  i32.const 0 ;; string: "uint64"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 64-bit unsigned integer value (unsigned long)"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An integer literal value"
  i32.const 0 ;; string: "hydra.core.Lambda"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "parameter"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The parameter of the lambda"
  i32.const 0 ;; string: "domain"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An optional domain type for the lambda"
  i32.const 0 ;; string: "body"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The body of the lambda"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A function abstraction (lambda)"
  i32.const 0 ;; string: "hydra.core.Let"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "bindings"
  i32.const 0 ;; string: "hydra.core.Binding"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The list of variable bindings"
  i32.const 0 ;; string: "body"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The body term in which the variables are bound"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A set of (possibly recursive) 'let' bindings together with a body in which they are bound"
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "binary"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A binary literal"
  i32.const 0 ;; string: "boolean"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A boolean literal"
  i32.const 0 ;; string: "float"
  i32.const 0 ;; string: "hydra.core.FloatValue"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A floating-point literal"
  i32.const 0 ;; string: "integer"
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An integer literal"
  i32.const 0 ;; string: "string"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A string literal"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A term constant; an instance of a literal type"
  i32.const 0 ;; string: "hydra.core.LiteralType"
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "binary"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type of a binary (byte string) value"
  i32.const 0 ;; string: "boolean"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type of a boolean (true/false) value"
  i32.const 0 ;; string: "float"
  i32.const 0 ;; string: "hydra.core.FloatType"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type of a floating-point value"
  i32.const 0 ;; string: "integer"
  i32.const 0 ;; string: "hydra.core.IntegerType"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type of an integer value"
  i32.const 0 ;; string: "string"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type of a string value"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants"
  i32.const 0 ;; string: "hydra.core.MapType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "keys"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type of keys in the map"
  i32.const 0 ;; string: "values"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type of values in the map"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A map type"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A unique identifier in some context; a string-valued key"
  i32.const 0 ;; string: "hydra.core.PairType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "first"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The first component of the pair"
  i32.const 0 ;; string: "second"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The second component of the pair"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A type which pairs a 'first' type and a 'second' type"
  i32.const 0 ;; string: "hydra.core.Projection"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "typeName"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The name of the record type"
  i32.const 0 ;; string: "field"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The name of the projected field"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A record elimination; a projection"
  i32.const 0 ;; string: "hydra.core.Record"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "typeName"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The name of the record type"
  i32.const 0 ;; string: "fields"
  i32.const 0 ;; string: "hydra.core.Field"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The fields of the record, as a list of name/term pairs"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A record, or labeled tuple; a map of field names to terms"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 18
  ;; list elements follow
  i32.const 0 ;; string: "annotated"
  i32.const 0 ;; string: "hydra.core.AnnotatedTerm"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A term annotated with metadata"
  i32.const 0 ;; string: "application"
  i32.const 0 ;; string: "hydra.core.Application"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A function application"
  i32.const 0 ;; string: "either"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An either value"
  i32.const 0 ;; string: "function"
  i32.const 0 ;; string: "hydra.core.Function"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A function term"
  i32.const 0 ;; string: "let"
  i32.const 0 ;; string: "hydra.core.Let"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A 'let' term, which binds variables to terms"
  i32.const 0 ;; string: "list"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A list"
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A literal value"
  i32.const 0 ;; string: "map"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A map of keys to values"
  i32.const 0 ;; string: "maybe"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An optional value"
  i32.const 0 ;; string: "pair"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A pair (2-tuple)"
  i32.const 0 ;; string: "record"
  i32.const 0 ;; string: "hydra.core.Record"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A record term"
  i32.const 0 ;; string: "set"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A set of values"
  i32.const 0 ;; string: "typeApplication"
  i32.const 0 ;; string: "hydra.core.TypeApplicationTerm"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A System F type application term"
  i32.const 0 ;; string: "typeLambda"
  i32.const 0 ;; string: "hydra.core.TypeLambda"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A System F type abstraction term"
  i32.const 0 ;; string: "union"
  i32.const 0 ;; string: "hydra.core.Injection"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An injection; an instance of a union type"
  i32.const 0 ;; string: "unit"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A unit value; a term with no value"
  i32.const 0 ;; string: "variable"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A variable reference"
  i32.const 0 ;; string: "wrap"
  i32.const 0 ;; string: "hydra.core.WrappedTerm"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A wrapped term; an instance of a wrapper type (newtype)"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A data term"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 17
  ;; list elements follow
  i32.const 0 ;; string: "annotated"
  i32.const 0 ;; string: "hydra.core.AnnotatedType"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An annotated type"
  i32.const 0 ;; string: "application"
  i32.const 0 ;; string: "hydra.core.ApplicationType"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A type application"
  i32.const 0 ;; string: "either"
  i32.const 0 ;; string: "hydra.core.EitherType"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An either (sum) type"
  i32.const 0 ;; string: "forall"
  i32.const 0 ;; string: "hydra.core.ForallType"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A universally quantified (polymorphic) type"
  i32.const 0 ;; string: "function"
  i32.const 0 ;; string: "hydra.core.FunctionType"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A function type"
  i32.const 0 ;; string: "list"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A list type"
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "hydra.core.LiteralType"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A literal type"
  i32.const 0 ;; string: "map"
  i32.const 0 ;; string: "hydra.core.MapType"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A map type"
  i32.const 0 ;; string: "maybe"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An optional type"
  i32.const 0 ;; string: "pair"
  i32.const 0 ;; string: "hydra.core.PairType"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A pair (2-tuple) type"
  i32.const 0 ;; string: "record"
  i32.const 0 ;; string: "hydra.core.FieldType"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A record type"
  i32.const 0 ;; string: "set"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A set type"
  i32.const 0 ;; string: "union"
  i32.const 0 ;; string: "hydra.core.FieldType"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A union type with field names"
  i32.const 0 ;; string: "unit"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The unit type"
  i32.const 0 ;; string: "variable"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A type variable"
  i32.const 0 ;; string: "void"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The void (uninhabited, or bottom) type"
  i32.const 0 ;; string: "wrap"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A wrapped type (newtype)"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A data type"
  i32.const 0 ;; string: "hydra.core.TypeApplicationTerm"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "body"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The term being applied to a type"
  i32.const 0 ;; string: "type"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type argument"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A term applied to a type; a type application"
  i32.const 0 ;; string: "hydra.core.TypeLambda"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "parameter"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type variable introduced by the abstraction"
  i32.const 0 ;; string: "body"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The body of the abstraction"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A System F type abstraction term"
  i32.const 0 ;; string: "hydra.core.TypeScheme"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "variables"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The free type variables"
  i32.const 0 ;; string: "type"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type expression"
  i32.const 0 ;; string: "constraints"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 0 ;; string: "hydra.core.TypeVariableMetadata"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Optional metadata for type variables, including typeclass constraints. The map keys are type variable names."
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A type expression together with free type variables occurring in the expression"
  i32.const 0 ;; string: "hydra.core.TypeVariableMetadata"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "classes"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The set of typeclass constraints on this type variable"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Metadata associated with a type variable, including typeclass constraints"
  i32.const 0 ;; string: "hydra.core.WrappedTerm"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "typeName"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The name of the wrapper type"
  i32.const 0 ;; string: "body"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The wrapped term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A term wrapped in a type name"
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An error that occurred during decoding of a term"
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 11
  ;; list elements follow
  i32.const 0 ;; string: "checking"
  i32.const 0 ;; string: "hydra.error.checking.CheckingError"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A type checking error"
  i32.const 0 ;; string: "decoding"
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An error that occurred during decoding of a term"
  i32.const 0 ;; string: "duplicateBinding"
  i32.const 0 ;; string: "hydra.error.core.DuplicateBindingError"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A duplicate binding name error"
  i32.const 0 ;; string: "duplicateField"
  i32.const 0 ;; string: "hydra.error.core.DuplicateFieldError"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A duplicate field name error"
  i32.const 0 ;; string: "other"
  i32.const 0 ;; string: "hydra.errors.OtherError"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Any other error"
  i32.const 0 ;; string: "undefinedField"
  i32.const 0 ;; string: "hydra.error.core.UndefinedFieldError"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A reference to an undefined field"
  i32.const 0 ;; string: "undefinedTermVariable"
  i32.const 0 ;; string: "hydra.error.core.UndefinedTermVariableError"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A reference to an undefined term variable"
  i32.const 0 ;; string: "untypedTermVariable"
  i32.const 0 ;; string: "hydra.error.core.UntypedTermVariableError"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A term variable whose type is not known"
  i32.const 0 ;; string: "unexpectedTermVariant"
  i32.const 0 ;; string: "hydra.error.core.UnexpectedTermVariantError"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An unexpected term variant"
  i32.const 0 ;; string: "unexpectedTypeVariant"
  i32.const 0 ;; string: "hydra.error.core.UnexpectedTypeVariantError"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An unexpected type variant"
  i32.const 0 ;; string: "unification"
  i32.const 0 ;; string: "hydra.errors.UnificationError"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A type unification error"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An error of any kind, with kernel errors particularly differentiated"
  i32.const 0 ;; string: "hydra.errors.OtherError"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Any other error"
  i32.const 0 ;; string: "hydra.errors.UnificationError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "leftType"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The left-hand type in the unification"
  i32.const 0 ;; string: "rightType"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The right-hand type in the unification"
  i32.const 0 ;; string: "message"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A human-readable error message"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An error that occurred during type unification"
  i32.const 0 ;; string: "hydra.graph.Graph"
  i32.const 8
  ;; list elements follow
  i32.const 0 ;; string: "boundTerms"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The terms bound by all term variables in scope"
  i32.const 0 ;; string: "boundTypes"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 0 ;; string: "hydra.core.TypeScheme"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type schemes of all term variables in scope"
  i32.const 0 ;; string: "classConstraints"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 0 ;; string: "hydra.core.TypeVariableMetadata"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A mutable map from type variable names to their accumulated class constraints. This is populated during type inference when operations requiring Eq or Ord are encountered."
  i32.const 0 ;; string: "lambdaVariables"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The set of term variables introduced by specifically by lambdas"
  i32.const 0 ;; string: "metadata"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Any additional metadata bound to term variables in scope"
  i32.const 0 ;; string: "primitives"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 0 ;; string: "hydra.graph.Primitive"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "All primitive functions and constants by name"
  i32.const 0 ;; string: "schemaTypes"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 0 ;; string: "hydra.core.TypeScheme"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "All schema types (type schemes) in scope"
  i32.const 0 ;; string: "typeVariables"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The set of type variables introduced specifically by type lambdas"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A graph, or lexical environment which binds names to terms, types, primitives, and metadata"
  i32.const 0 ;; string: "hydra.graph.Primitive"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "name"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The unique name of the primitive function"
  i32.const 0 ;; string: "type"
  i32.const 0 ;; string: "hydra.core.TypeScheme"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type signature of the primitive function"
  i32.const 0 ;; string: "implementation"
  i32.const 0 ;; string: "hydra.context.Context"
  i32.const 0 ;; string: "hydra.graph.Graph"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "hydra.context.InContext"
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A concrete implementation of the primitive function. The Context and Graph parameters are needed by higher-order primitives (e.g. lists.map, lists.foldl, eithers.bind) which must evaluate function arguments via term reduction; the Graph provides variable and primitive bindings, while the Context supports tracing and error reporting."
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A built-in function or constant"
  i32.const 0 ;; string: "hydra.graph.TermCoder"
  i32.const 0 ;; string: "a"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "type"
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The Hydra type of encoded terms"
  i32.const 0 ;; string: "encode"
  i32.const 0 ;; string: "hydra.context.Context"
  i32.const 0 ;; string: "hydra.graph.Graph"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "hydra.context.InContext"
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "a"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An encode function from terms to native values"
  i32.const 0 ;; string: "decode"
  i32.const 0 ;; string: "hydra.context.Context"
  i32.const 0 ;; string: "a"
  i32.const 0 ;; string: "hydra.context.InContext"
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A decode function from native values to terms"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms."
  i32.const 0 ;; string: "hydra.packaging.Definition"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "term"
  i32.const 0 ;; string: "hydra.packaging.TermDefinition"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A term definition"
  i32.const 0 ;; string: "type"
  i32.const 0 ;; string: "hydra.packaging.TypeDefinition"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A type definition"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A definition, which may be either a term or type definition"
  i32.const 0 ;; string: "hydra.packaging.FileExtension"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A file extension (without the dot), e.g. "json" or "py""
  i32.const 0 ;; string: "hydra.packaging.Library"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "namespace"
  i32.const 0 ;; string: "hydra.packaging.Namespace"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A common prefix for all primitive function names in the library"
  i32.const 0 ;; string: "prefix"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A preferred namespace prefix for function names in the library"
  i32.const 0 ;; string: "primitives"
  i32.const 0 ;; string: "hydra.graph.Primitive"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The primitives defined in this library"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A library of primitive functions"
  i32.const 0 ;; string: "hydra.packaging.Module"
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "namespace"
  i32.const 0 ;; string: "hydra.packaging.Namespace"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A common prefix for all element names in the module"
  i32.const 0 ;; string: "definitions"
  i32.const 0 ;; string: "hydra.packaging.Definition"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The definitions in this module"
  i32.const 0 ;; string: "termDependencies"
  i32.const 0 ;; string: "hydra.packaging.Namespace"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Any modules which the term expressions of this module directly depend upon"
  i32.const 0 ;; string: "typeDependencies"
  i32.const 0 ;; string: "hydra.packaging.Namespace"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Any modules which the type expressions of this module directly depend upon"
  i32.const 0 ;; string: "description"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An optional human-readable description of the module"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A logical collection of elements in the same namespace, having dependencies on zero or more other modules"
  i32.const 0 ;; string: "hydra.packaging.Namespace"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A prefix for element names"
  i32.const 0 ;; string: "hydra.packaging.Namespaces"
  i32.const 0 ;; string: "n"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "focus"
  i32.const 0 ;; string: "hydra.packaging.Namespace"
  i32.const 0 ;; string: "n"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The namespace in focus, together with its associated value"
  i32.const 0 ;; string: "mapping"
  i32.const 0 ;; string: "hydra.packaging.Namespace"
  i32.const 0 ;; string: "n"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A mapping of namespaces to values"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A mapping from namespaces to values of type n, with a focus on one namespace"
  i32.const 0 ;; string: "hydra.packaging.Package"
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "name"
  i32.const 0 ;; string: "hydra.packaging.PackageName"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The name of the package"
  i32.const 0 ;; string: "modules"
  i32.const 0 ;; string: "hydra.packaging.Module"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The modules in this package"
  i32.const 0 ;; string: "dependencies"
  i32.const 0 ;; string: "hydra.packaging.PackageName"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The packages which this package depends on"
  i32.const 0 ;; string: "description"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An optional human-readable description of the package"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A package, which is a named collection of modules with metadata and dependencies"
  i32.const 0 ;; string: "hydra.packaging.PackageName"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The unique name of a package, e.g. "hydra-kernel" or "hydra-python""
  i32.const 0 ;; string: "hydra.packaging.QualifiedName"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "namespace"
  i32.const 0 ;; string: "hydra.packaging.Namespace"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The optional namespace"
  i32.const 0 ;; string: "local"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The local name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A qualified name consisting of an optional namespace together with a mandatory local name"
  i32.const 0 ;; string: "hydra.packaging.TermDefinition"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "name"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The name of the term"
  i32.const 0 ;; string: "term"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The term being defined"
  i32.const 0 ;; string: "type"
  i32.const 0 ;; string: "hydra.core.TypeScheme"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type scheme of the term, including any class constraints"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A term-level definition, including a name, a term, and the type scheme of the term"
  i32.const 0 ;; string: "hydra.packaging.TypeDefinition"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "name"
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The name of the type"
  i32.const 0 ;; string: "type"
  i32.const 0 ;; string: "hydra.core.TypeScheme"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "The type scheme being defined"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A type-level definition, including a name and the type scheme"
  i32.const 0 ;; string: "hydra.util.CaseConvention"
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "camel"
  i32.const 0
  i32.const 0 ;; string: "pascal"
  i32.const 0
  i32.const 0 ;; string: "lowerSnake"
  i32.const 0
  i32.const 0 ;; string: "upperSnake"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "A naming convention for symbols, such as camelCase or snake_case"
  i32.const 0 ;; string: "hydra.util.Comparison"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "lessThan"
  i32.const 0
  i32.const 0 ;; string: "equalTo"
  i32.const 0
  i32.const 0 ;; string: "greaterThan"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "An equality judgement: less than, equal to, or greater than"
  i32.const 0 ;; string: "hydra.util.Precision"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "arbitrary"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Arbitrary precision"
  i32.const 0 ;; string: "bits"
  i32.const 0
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Precision to a specified number of bits"
  i32.const 1
  ;; map entries follow
  i32.const 0 ;; string: "description"
  i32.const 0 ;; string: "Numeric precision: arbitrary precision, or precision to a specified number of bits"
)
)
