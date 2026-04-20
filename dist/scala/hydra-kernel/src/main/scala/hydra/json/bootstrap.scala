package hydra.json.bootstrap

import hydra.core.*

lazy val typesByName: Map[hydra.core.Name, hydra.core.Type] = Map("hydra.coders.Adapter" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.forall(hydra.core.ForallType("t1",
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Type.forall(hydra.core.ForallType("t2", hydra.core.Type.forall(hydra.core.ForallType("v1",
   hydra.core.Type.forall(hydra.core.ForallType("v2", hydra.core.Type.record(Seq(hydra.core.FieldType("isLossy",
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.boolean),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Whether information may be lost in the course of this adaptation")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("source", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("t1"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The source type")))))),
   hydra.core.FieldType("target", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("t2"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The target type")))))),
   hydra.core.FieldType("coder", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable("hydra.coders.Coder"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("v1"))), hydra.core.Type.variable("v2"))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The coder for transforming instances of the source type to instances of the target type")))))))))))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A two-level bidirectional encoder which adapts types to types and terms to terms"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.coders.AdapterContext" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("graph",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.graph.Graph"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The underlying graph of elements and primitives")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("language", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.coders.Language"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The language being encoded or decoded")))))),
   hydra.core.FieldType("adapters", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable("hydra.core.Name"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable("hydra.coders.Adapter"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("hydra.core.Type"))), hydra.core.Type.variable("hydra.core.Type"))),
   hydra.core.Type.variable("hydra.core.Term"))), hydra.core.Type.variable("hydra.core.Term"))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A map of type names to adapters for those types")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An evaluation context together with a source language and a target language"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.coders.Bicoder" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.forall(hydra.core.ForallType("t1",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.forall(hydra.core.ForallType("t2", hydra.core.Type.forall(hydra.core.ForallType("v1",
   hydra.core.Type.forall(hydra.core.ForallType("v2", hydra.core.Type.record(Seq(hydra.core.FieldType("encode",
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("t1"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable("hydra.coders.Adapter"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("t1"))), hydra.core.Type.variable("t2"))), hydra.core.Type.variable("v1"))),
   hydra.core.Type.variable("v2"))))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A function from source types to adapters")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("decode", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("t2"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable("hydra.coders.Adapter"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("t2"))), hydra.core.Type.variable("t1"))), hydra.core.Type.variable("v2"))),
   hydra.core.Type.variable("v1"))))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A function from target types to adapters")))))))))))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A two-level encoder and decoder, operating both at a type level and an instance (data) level"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.coders.Coder" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.forall(hydra.core.ForallType("v1",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.forall(hydra.core.ForallType("v2", hydra.core.Type.record(Seq(hydra.core.FieldType("encode",
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("hydra.context.Context"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("v1"),
   hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.variable("hydra.errors.Error"),
   hydra.core.Type.variable("v2"))))))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A function which encodes source values as target values in a given context")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("decode", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("hydra.context.Context"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("v2"),
   hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.variable("hydra.errors.Error"),
   hydra.core.Type.variable("v1"))))))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A function which decodes target values as source values in a given context")))))))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An encoder and decoder; a bidirectional transformation between two types"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.coders.CoderDirection" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("encode",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.unit), hydra.core.FieldType("decode", hydra.core.Type.unit))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Indicates either the 'out' or the 'in' direction of a coder"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.coders.Language" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("name",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.coders.LanguageName"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The unique name of the language")))))),
   hydra.core.FieldType("constraints", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.coders.LanguageConstraints"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The constraints which characterize the language")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A named language together with language-specific constraints"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.coders.LanguageConstraints" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("eliminationVariants",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.set(hydra.core.Type.variable("hydra.variants.EliminationVariant")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("All supported elimination variants")))))),
   hydra.core.FieldType("literalVariants", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.set(hydra.core.Type.variable("hydra.variants.LiteralVariant")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("All supported literal variants")))))),
   hydra.core.FieldType("floatTypes", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.set(hydra.core.Type.variable("hydra.core.FloatType")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("All supported float types")))))),
   hydra.core.FieldType("functionVariants", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.set(hydra.core.Type.variable("hydra.variants.FunctionVariant")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("All supported function variants")))))),
   hydra.core.FieldType("integerTypes", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.set(hydra.core.Type.variable("hydra.core.IntegerType")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("All supported integer types")))))),
   hydra.core.FieldType("termVariants", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.set(hydra.core.Type.variable("hydra.variants.TermVariant")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("All supported term variants")))))),
   hydra.core.FieldType("typeVariants", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.set(hydra.core.Type.variable("hydra.variants.TypeVariant")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("All supported type variants")))))),
   hydra.core.FieldType("types", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.literal(hydra.core.LiteralType.boolean))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A logical set of types, as a predicate which tests a type for inclusion")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A set of constraints on valid type and term expressions, characterizing a language"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.coders.LanguageName" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.wrap(hydra.core.Type.literal(hydra.core.LiteralType.string)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The unique name of a language"))))),
   "hydra.coders.SymmetricAdapter" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.forall(hydra.core.ForallType("t",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.forall(hydra.core.ForallType("v", hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable("hydra.coders.Adapter"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("t"))), hydra.core.Type.variable("t"))), hydra.core.Type.variable("v"))),
   hydra.core.Type.variable("v"))))))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A bidirectional encoder which maps between the same type and term languages on either side"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.coders.TraversalOrder" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("pre",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit, Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Pre-order traversal")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("post", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Post-order traversal")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Specifies either a pre-order or post-order traversal"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.coders.TypeAdapter" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("hydra.coders.AdapterContext"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("hydra.core.Type"),
   hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.literal(hydra.core.LiteralType.string),
   hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable("hydra.coders.SymmetricAdapter"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("hydra.core.Type"))), hydra.core.Type.variable("hydra.core.Term"))))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A function which maps a Hydra type to a symmetric adapter between types and terms"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.context.Context" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("trace",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.literal(hydra.core.LiteralType.string)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A stack of context labels describing the current execution path")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("messages", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.literal(hydra.core.LiteralType.string)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A log of warnings and/or info messages")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("other", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable("hydra.core.Name"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("hydra.core.Term"))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A map of string keys to arbitrary terms as values, for application-specific use")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An execution context for tracing and diagnostics, threaded through function calls"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.context.InContext" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.forall(hydra.core.ForallType("e",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.record(Seq(hydra.core.FieldType("object", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("e"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A domain object; typically an error")))))),
   hydra.core.FieldType("context", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.context.Context"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The execution context at the point of capture")))))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A particular domain object (such as an error) together with an execution context"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.AnnotatedTerm" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("body",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Term"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The term being annotated")))))),
   hydra.core.FieldType("annotation", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable("hydra.core.Name"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("hydra.core.Term"))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The annotation as a map from keys to values")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A term together with an annotation"))))),
   "hydra.core.AnnotatedType" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("body",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type being annotated")))))),
   hydra.core.FieldType("annotation", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable("hydra.core.Name"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("hydra.core.Term"))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The annotation as a map from keys to values")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A type together with an annotation"))))),
   "hydra.core.Application" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("function",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Term"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The left-hand side of the application")))))),
   hydra.core.FieldType("argument", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Term"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The right-hand side of the application")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A term which applies a function to an argument"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.ApplicationType" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("function",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The left-hand side of the application")))))),
   hydra.core.FieldType("argument", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The right-hand side of the application")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type-level analog of an application term"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.Binding" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("name",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The name of the bound variable")))))),
   hydra.core.FieldType("term", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Term"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The term to which the variable is bound")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("type", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.maybe(hydra.core.Type.variable("hydra.core.TypeScheme")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The optional type of the bound term")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A field with an optional type scheme, used to bind variables to terms in a 'let' expression"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.CaseStatement" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("typeName",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The name of the union type")))))),
   hydra.core.FieldType("default", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.maybe(hydra.core.Type.variable("hydra.core.Term")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An optional default case, used if none of the explicit cases match")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("cases", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.variable("hydra.core.Field")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A list of case alternatives, one per union field")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A union elimination; a case statement"))))),
   "hydra.core.EitherType" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("left",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The 'left' alternative")))))),
   hydra.core.FieldType("right", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The 'right' alternative")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A type which provides a choice between a 'left' type and a 'right' type"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.Field" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("name",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The name of the field")))))),
   hydra.core.FieldType("term", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Term"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The term value of the field")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A name/term pair"))))),
   "hydra.core.FieldType" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("name",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The name of the field")))))),
   hydra.core.FieldType("type", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type of the field")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A name/type pair"))))),
   "hydra.core.FloatType" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("bigfloat",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit, Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An arbitrary-precision floating-point type")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("float32", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 32-bit floating-point type")))))),
   hydra.core.FieldType("float64", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 64-bit floating-point type")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A floating-point type"))))),
   "hydra.core.FloatValue" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("bigfloat",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.float(hydra.core.FloatType.bigfloat)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An arbitrary-precision floating-point value")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("float32", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.float(hydra.core.FloatType.float32)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 32-bit floating-point value")))))),
   hydra.core.FieldType("float64", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.float(hydra.core.FloatType.float64)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 64-bit floating-point value")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A floating-point literal value"))))),
   "hydra.core.ForallType" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("parameter",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The variable which is bound by the lambda")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("body", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The body of the lambda")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A universally quantified type; the System F equivalent of a type scheme, and the type-level equivalent of a lambda term."))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.FunctionType" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("domain",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The domain (input) type of the function")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("codomain", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The codomain (output) type of the function")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A function type, also known as an arrow type"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.Injection" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("typeName",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The name of the union type")))))),
   hydra.core.FieldType("field", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Field"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The field being injected, including its name and value")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An instance of a union type; i.e. a string-indexed generalization of inl() or inr()"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.IntegerType" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("bigint",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit, Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An arbitrary-precision integer type")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("int8", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An 8-bit signed integer type")))))),
   hydra.core.FieldType("int16", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 16-bit signed integer type")))))),
   hydra.core.FieldType("int32", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 32-bit signed integer type")))))),
   hydra.core.FieldType("int64", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 64-bit signed integer type")))))),
   hydra.core.FieldType("uint8", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An 8-bit unsigned integer type")))))),
   hydra.core.FieldType("uint16", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 16-bit unsigned integer type")))))),
   hydra.core.FieldType("uint32", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 32-bit unsigned integer type")))))),
   hydra.core.FieldType("uint64", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 64-bit unsigned integer type")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An integer type"))))),
   "hydra.core.IntegerValue" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("bigint",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.bigint)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An arbitrary-precision integer value")))))),
   hydra.core.FieldType("int8", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int8)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An 8-bit signed integer value")))))),
   hydra.core.FieldType("int16", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int16)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 16-bit signed integer value (short value)")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("int32", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 32-bit signed integer value (int value)")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("int64", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int64)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 64-bit signed integer value (long value)")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("uint8", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.uint8)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An 8-bit unsigned integer value (byte)")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("uint16", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.uint16)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 16-bit unsigned integer value")))))),
   hydra.core.FieldType("uint32", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.uint32)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 32-bit unsigned integer value (unsigned int)")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("uint64", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.uint64)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 64-bit unsigned integer value (unsigned long)")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An integer literal value"))))),
   "hydra.core.Lambda" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("parameter",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The parameter of the lambda")))))),
   hydra.core.FieldType("domain", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.maybe(hydra.core.Type.variable("hydra.core.Type")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An optional domain type for the lambda")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("body", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Term"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The body of the lambda")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A function abstraction (lambda)"))))),
   "hydra.core.Let" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("bindings",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.variable("hydra.core.Binding")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The list of variable bindings")))))),
   hydra.core.FieldType("body", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Term"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The body term in which the variables are bound")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A set of (possibly recursive) 'let' bindings together with a body in which they are bound"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.Literal" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("binary",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.binary),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A binary literal")))))),
   hydra.core.FieldType("boolean", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.boolean),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A boolean literal")))))),
   hydra.core.FieldType("decimal", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.decimal),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An arbitrary-precision decimal literal")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("float", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.FloatValue"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A floating-point literal")))))),
   hydra.core.FieldType("integer", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.IntegerValue"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An integer literal")))))),
   hydra.core.FieldType("string", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.string),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A string literal")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A term constant; an instance of a literal type"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.LiteralType" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("binary",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit, Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type of a binary (byte string) value")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("boolean", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type of a boolean (true/false) value")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("decimal", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type of an arbitrary-precision decimal value")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("float", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.FloatType"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type of a floating-point value")))))),
   hydra.core.FieldType("integer", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.IntegerType"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type of an integer value")))))),
   hydra.core.FieldType("string", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type of a string value")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.MapType" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("keys",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type of keys in the map")))))),
   hydra.core.FieldType("values", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type of values in the map")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A map type"))))),
   "hydra.core.Name" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.wrap(hydra.core.Type.literal(hydra.core.LiteralType.string)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A unique identifier in some context; a string-valued key"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.PairType" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("first",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The first component of the pair")))))),
   hydra.core.FieldType("second", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The second component of the pair")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A type which pairs a 'first' type and a 'second' type"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.Projection" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("typeName",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The name of the record type")))))),
   hydra.core.FieldType("field", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The name of the projected field")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A record elimination; a projection"))))),
   "hydra.core.Record" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("typeName",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The name of the record type")))))),
   hydra.core.FieldType("fields", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.variable("hydra.core.Field")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The fields of the record, as a list of name/term pairs")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A record, or labeled tuple; a map of field names to terms"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.Term" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("annotated",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.AnnotatedTerm"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A term annotated with metadata")))))),
   hydra.core.FieldType("application", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Application"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A function application")))))),
   hydra.core.FieldType("cases", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.CaseStatement"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A union elimination; a case statement")))))),
   hydra.core.FieldType("either", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.variable("hydra.core.Term"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("hydra.core.Term"))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An either value")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("inject", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Injection"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An injection; an instance of a union type")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("lambda", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Lambda"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A function abstraction (lambda)")))))),
   hydra.core.FieldType("let", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Let"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A 'let' term, which binds variables to terms")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("list", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.variable("hydra.core.Term")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A list")))))),
   hydra.core.FieldType("literal", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Literal"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A literal value")))))),
   hydra.core.FieldType("map", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable("hydra.core.Term"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("hydra.core.Term"))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A map of keys to values")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("maybe", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.maybe(hydra.core.Type.variable("hydra.core.Term")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An optional value")))))),
   hydra.core.FieldType("pair", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.pair(hydra.core.PairType(hydra.core.Type.variable("hydra.core.Term"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("hydra.core.Term"))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A pair (2-tuple)")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("project", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Projection"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A record elimination; a projection")))))),
   hydra.core.FieldType("record", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Record"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A record term")))))),
   hydra.core.FieldType("set", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.set(hydra.core.Type.variable("hydra.core.Term")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A set of values")))))),
   hydra.core.FieldType("typeApplication", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.TypeApplicationTerm"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A System F type application term")))))),
   hydra.core.FieldType("typeLambda", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.TypeLambda"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A System F type abstraction term")))))),
   hydra.core.FieldType("unit", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A unit value; a term with no value")))))),
   hydra.core.FieldType("unwrap", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An unwrap elimination; the inverse of a wrap")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("variable", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A variable reference")))))),
   hydra.core.FieldType("wrap", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.WrappedTerm"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A wrapped term; an instance of a wrapper type (newtype)")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A data term"))))),
   "hydra.core.Type" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("annotated",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.AnnotatedType"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An annotated type")))))),
   hydra.core.FieldType("application", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.ApplicationType"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A type application")))))),
   hydra.core.FieldType("either", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.EitherType"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An either (sum) type")))))),
   hydra.core.FieldType("forall", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.ForallType"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A universally quantified (polymorphic) type")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("function", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.FunctionType"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A function type")))))),
   hydra.core.FieldType("list", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A list type")))))),
   hydra.core.FieldType("literal", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.LiteralType"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A literal type")))))),
   hydra.core.FieldType("map", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.MapType"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A map type")))))),
   hydra.core.FieldType("maybe", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An optional type")))))),
   hydra.core.FieldType("pair", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.PairType"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A pair (2-tuple) type")))))),
   hydra.core.FieldType("record", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.variable("hydra.core.FieldType")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A record type")))))),
   hydra.core.FieldType("set", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A set type")))))),
   hydra.core.FieldType("union", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.variable("hydra.core.FieldType")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A union type with field names")))))),
   hydra.core.FieldType("unit", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The unit type")))))),
   hydra.core.FieldType("variable", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A type variable")))))),
   hydra.core.FieldType("void", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The void (uninhabited, or bottom) type")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("wrap", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A wrapped type (newtype)")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A data type"))))),
   "hydra.core.TypeApplicationTerm" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("body",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Term"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The term being applied to a type")))))),
   hydra.core.FieldType("type", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type argument")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A term applied to a type; a type application"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.TypeLambda" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("parameter",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type variable introduced by the abstraction")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("body", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Term"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The body of the abstraction")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A System F type abstraction term"))))),
   "hydra.core.TypeScheme" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("variables",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.variable("hydra.core.Name")),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The free type variables")))))),
   hydra.core.FieldType("type", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type expression")))))),
   hydra.core.FieldType("constraints", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.maybe(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable("hydra.core.Name"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("hydra.core.TypeVariableMetadata")))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Optional metadata for type variables, including typeclass constraints. The map keys are type variable names.")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A type expression together with free type variables occurring in the expression"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.TypeVariableMetadata" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("classes",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.set(hydra.core.Type.variable("hydra.core.Name")),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The set of typeclass constraints on this type variable")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Metadata associated with a type variable, including typeclass constraints"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.core.WrappedTerm" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("typeName",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The name of the wrapper type")))))),
   hydra.core.FieldType("body", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Term"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The wrapped term")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A term wrapped in a type name"))))),
   "hydra.errors.DecodingError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.wrap(hydra.core.Type.literal(hydra.core.LiteralType.string)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An error that occurred during decoding of a term"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.errors.EmptyListError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An empty list was encountered where a non-empty list was required"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.errors.Error" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("checking",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.error.checking.CheckingError"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A type checking error")))))),
   hydra.core.FieldType("decoding", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.DecodingError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An error that occurred during decoding of a term")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("duplicateBinding", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.error.core.DuplicateBindingError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A duplicate binding name error")))))),
   hydra.core.FieldType("duplicateField", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.error.core.DuplicateFieldError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A duplicate field name error")))))),
   hydra.core.FieldType("extraction", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.ExtractionError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An error that occurred while extracting a value from a term")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("inference", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.InferenceError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A type inference error")))))),
   hydra.core.FieldType("other", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.OtherError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Any other error")))))),
   hydra.core.FieldType("resolution", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.ResolutionError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A name-resolution error")))))),
   hydra.core.FieldType("undefinedField", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.error.core.UndefinedFieldError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A reference to an undefined field")))))),
   hydra.core.FieldType("undefinedTermVariable", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.error.core.UndefinedTermVariableError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A reference to an undefined term variable")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("untypedTermVariable", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.error.core.UntypedTermVariableError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A term variable whose type is not known")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("unexpectedTermVariant", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.error.core.UnexpectedTermVariantError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An unexpected term variant")))))),
   hydra.core.FieldType("unexpectedTypeVariant", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.error.core.UnexpectedTypeVariantError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An unexpected type variant")))))),
   hydra.core.FieldType("unification", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.UnificationError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A type unification error")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An error of any kind, with kernel errors particularly differentiated"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.errors.ExtractionError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("emptyList",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.EmptyListError"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An empty list was encountered where a non-empty list was required")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("multipleBindings", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.MultipleBindingsError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Multiple let bindings were found with the same name")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("multipleFields", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.MultipleFieldsError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Multiple record fields were found with the same field name")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("noMatchingField", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.NoMatchingFieldError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("No field with the expected name was found in a record")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("noSuchBinding", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.NoSuchBindingError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("No let binding with the expected name was found")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("notEnoughCases", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.NotEnoughCasesError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A case statement did not contain enough cases to match the target")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("unexpectedShape", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.UnexpectedShapeError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A term, type, literal, or other value had an unexpected shape")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An error that occurred while extracting a typed value from a term"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.errors.InferenceError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("checking",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.error.checking.CheckingError"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A type checking error encountered during inference")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("other", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.OtherInferenceError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A generic inference error carrying a message and a subterm path. Placeholder arm; sites should migrate to typed variants.")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("unification", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.UnificationInferenceError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A unification failure encountered while inferring types")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An error that occurred during type inference"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.errors.MultipleBindingsError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("name",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The binding name which was duplicated")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Multiple let bindings with the same name were found"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.errors.MultipleFieldsError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("fieldName",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The field name which appeared more than once")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Multiple fields with the same name were found in a record"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.errors.NoMatchingFieldError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("fieldName",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The field name which was not found")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("No field with the expected name was present"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.errors.NoSuchBindingError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("name",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The binding name which was not found")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("No let binding with the expected name was present"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.errors.NoSuchPrimitiveError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("name",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The primitive name which was not found")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("No primitive function with the expected name was registered in the graph"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.errors.NotEnoughCasesError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit,
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A case statement was missing a case for the requested variant"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.errors.OtherError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.wrap(hydra.core.Type.literal(hydra.core.LiteralType.string)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Any other error"))))),
   "hydra.errors.OtherInferenceError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("path",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.paths.SubtermPath"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The subterm path at which the error was observed")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("message", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.string),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A human-readable error message")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A generic inference error: message + subterm path"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.errors.OtherResolutionError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.wrap(hydra.core.Type.literal(hydra.core.LiteralType.string)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A generic resolution error: message"))))),
   "hydra.errors.ResolutionError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("noSuchBinding",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.NoSuchBindingError"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("No binding with the expected name was found in the graph")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("noSuchPrimitive", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.NoSuchPrimitiveError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("No primitive function with the expected name was found in the graph")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("noMatchingField", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.NoMatchingFieldError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("No field with the expected name was present in a record or case statement")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("other", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.OtherResolutionError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A generic resolution error carrying a message")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("unexpectedShape", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.UnexpectedShapeError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A term had a shape other than the one expected (e.g. a record, an injection)")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An error that occurred while resolving a name, primitive, or record/union shape in a graph"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.errors.UnexpectedShapeError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("expected",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.string),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A description of the expected shape")))))),
   hydra.core.FieldType("actual", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.string),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A description of the shape actually encountered")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A term, type, literal, or related value had a shape other than the one expected"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.errors.UnificationError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("leftType",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The left-hand type in the unification")))))),
   hydra.core.FieldType("rightType", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The right-hand type in the unification")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("message", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.string),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A human-readable error message")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An error that occurred during type unification"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.errors.UnificationInferenceError" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("path",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.paths.SubtermPath"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The subterm path at which the unification failure was observed")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("cause", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.errors.UnificationError"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The underlying unification error")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A unification failure at a specific subterm locus during inference"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.graph.Graph" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("boundTerms",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable("hydra.core.Name"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("hydra.core.Term"))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The terms bound by all term variables in scope")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("boundTypes", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable("hydra.core.Name"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("hydra.core.TypeScheme"))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type schemes of all term variables in scope")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("classConstraints", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable("hydra.core.Name"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("hydra.core.TypeVariableMetadata"))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A mutable map from type variable names to their accumulated class constraints. This is populated during type inference when operations requiring Eq or Ord are encountered.")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("lambdaVariables", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.set(hydra.core.Type.variable("hydra.core.Name")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The set of term variables introduced by specifically by lambdas")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("metadata", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable("hydra.core.Name"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("hydra.core.Term"))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Any additional metadata bound to term variables in scope")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("primitives", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable("hydra.core.Name"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("hydra.graph.Primitive"))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("All primitive functions and constants by name")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("schemaTypes", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable("hydra.core.Name"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("hydra.core.TypeScheme"))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("All schema types (type schemes) in scope")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("typeVariables", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.set(hydra.core.Type.variable("hydra.core.Name")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The set of type variables introduced specifically by type lambdas")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A graph, or lexical environment which binds names to terms, types, primitives, and metadata"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.graph.Primitive" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("name",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The unique name of the primitive function")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("type", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.TypeScheme"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type signature of the primitive function")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("implementation", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("hydra.context.Context"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("hydra.graph.Graph"),
   hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.list(hydra.core.Type.variable("hydra.core.Term")),
   hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.variable("hydra.errors.Error"),
   hydra.core.Type.variable("hydra.core.Term"))))))))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A concrete implementation of the primitive function. The Context and Graph parameters are needed by higher-order primitives (e.g. lists.map, lists.foldl, eithers.bind) which must evaluate function arguments via term reduction; the Graph provides variable and primitive bindings, while the Context supports tracing and error reporting.")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A built-in function or constant"))))),
   "hydra.graph.TermCoder" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.forall(hydra.core.ForallType("a",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.record(Seq(hydra.core.FieldType("type", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Type"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The Hydra type of encoded terms")))))),
   hydra.core.FieldType("encode", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("hydra.context.Context"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("hydra.graph.Graph"),
   hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("hydra.core.Term"),
   hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.variable("hydra.errors.Error"),
   hydra.core.Type.variable("a"))))))))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An encode function from terms to native values")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("decode", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("hydra.context.Context"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.variable("a"),
   hydra.core.Type.either(hydra.core.EitherType(hydra.core.Type.variable("hydra.errors.Error"),
   hydra.core.Type.variable("hydra.core.Term"))))))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A decode function from native values to terms")))))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms."))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.packaging.Definition" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("term",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.packaging.TermDefinition"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A term definition")))))),
   hydra.core.FieldType("type", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.packaging.TypeDefinition"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A type definition")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A definition, which may be either a term or type definition"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.packaging.FileExtension" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.wrap(hydra.core.Type.literal(hydra.core.LiteralType.string)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A file extension (without the dot), e.g. \"json\" or \"py\""))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.packaging.Library" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("namespace",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.packaging.Namespace"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A common prefix for all primitive function names in the library")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("prefix", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.string),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A preferred namespace prefix for function names in the library")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("primitives", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.variable("hydra.graph.Primitive")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The primitives defined in this library")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A library of primitive functions"))))),
   "hydra.packaging.Module" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("namespace",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.packaging.Namespace"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A common prefix for all element names in the module")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("definitions", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.variable("hydra.packaging.Definition")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The definitions in this module")))))),
   hydra.core.FieldType("termDependencies", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.variable("hydra.packaging.Namespace")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Any modules which the term expressions of this module directly depend upon")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("typeDependencies", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.variable("hydra.packaging.Namespace")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Any modules which the type expressions of this module directly depend upon")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("description", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.maybe(hydra.core.Type.literal(hydra.core.LiteralType.string)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An optional human-readable description of the module")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A logical collection of elements in the same namespace, having dependencies on zero or more other modules"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.packaging.Namespace" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.wrap(hydra.core.Type.literal(hydra.core.LiteralType.string)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A prefix for element names"))))),
   "hydra.packaging.Namespaces" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.forall(hydra.core.ForallType("n",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.record(Seq(hydra.core.FieldType("focus", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.pair(hydra.core.PairType(hydra.core.Type.variable("hydra.packaging.Namespace"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("n"))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The namespace in focus, together with its associated value")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("mapping", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.map(hydra.core.MapType(hydra.core.Type.variable("hydra.packaging.Namespace"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.variable("n"))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A mapping of namespaces to values")))))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A mapping from namespaces to values of type n, with a focus on one namespace"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.packaging.Package" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("name",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.packaging.PackageName"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The name of the package")))))),
   hydra.core.FieldType("modules", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.variable("hydra.packaging.Module")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The modules in this package")))))),
   hydra.core.FieldType("dependencies", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.list(hydra.core.Type.variable("hydra.packaging.PackageName")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The packages which this package depends on")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("description", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.maybe(hydra.core.Type.literal(hydra.core.LiteralType.string)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An optional human-readable description of the package")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A package, which is a named collection of modules with metadata and dependencies"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.packaging.PackageName" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.wrap(hydra.core.Type.literal(hydra.core.LiteralType.string)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The unique name of a package, e.g. \"hydra-kernel\" or \"hydra-python\""))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.packaging.QualifiedName" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("namespace",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.maybe(hydra.core.Type.variable("hydra.packaging.Namespace")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The optional namespace")))))),
   hydra.core.FieldType("local", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.string),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The local name")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A qualified name consisting of an optional namespace together with a mandatory local name"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.packaging.TermDefinition" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("name",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The name of the term")))))),
   hydra.core.FieldType("term", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Term"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The term being defined")))))),
   hydra.core.FieldType("type", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.maybe(hydra.core.Type.variable("hydra.core.TypeScheme")),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type scheme of the term, including any class constraints")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A term-level definition, including a name, a term, and the type scheme of the term"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.packaging.TypeDefinition" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.record(Seq(hydra.core.FieldType("name",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.Name"),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The name of the type")))))),
   hydra.core.FieldType("type", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.variable("hydra.core.TypeScheme"),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("The type scheme being defined")))))))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A type-level definition, including a name and the type scheme"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.util.CaseConvention" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("camel",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.unit), hydra.core.FieldType("pascal", hydra.core.Type.unit), hydra.core.FieldType("lowerSnake",
   hydra.core.Type.unit), hydra.core.FieldType("upperSnake", hydra.core.Type.unit))),
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("A naming convention for symbols, such as camelCase or snake_case"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.util.Comparison" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("lessThan",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.unit), hydra.core.FieldType("equalTo", hydra.core.Type.unit), hydra.core.FieldType("greaterThan",
   hydra.core.Type.unit))), Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("An equality judgement: less than, equal to, or greater than"))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   "hydra.util.Precision" -> hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.union(Seq(hydra.core.FieldType("arbitrary",
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.unit, Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Arbitrary precision")))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   hydra.core.FieldType("bits", hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.core.Type.literal(hydra.core.LiteralType.integer(hydra.core.IntegerType.int32)),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Precision to a specified number of bits")))))))),
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   Map("description" -> hydra.core.Term.literal(hydra.core.Literal.string("Numeric precision: arbitrary precision, or precision to a specified number of bits"))))))
