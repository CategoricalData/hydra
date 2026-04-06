// Note: this is an automatically generated file. Do not edit.

package hydra.json;

/**
 * A module which provides a minimal typing environment for decoding other modules from JSON. This avoids certain problems with generating entire source modules into target languages like Java, which is subject to method size limits for large modules like hydra.core.
 */
public interface Bootstrap {
  static java.util.Map<hydra.core.Name, hydra.core.Type> typesByName() {
    return new java.util.TreeMap(java.util.Map.ofEntries(
      java.util.Map.entry(
        new hydra.core.Name("hydra.coders.Adapter"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t2"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("v1"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("v2"), new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("isLossy"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("Whether information may be lost in the course of this adaptation")))))))),
          new hydra.core.FieldType(new hydra.core.Name("source"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("t1")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The source type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("target"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("t2")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The target type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("coder"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.coders.Coder")), new hydra.core.Type.Variable(new hydra.core.Name("v1")))), new hydra.core.Type.Variable(new hydra.core.Name("v2")))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The coder for transforming instances of the source type to instances of the target type")))))))))))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A two-level bidirectional encoder which adapts types to types and terms to terms")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.coders.AdapterContext"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("graph"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.graph.Graph")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The underlying graph of elements and primitives")))))))),
          new hydra.core.FieldType(new hydra.core.Name("language"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.coders.Language")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The language being encoded or decoded")))))))),
          new hydra.core.FieldType(new hydra.core.Name("adapters"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.coders.Adapter")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")))), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")))), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")))), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A map of type names to adapters for those types")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("An evaluation context together with a source language and a target language")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.coders.Bicoder"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t2"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("v1"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("v2"), new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("encode"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t1")), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.coders.Adapter")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))), new hydra.core.Type.Variable(new hydra.core.Name("t2")))), new hydra.core.Type.Variable(new hydra.core.Name("v1")))), new hydra.core.Type.Variable(new hydra.core.Name("v2")))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A function from source types to adapters")))))))),
          new hydra.core.FieldType(new hydra.core.Name("decode"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t2")), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.coders.Adapter")), new hydra.core.Type.Variable(new hydra.core.Name("t2")))), new hydra.core.Type.Variable(new hydra.core.Name("t1")))), new hydra.core.Type.Variable(new hydra.core.Name("v2")))), new hydra.core.Type.Variable(new hydra.core.Name("v1")))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A function from target types to adapters")))))))))))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A two-level encoder and decoder, operating both at a type level and an instance (data) level")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.coders.Coder"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("v1"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("v2"), new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("encode"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.context.Context")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("v1")), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.context.InContext")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.errors.Error")))), new hydra.core.Type.Variable(new hydra.core.Name("v2")))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A function which encodes source values as target values in a given context")))))))),
          new hydra.core.FieldType(new hydra.core.Name("decode"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.context.Context")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("v2")), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.context.InContext")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.errors.Error")))), new hydra.core.Type.Variable(new hydra.core.Name("v1")))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A function which decodes target values as source values in a given context")))))))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("An encoder and decoder; a bidirectional transformation between two types")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.coders.CoderDirection"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("encode"), new hydra.core.Type.Unit()),
          new hydra.core.FieldType(new hydra.core.Name("decode"), new hydra.core.Type.Unit()))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("Indicates either the 'out' or the 'in' direction of a coder")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.coders.Language"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.coders.LanguageName")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The unique name of the language")))))))),
          new hydra.core.FieldType(new hydra.core.Name("constraints"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.coders.LanguageConstraints")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The constraints which characterize the language")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A named language together with language-specific constraints")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.coders.LanguageConstraints"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("eliminationVariants"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("hydra.variants.EliminationVariant"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("All supported elimination variants")))))))),
          new hydra.core.FieldType(new hydra.core.Name("literalVariants"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("hydra.variants.LiteralVariant"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("All supported literal variants")))))))),
          new hydra.core.FieldType(new hydra.core.Name("floatTypes"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.FloatType"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("All supported float types")))))))),
          new hydra.core.FieldType(new hydra.core.Name("functionVariants"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("hydra.variants.FunctionVariant"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("All supported function variants")))))))),
          new hydra.core.FieldType(new hydra.core.Name("integerTypes"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.IntegerType"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("All supported integer types")))))))),
          new hydra.core.FieldType(new hydra.core.Name("termVariants"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("hydra.variants.TermVariant"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("All supported term variants")))))))),
          new hydra.core.FieldType(new hydra.core.Name("typeVariants"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("hydra.variants.TypeVariant"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("All supported type variants")))))))),
          new hydra.core.FieldType(new hydra.core.Name("types"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A logical set of types, as a predicate which tests a type for inclusion")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A set of constraints on valid type and term expressions, characterizing a language")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.coders.LanguageName"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Wrap(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("The unique name of a language")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.coders.SymmetricAdapter"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("v"), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.coders.Adapter")), new hydra.core.Type.Variable(new hydra.core.Name("t")))), new hydra.core.Type.Variable(new hydra.core.Name("t")))), new hydra.core.Type.Variable(new hydra.core.Name("v")))), new hydra.core.Type.Variable(new hydra.core.Name("v")))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A bidirectional encoder which maps between the same type and term languages on either side")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.coders.TraversalOrder"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("pre"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("Pre-order traversal")))))))),
          new hydra.core.FieldType(new hydra.core.Name("post"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("Post-order traversal")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("Specifies either a pre-order or post-order traversal")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.coders.TypeAdapter"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.coders.AdapterContext")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.coders.SymmetricAdapter")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")))), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A function which maps a Hydra type to a symmetric adapter between types and terms")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.context.Context"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("trace"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A stack of context labels describing the current execution path")))))))),
          new hydra.core.FieldType(new hydra.core.Name("messages"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A log of warnings and/or info messages")))))))),
          new hydra.core.FieldType(new hydra.core.Name("other"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A map of string keys to arbitrary terms as values, for application-specific use")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("An execution context for tracing and diagnostics, threaded through function calls")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.context.InContext"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("e"), new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("object"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("e")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A domain object; typically an error")))))))),
          new hydra.core.FieldType(new hydra.core.Name("context"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.context.Context")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The execution context at the point of capture")))))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A particular domain object (such as an error) together with an execution context")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.AnnotatedTerm"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("body"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The term being annotated")))))))),
          new hydra.core.FieldType(new hydra.core.Name("annotation"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The annotation as a map from keys to values")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A term together with an annotation")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.AnnotatedType"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("body"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type being annotated")))))))),
          new hydra.core.FieldType(new hydra.core.Name("annotation"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The annotation as a map from keys to values")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A type together with an annotation")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.Application"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("function"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The left-hand side of the application")))))))),
          new hydra.core.FieldType(new hydra.core.Name("argument"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The right-hand side of the application")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A term which applies a function to an argument")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.ApplicationType"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("function"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The left-hand side of the application")))))))),
          new hydra.core.FieldType(new hydra.core.Name("argument"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The right-hand side of the application")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type-level analog of an application term")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.Binding"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The name of the bound variable")))))))),
          new hydra.core.FieldType(new hydra.core.Name("term"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The term to which the variable is bound")))))))),
          new hydra.core.FieldType(new hydra.core.Name("type"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.TypeScheme"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The optional type of the bound term")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A field with an optional type scheme, used to bind variables to terms in a 'let' expression")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.CaseStatement"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("typeName"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The name of the union type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("default"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An optional default case, used if none of the explicit cases match")))))))),
          new hydra.core.FieldType(new hydra.core.Name("cases"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Field"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A list of case alternatives, one per union field")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A union elimination; a case statement")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.EitherType"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("left"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The 'left' alternative")))))))),
          new hydra.core.FieldType(new hydra.core.Name("right"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The 'right' alternative")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A type which provides a choice between a 'left' type and a 'right' type")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.Elimination"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("record"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Projection")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("Eliminates a record by projecting a given field")))))))),
          new hydra.core.FieldType(new hydra.core.Name("union"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.CaseStatement")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("Eliminates a union term by matching over the fields of the union. This is a case statement.")))))))),
          new hydra.core.FieldType(new hydra.core.Name("wrap"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("Unwrap a wrapped term")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A corresponding elimination for an introduction term")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.Field"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The name of the field")))))))),
          new hydra.core.FieldType(new hydra.core.Name("term"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The term value of the field")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A name/term pair")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.FieldType"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The name of the field")))))))),
          new hydra.core.FieldType(new hydra.core.Name("type"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type of the field")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A name/type pair")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.FloatType"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("bigfloat"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An arbitrary-precision floating-point type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("float32"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 32-bit floating-point type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("float64"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 64-bit floating-point type")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A floating-point type")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.FloatValue"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("bigfloat"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Bigfloat())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An arbitrary-precision floating-point value")))))))),
          new hydra.core.FieldType(new hydra.core.Name("float32"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 32-bit floating-point value")))))))),
          new hydra.core.FieldType(new hydra.core.Name("float64"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float64())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 64-bit floating-point value")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A floating-point literal value")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.ForallType"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("parameter"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The variable which is bound by the lambda")))))))),
          new hydra.core.FieldType(new hydra.core.Name("body"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The body of the lambda")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A universally quantified type; the System F equivalent of a type scheme, and the type-level equivalent of a lambda term.")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.Function"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("elimination"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Elimination")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An elimination for any of a few term variants")))))))),
          new hydra.core.FieldType(new hydra.core.Name("lambda"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Lambda")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A function abstraction (lambda)")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A function")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.FunctionType"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("domain"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The domain (input) type of the function")))))))),
          new hydra.core.FieldType(new hydra.core.Name("codomain"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The codomain (output) type of the function")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A function type, also known as an arrow type")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.Injection"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("typeName"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The name of the union type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("field"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Field")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The field being injected, including its name and value")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("An instance of a union type; i.e. a string-indexed generalization of inl() or inr()")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.IntegerType"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("bigint"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An arbitrary-precision integer type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("int8"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An 8-bit signed integer type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("int16"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 16-bit signed integer type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("int32"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 32-bit signed integer type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("int64"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 64-bit signed integer type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("uint8"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An 8-bit unsigned integer type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("uint16"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 16-bit unsigned integer type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("uint32"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 32-bit unsigned integer type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("uint64"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 64-bit unsigned integer type")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("An integer type")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.IntegerValue"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("bigint"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Bigint())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An arbitrary-precision integer value")))))))),
          new hydra.core.FieldType(new hydra.core.Name("int8"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int8())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An 8-bit signed integer value")))))))),
          new hydra.core.FieldType(new hydra.core.Name("int16"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int16())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 16-bit signed integer value (short value)")))))))),
          new hydra.core.FieldType(new hydra.core.Name("int32"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 32-bit signed integer value (int value)")))))))),
          new hydra.core.FieldType(new hydra.core.Name("int64"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int64())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 64-bit signed integer value (long value)")))))))),
          new hydra.core.FieldType(new hydra.core.Name("uint8"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint8())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An 8-bit unsigned integer value (byte)")))))))),
          new hydra.core.FieldType(new hydra.core.Name("uint16"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint16())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 16-bit unsigned integer value")))))))),
          new hydra.core.FieldType(new hydra.core.Name("uint32"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint32())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 32-bit unsigned integer value (unsigned int)")))))))),
          new hydra.core.FieldType(new hydra.core.Name("uint64"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint64())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 64-bit unsigned integer value (unsigned long)")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("An integer literal value")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.Lambda"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("parameter"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The parameter of the lambda")))))))),
          new hydra.core.FieldType(new hydra.core.Name("domain"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An optional domain type for the lambda")))))))),
          new hydra.core.FieldType(new hydra.core.Name("body"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The body of the lambda")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A function abstraction (lambda)")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.Let"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("bindings"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Binding"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The list of variable bindings")))))))),
          new hydra.core.FieldType(new hydra.core.Name("body"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The body term in which the variables are bound")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A set of (possibly recursive) 'let' bindings together with a body in which they are bound")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.Literal"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("binary"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Binary()), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A binary literal")))))))),
          new hydra.core.FieldType(new hydra.core.Name("boolean"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A boolean literal")))))))),
          new hydra.core.FieldType(new hydra.core.Name("float"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.FloatValue")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A floating-point literal")))))))),
          new hydra.core.FieldType(new hydra.core.Name("integer"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.IntegerValue")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An integer literal")))))))),
          new hydra.core.FieldType(new hydra.core.Name("string"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A string literal")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A term constant; an instance of a literal type")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.LiteralType"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("binary"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type of a binary (byte string) value")))))))),
          new hydra.core.FieldType(new hydra.core.Name("boolean"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type of a boolean (true/false) value")))))))),
          new hydra.core.FieldType(new hydra.core.Name("float"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.FloatType")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type of a floating-point value")))))))),
          new hydra.core.FieldType(new hydra.core.Name("integer"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.IntegerType")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type of an integer value")))))))),
          new hydra.core.FieldType(new hydra.core.Name("string"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type of a string value")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.MapType"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("keys"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type of keys in the map")))))))),
          new hydra.core.FieldType(new hydra.core.Name("values"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type of values in the map")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A map type")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.Name"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Wrap(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A unique identifier in some context; a string-valued key")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.PairType"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("first"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The first component of the pair")))))))),
          new hydra.core.FieldType(new hydra.core.Name("second"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The second component of the pair")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A type which pairs a 'first' type and a 'second' type")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.Projection"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("typeName"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The name of the record type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("field"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The name of the projected field")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A record elimination; a projection")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.Record"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("typeName"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The name of the record type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("fields"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Field"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The fields of the record, as a list of name/term pairs")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A record, or labeled tuple; a map of field names to terms")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.Term"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("annotated"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.AnnotatedTerm")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A term annotated with metadata")))))))),
          new hydra.core.FieldType(new hydra.core.Name("application"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Application")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A function application")))))))),
          new hydra.core.FieldType(new hydra.core.Name("either"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An either value")))))))),
          new hydra.core.FieldType(new hydra.core.Name("function"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Function")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A function term")))))))),
          new hydra.core.FieldType(new hydra.core.Name("let"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Let")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A 'let' term, which binds variables to terms")))))))),
          new hydra.core.FieldType(new hydra.core.Name("list"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A list")))))))),
          new hydra.core.FieldType(new hydra.core.Name("literal"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Literal")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A literal value")))))))),
          new hydra.core.FieldType(new hydra.core.Name("map"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A map of keys to values")))))))),
          new hydra.core.FieldType(new hydra.core.Name("maybe"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An optional value")))))))),
          new hydra.core.FieldType(new hydra.core.Name("pair"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A pair (2-tuple)")))))))),
          new hydra.core.FieldType(new hydra.core.Name("record"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Record")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A record term")))))))),
          new hydra.core.FieldType(new hydra.core.Name("set"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A set of values")))))))),
          new hydra.core.FieldType(new hydra.core.Name("typeApplication"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.TypeApplicationTerm")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A System F type application term")))))))),
          new hydra.core.FieldType(new hydra.core.Name("typeLambda"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.TypeLambda")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A System F type abstraction term")))))))),
          new hydra.core.FieldType(new hydra.core.Name("union"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Injection")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An injection; an instance of a union type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("unit"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A unit value; a term with no value")))))))),
          new hydra.core.FieldType(new hydra.core.Name("variable"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A variable reference")))))))),
          new hydra.core.FieldType(new hydra.core.Name("wrap"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.WrappedTerm")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A wrapped term; an instance of a wrapper type (newtype)")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A data term")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.Type"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("annotated"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.AnnotatedType")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An annotated type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("application"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.ApplicationType")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A type application")))))))),
          new hydra.core.FieldType(new hydra.core.Name("either"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.EitherType")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An either (sum) type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("forall"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.ForallType")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A universally quantified (polymorphic) type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("function"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.FunctionType")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A function type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("list"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A list type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("literal"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.LiteralType")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A literal type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("map"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.MapType")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A map type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("maybe"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An optional type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("pair"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.PairType")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A pair (2-tuple) type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("record"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.FieldType"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A record type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("set"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A set type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("union"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.FieldType"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A union type with field names")))))))),
          new hydra.core.FieldType(new hydra.core.Name("unit"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The unit type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("variable"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A type variable")))))))),
          new hydra.core.FieldType(new hydra.core.Name("void"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The void (uninhabited, or bottom) type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("wrap"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A wrapped type (newtype)")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A data type")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.TypeApplicationTerm"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("body"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The term being applied to a type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("type"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type argument")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A term applied to a type; a type application")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.TypeLambda"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("parameter"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type variable introduced by the abstraction")))))))),
          new hydra.core.FieldType(new hydra.core.Name("body"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The body of the abstraction")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A System F type abstraction term")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.TypeScheme"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("variables"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The free type variables")))))))),
          new hydra.core.FieldType(new hydra.core.Name("type"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type expression")))))))),
          new hydra.core.FieldType(new hydra.core.Name("constraints"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Maybe(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.TypeVariableMetadata"))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("Optional metadata for type variables, including typeclass constraints. The map keys are type variable names.")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A type expression together with free type variables occurring in the expression")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.TypeVariableMetadata"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(new hydra.core.FieldType(new hydra.core.Name("classes"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("The set of typeclass constraints on this type variable")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("Metadata associated with a type variable, including typeclass constraints")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.core.WrappedTerm"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("typeName"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The name of the wrapper type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("body"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The wrapped term")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A term wrapped in a type name")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.errors.DecodingError"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Wrap(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("An error that occurred during decoding of a term")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.errors.Error"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("checking"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.error.checking.CheckingError")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A type checking error")))))))),
          new hydra.core.FieldType(new hydra.core.Name("decoding"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.errors.DecodingError")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An error that occurred during decoding of a term")))))))),
          new hydra.core.FieldType(new hydra.core.Name("duplicateBinding"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.error.core.DuplicateBindingError")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A duplicate binding name error")))))))),
          new hydra.core.FieldType(new hydra.core.Name("duplicateField"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.error.core.DuplicateFieldError")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A duplicate field name error")))))))),
          new hydra.core.FieldType(new hydra.core.Name("other"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.errors.OtherError")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("Any other error")))))))),
          new hydra.core.FieldType(new hydra.core.Name("undefinedField"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.error.core.UndefinedFieldError")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A reference to an undefined field")))))))),
          new hydra.core.FieldType(new hydra.core.Name("undefinedTermVariable"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.error.core.UndefinedTermVariableError")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A reference to an undefined term variable")))))))),
          new hydra.core.FieldType(new hydra.core.Name("untypedTermVariable"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.error.core.UntypedTermVariableError")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A term variable whose type is not known")))))))),
          new hydra.core.FieldType(new hydra.core.Name("unexpectedTermVariant"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An unexpected term variant")))))))),
          new hydra.core.FieldType(new hydra.core.Name("unexpectedTypeVariant"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An unexpected type variant")))))))),
          new hydra.core.FieldType(new hydra.core.Name("unification"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.errors.UnificationError")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A type unification error")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("An error of any kind, with kernel errors particularly differentiated")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.errors.OtherError"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Wrap(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("Any other error")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.errors.UnificationError"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("leftType"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The left-hand type in the unification")))))))),
          new hydra.core.FieldType(new hydra.core.Name("rightType"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The right-hand type in the unification")))))))),
          new hydra.core.FieldType(new hydra.core.Name("message"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A human-readable error message")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("An error that occurred during type unification")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.graph.Graph"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("boundTerms"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The terms bound by all term variables in scope")))))))),
          new hydra.core.FieldType(new hydra.core.Name("boundTypes"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.TypeScheme")))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type schemes of all term variables in scope")))))))),
          new hydra.core.FieldType(new hydra.core.Name("classConstraints"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.TypeVariableMetadata")))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A mutable map from type variable names to their accumulated class constraints. This is populated during type inference when operations requiring Eq or Ord are encountered.")))))))),
          new hydra.core.FieldType(new hydra.core.Name("lambdaVariables"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The set of term variables introduced by specifically by lambdas")))))))),
          new hydra.core.FieldType(new hydra.core.Name("metadata"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("Any additional metadata bound to term variables in scope")))))))),
          new hydra.core.FieldType(new hydra.core.Name("primitives"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.graph.Primitive")))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("All primitive functions and constants by name")))))))),
          new hydra.core.FieldType(new hydra.core.Name("schemaTypes"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.TypeScheme")))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("All schema types (type schemes) in scope")))))))),
          new hydra.core.FieldType(new hydra.core.Name("typeVariables"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The set of type variables introduced specifically by type lambdas")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A graph, or lexical environment which binds names to terms, types, primitives, and metadata")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.graph.Primitive"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The unique name of the primitive function")))))))),
          new hydra.core.FieldType(new hydra.core.Name("type"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.TypeScheme")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type signature of the primitive function")))))))),
          new hydra.core.FieldType(new hydra.core.Name("implementation"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.context.Context")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.graph.Graph")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term"))), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.context.InContext")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.errors.Error")))), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A concrete implementation of the primitive function. The Context and Graph parameters are needed by higher-order primitives (e.g. lists.map, lists.foldl, eithers.bind) which must evaluate function arguments via term reduction; the Graph provides variable and primitive bindings, while the Context supports tracing and error reporting.")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A built-in function or constant")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.graph.TermCoder"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("type"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The Hydra type of encoded terms")))))))),
          new hydra.core.FieldType(new hydra.core.Name("encode"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.context.Context")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.graph.Graph")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.context.InContext")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.errors.Error")))), new hydra.core.Type.Variable(new hydra.core.Name("a")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An encode function from terms to native values")))))))),
          new hydra.core.FieldType(new hydra.core.Name("decode"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.context.Context")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.context.InContext")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.errors.Error")))), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A decode function from native values to terms")))))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms.")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.packaging.Definition"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("term"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.packaging.TermDefinition")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A term definition")))))))),
          new hydra.core.FieldType(new hydra.core.Name("type"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.packaging.TypeDefinition")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A type definition")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A definition, which may be either a term or type definition")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.packaging.FileExtension"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Wrap(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A file extension (without the dot), e.g. \"json\" or \"py\"")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.packaging.Library"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("namespace"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.packaging.Namespace")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A common prefix for all primitive function names in the library")))))))),
          new hydra.core.FieldType(new hydra.core.Name("prefix"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A preferred namespace prefix for function names in the library")))))))),
          new hydra.core.FieldType(new hydra.core.Name("primitives"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.graph.Primitive"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The primitives defined in this library")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A library of primitive functions")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.packaging.Module"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("namespace"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.packaging.Namespace")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A common prefix for all element names in the module")))))))),
          new hydra.core.FieldType(new hydra.core.Name("definitions"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.packaging.Definition"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The definitions in this module")))))))),
          new hydra.core.FieldType(new hydra.core.Name("termDependencies"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.packaging.Namespace"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("Any modules which the term expressions of this module directly depend upon")))))))),
          new hydra.core.FieldType(new hydra.core.Name("typeDependencies"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.packaging.Namespace"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("Any modules which the type expressions of this module directly depend upon")))))))),
          new hydra.core.FieldType(new hydra.core.Name("description"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An optional human-readable description of the module")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A logical collection of elements in the same namespace, having dependencies on zero or more other modules")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.packaging.Namespace"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Wrap(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A prefix for element names")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.packaging.Namespaces"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("n"), new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("focus"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.packaging.Namespace")), new hydra.core.Type.Variable(new hydra.core.Name("n")))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The namespace in focus, together with its associated value")))))))),
          new hydra.core.FieldType(new hydra.core.Name("mapping"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.packaging.Namespace")), new hydra.core.Type.Variable(new hydra.core.Name("n")))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("A mapping of namespaces to values")))))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A mapping from namespaces to values of type n, with a focus on one namespace")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.packaging.Package"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.packaging.PackageName")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The name of the package")))))))),
          new hydra.core.FieldType(new hydra.core.Name("modules"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.packaging.Module"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The modules in this package")))))))),
          new hydra.core.FieldType(new hydra.core.Name("dependencies"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.packaging.PackageName"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The packages which this package depends on")))))))),
          new hydra.core.FieldType(new hydra.core.Name("description"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("An optional human-readable description of the package")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A package, which is a named collection of modules with metadata and dependencies")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.packaging.PackageName"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Wrap(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("The unique name of a package, e.g. \"hydra-kernel\" or \"hydra-python\"")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.packaging.QualifiedName"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("namespace"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.packaging.Namespace"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The optional namespace")))))))),
          new hydra.core.FieldType(new hydra.core.Name("local"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The local name")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A qualified name consisting of an optional namespace together with a mandatory local name")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.packaging.TermDefinition"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The name of the term")))))))),
          new hydra.core.FieldType(new hydra.core.Name("term"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Term")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The term being defined")))))))),
          new hydra.core.FieldType(new hydra.core.Name("type"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.TypeScheme"))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type scheme of the term, including any class constraints")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A term-level definition, including a name, a term, and the type scheme of the term")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.packaging.TypeDefinition"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Record(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Name")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The name of the type")))))))),
          new hydra.core.FieldType(new hydra.core.Name("type"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.TypeScheme")), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("The type scheme being defined")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A type-level definition, including a name and the type scheme")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.util.CaseConvention"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("camel"), new hydra.core.Type.Unit()),
          new hydra.core.FieldType(new hydra.core.Name("pascal"), new hydra.core.Type.Unit()),
          new hydra.core.FieldType(new hydra.core.Name("lowerSnake"), new hydra.core.Type.Unit()),
          new hydra.core.FieldType(new hydra.core.Name("upperSnake"), new hydra.core.Type.Unit()))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("A naming convention for symbols, such as camelCase or snake_case")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.util.Comparison"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("lessThan"), new hydra.core.Type.Unit()),
          new hydra.core.FieldType(new hydra.core.Name("equalTo"), new hydra.core.Type.Unit()),
          new hydra.core.FieldType(new hydra.core.Name("greaterThan"), new hydra.core.Type.Unit()))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("An equality judgement: less than, equal to, or greater than")))))))),
      java.util.Map.entry(
        new hydra.core.Name("hydra.util.Precision"),
        new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Union(java.util.Arrays.asList(
          new hydra.core.FieldType(new hydra.core.Name("arbitrary"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Unit(), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("Arbitrary precision")))))))),
          new hydra.core.FieldType(new hydra.core.Name("bits"), new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Name("description"),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("Precision to a specified number of bits")))))))))), new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("description"),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("Numeric precision: arbitrary precision, or precision to a specified number of bits"))))))))));
  }
}
