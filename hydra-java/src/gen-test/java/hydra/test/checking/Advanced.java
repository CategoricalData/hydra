// Note: this is an automatically generated file. Do not edit.

package hydra.test.checking;

/**
 * Advanced type checking test cases: annotated terms and flows
 */
public interface Advanced {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("Advanced", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(hydra.test.checking.Advanced.annotatedTermsTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup annotatedTermsTests() {
    return new hydra.testing.TestGroup("Annotated terms", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.Advanced.topLevelAnnotationsTests(),
      hydra.test.checking.Advanced.nestedAnnotationsTests(),
      hydra.test.checking.Advanced.annotationsInComplexContextsTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup annotationsInComplexContextsTests() {
    return new hydra.testing.TestGroup("Annotations in complex contexts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("annotated let binding", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
            new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
            new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("world")), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("y")))))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("annotated record fields", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypePersonName(), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice")), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap()))))),
            new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("Smith")), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap()))))),
            new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))))))))), hydra.show.Core.type(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonName())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("annotated function in application", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("add"), new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("add")), new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))))), new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(20))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup nestedAnnotationsTests() {
    return new hydra.testing.TestGroup("Nested annotations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("annotation within annotation", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(100))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("annotated terms in tuple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))), new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("annotated term in function application", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))), new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup topLevelAnnotationsTests() {
    return new hydra.testing.TestGroup("Top-level annotations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("annotated literal", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("annotated list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("annotated record", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypePersonName(), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("John"))),
            new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Doe"))),
            new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(25))))))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))))), hydra.show.Core.type(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonName())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("annotated lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Collections.<hydra.core.Name, hydra.core.Term>emptyMap())))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }
}
