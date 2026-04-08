// Note: this is an automatically generated file. Do not edit.

package hydra.test.checking;

/**
 * Algebraic type checking test cases: unit, pairs, eithers, optionals
 */
public interface AlgebraicTypes {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("Algebraic types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.AlgebraicTypes.unitTests(),
      hydra.test.checking.AlgebraicTypes.pairsTests(),
      hydra.test.checking.AlgebraicTypes.eithersTests(),
      hydra.test.checking.AlgebraicTypes.optionalsTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup basicPairsTests() {
    return new hydra.testing.TestGroup("Basic pairs", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("pair of int and string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("pair of string and boolean", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("test")), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("pair of boolean and int", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(100))))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup eithersInComplexContextsTests() {
    return new hydra.testing.TestGroup("Eithers in complex contexts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("either in list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Literal(new hydra.core.Literal.String_("error")))),
            new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("either in let binding", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("result"), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("result")))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup eithersTests() {
    return new hydra.testing.TestGroup("Eithers", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.AlgebraicTypes.leftValuesTests(),
      hydra.test.checking.AlgebraicTypes.rightValuesTests(),
      hydra.test.checking.AlgebraicTypes.polymorphicEithersTests(),
      hydra.test.checking.AlgebraicTypes.eithersInComplexContextsTests(),
      hydra.test.checking.AlgebraicTypes.nestedEithersTests(),
      hydra.test.checking.AlgebraicTypes.eithersWithComplexTypesTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup eithersWithComplexTypesTests() {
    return new hydra.testing.TestGroup("Eithers with complex types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("either with record on left", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypePersonName(), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
            new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Smith"))),
            new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonName()), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("either with record on right", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypePersonName(), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Bob"))),
            new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Jones"))),
            new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(25))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonName())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup leftValuesTests() {
    return new hydra.testing.TestGroup("Left values", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("left int", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("left string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Literal(new hydra.core.Literal.String_("error")))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("left boolean", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup monomorphicOptionalsTests() {
    return new hydra.testing.TestGroup("Monomorphic optionals", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("nothing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("just int", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), hydra.show.Core.type(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("just string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))))), hydra.show.Core.type(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("just boolean", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))), hydra.show.Core.type(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup nestedEithersTests() {
    return new hydra.testing.TestGroup("Nested eithers", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("either of either (left left)", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("either of either (left right)", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Literal(new hydra.core.Literal.String_("nested")))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))), new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("either of either (right)", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("either of list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("list of eithers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")))),
            new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))),
            new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup nestedOptionalsTests() {
    return new hydra.testing.TestGroup("Nested optionals", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("optional of optional", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("nested")))))))), hydra.show.Core.type(new hydra.core.Type.Maybe(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("optional of list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))))), hydra.show.Core.type(new hydra.core.Type.Maybe(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("list of optionals", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")))),
            new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
            new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup nestedPairsTests() {
    return new hydra.testing.TestGroup("Nested pairs", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("pair of pairs", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("one")))))), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))), new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("pair with list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("numbers")))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("list of pairs", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")))))),
            new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")))))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup optionalsInComplexContextsTests() {
    return new hydra.testing.TestGroup("Optionals in complex contexts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("optional in record", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypeBuddyListAName(), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("head"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("first"))),
            new hydra.core.Field(new hydra.core.Name("tail"), new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypeBuddyListBName(), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("head"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("second"))),
              new hydra.core.Field(new hydra.core.Name("tail"), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))))))))))))), hydra.show.Core.type(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeBuddyListAName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("optional in let binding", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("maybeValue"), new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("maybeValue")))))), hydra.show.Core.type(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup optionalsTests() {
    return new hydra.testing.TestGroup("Optionals", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.AlgebraicTypes.monomorphicOptionalsTests(),
      hydra.test.checking.AlgebraicTypes.polymorphicOptionalsTests(),
      hydra.test.checking.AlgebraicTypes.optionalsInComplexContextsTests(),
      hydra.test.checking.AlgebraicTypes.nestedOptionalsTests(),
      hydra.test.checking.AlgebraicTypes.optionalsWithComplexTypesTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup optionalsWithComplexTypesTests() {
    return new hydra.testing.TestGroup("Optionals with complex types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("optional map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
      hydra.Inference.inferTypeOf(
        hydra.test.TestGraph.testContext(),
        hydra.test.TestGraph.testGraph(),
        new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("key")),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))))), hydra.show.Core.type(new hydra.core.Type.Maybe(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup pairsInComplexContextsTests() {
    return new hydra.testing.TestGroup("Pairs in complex contexts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("pair in list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("one")))))),
            new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("two")))))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("pair in let binding", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("result"), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("answer")))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("result")))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup pairsTests() {
    return new hydra.testing.TestGroup("Pairs", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.AlgebraicTypes.basicPairsTests(),
      hydra.test.checking.AlgebraicTypes.polymorphicPairsTests(),
      hydra.test.checking.AlgebraicTypes.pairsInComplexContextsTests(),
      hydra.test.checking.AlgebraicTypes.nestedPairsTests(),
      hydra.test.checking.AlgebraicTypes.pairsWithComplexTypesTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup pairsWithComplexTypesTests() {
    return new hydra.testing.TestGroup("Pairs with complex types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("pair with record on first", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Person"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
            new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Smith"))),
            new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Variable(new hydra.core.Name("Person")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("pair with record on second", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("name")), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Person"), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Bob"))),
            new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Jones"))),
            new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(25))))))))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Variable(new hydra.core.Name("Person"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup polymorphicEithersTests() {
    return new hydra.testing.TestGroup("Polymorphic eithers", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("left from lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("right from lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("t1")), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("either from two lambdas", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("flag"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.ifElse")), new hydra.core.Term.Variable(new hydra.core.Name("flag")))), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("x")))))), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup polymorphicOptionalsTests() {
    return new hydra.testing.TestGroup("Polymorphic optionals", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("optional from lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("nothing from lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("t1")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("conditional optional", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("flag"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.ifElse")), new hydra.core.Term.Variable(new hydra.core.Name("flag")))), new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Variable(new hydra.core.Name("x")))))), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup polymorphicPairsTests() {
    return new hydra.testing.TestGroup("Polymorphic pairs", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("pair from lambda (first element)", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("constant"))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("pair from lambda (second element)", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("constant")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("pair from two lambdas", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("y")))))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t1")), new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("pair with repeated variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup rightValuesTests() {
    return new hydra.testing.TestGroup("Right values", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("right int", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("right string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Literal(new hydra.core.Literal.String_("success")))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("right boolean", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup unitTermInPolymorphicContextTests() {
    return new hydra.testing.TestGroup("Unit term in polymorphic context", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("unit from lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
      hydra.Inference.inferTypeOf(
        hydra.test.TestGraph.testContext(),
        hydra.test.TestGraph.testGraph(),
        new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Unit()))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Unit()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup unitTermTests() {
    return new hydra.testing.TestGroup("Unit term", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("unit literal", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
      hydra.Inference.inferTypeOf(
        hydra.test.TestGraph.testContext(),
        hydra.test.TestGraph.testGraph(),
        new hydra.core.Term.Unit())), hydra.show.Core.type(new hydra.core.Type.Unit()))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup unitTests() {
    return new hydra.testing.TestGroup("Unit", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.AlgebraicTypes.unitTermTests(),
      hydra.test.checking.AlgebraicTypes.unitTermInPolymorphicContextTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}
