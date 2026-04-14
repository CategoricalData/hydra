// Note: this is an automatically generated file. Do not edit.

package hydra.test.checking;

/**
 * Collection type checking test cases: lists, sets, maps
 */
public interface Collections {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("Collections", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.Collections.listsTests(),
      hydra.test.checking.Collections.setsTests(),
      hydra.test.checking.Collections.mapsTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup emptyListsTests() {
    return new hydra.testing.TestGroup("Empty lists", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("pair of empty lists", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t1")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("empty list in tuple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())), new hydra.core.Term.Literal(new hydra.core.Literal.String_("context")))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup listsInComplexContextsTests() {
    return new hydra.testing.TestGroup("Lists in complex contexts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("multiple lists in tuple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
      hydra.Inference.inferTypeOf(
        hydra.test.TestGraph.testContext(),
        hydra.test.TestGraph.testGraph(),
        new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))), new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")))))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))), new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup listsOfLiteralsTests() {
    return new hydra.testing.TestGroup("Lists of literals", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("int list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("string list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("world")))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("single element list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Bigint(new java.math.BigInteger("42")))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Bigint())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("mixed numeric types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (1.0)))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (2.5)))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (3.140000104904175)))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup listsTests() {
    return new hydra.testing.TestGroup("Lists", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.Collections.listsOfLiteralsTests(),
      hydra.test.checking.Collections.emptyListsTests(),
      hydra.test.checking.Collections.polymorphicListsTests(),
      hydra.test.checking.Collections.nestedListsTests(),
      hydra.test.checking.Collections.listsInComplexContextsTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup mapsInComplexContextsTests() {
    return new hydra.testing.TestGroup("Maps in complex contexts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("map in tuple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("one")))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("context")))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("nested maps", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("outer")),
            new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))))))))), hydra.show.Core.type(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("map in let binding", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("lookup"), new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("key1")),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(100)))),
            java.util.Map.entry(
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("key2")),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(200))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("lookup")))))), hydra.show.Core.type(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup mapsTests() {
    return new hydra.testing.TestGroup("Maps", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.Collections.monomorphicMapsTests(),
      hydra.test.checking.Collections.polymorphicMapsTests(),
      hydra.test.checking.Collections.mapsInComplexContextsTests(),
      hydra.test.checking.Collections.mapsWithComplexTypesTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup mapsWithComplexTypesTests() {
    return new hydra.testing.TestGroup("Maps with complex types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("map of records", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("person1")),
            new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypePersonName(), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
              new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Smith"))),
              new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(25))))))))))))), hydra.show.Core.type(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonName())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("map of lists", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
              new hydra.core.Term.List(java.util.Arrays.asList(
                new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")),
                new hydra.core.Term.Literal(new hydra.core.Literal.String_("b"))))),
            java.util.Map.entry(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
              new hydra.core.Term.List(java.util.Arrays.asList(
                new hydra.core.Term.Literal(new hydra.core.Literal.String_("c")),
                new hydra.core.Term.Literal(new hydra.core.Literal.String_("d")))))))))), hydra.show.Core.type(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("map of tuples", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("coords")),
            new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(20))))))))))))), hydra.show.Core.type(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup monomorphicMapsTests() {
    return new hydra.testing.TestGroup("Monomorphic maps", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Map((java.util.Map<hydra.core.Term, hydra.core.Term>) ((java.util.Map<hydra.core.Term, hydra.core.Term>) (java.util.Collections.<hydra.core.Term, hydra.core.Term>emptyMap()))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("int to string map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("one"))),
            java.util.Map.entry(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("two")))))))), hydra.show.Core.type(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("string to int map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))),
            java.util.Map.entry(
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))))), hydra.show.Core.type(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("single entry map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Bigint(new java.math.BigInteger("42")))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))))), hydra.show.Core.type(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Bigint())), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup monomorphicSetsTests() {
    return new hydra.testing.TestGroup("Monomorphic sets", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Set((java.util.Set<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptySet())))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("int set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3)))))))), hydra.show.Core.type(new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("string set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("apple")),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("banana")),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("cherry"))))))), hydra.show.Core.type(new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("single element set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))))))), hydra.show.Core.type(new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup nestedListsTests() {
    return new hydra.testing.TestGroup("Nested lists", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("list of lists", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))),
            new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("empty nested lists", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())),
            new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.List(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("nested polymorphic", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.List(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup nestedSetsTests() {
    return new hydra.testing.TestGroup("Nested sets", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("set of lists", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
            new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")),
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")))),
            new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("c")),
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("d"))))))))), hydra.show.Core.type(new hydra.core.Type.Set(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("set of tuples", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
            new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))),
            new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(4)))))))))))), hydra.show.Core.type(new hydra.core.Type.Set(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("set of sets", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(new hydra.core.Term.Literal(new hydra.core.Literal.String_("nested")))))))))), hydra.show.Core.type(new hydra.core.Type.Set(new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup polymorphicListsTests() {
    return new hydra.testing.TestGroup("Polymorphic lists", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("list from lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Variable(new hydra.core.Name("x")))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("list with repeated var", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Variable(new hydra.core.Name("x")),
            new hydra.core.Term.Variable(new hydra.core.Name("x")))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("list from two lambdas", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Variable(new hydra.core.Name("x")),
            new hydra.core.Term.Variable(new hydra.core.Name("y")))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup polymorphicMapsTests() {
    return new hydra.testing.TestGroup("Polymorphic maps", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("map from lambda keys", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("k"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Term.Variable(new hydra.core.Name("k")),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("value")))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("map from lambda values", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("key")),
            new hydra.core.Term.Variable(new hydra.core.Name("v")))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("map from lambda both", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("k"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Term.Variable(new hydra.core.Name("k")),
            new hydra.core.Term.Variable(new hydra.core.Name("v")))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t1"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t1")), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("map with repeated variables", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Term.Variable(new hydra.core.Name("x")),
            new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup polymorphicSetsTests() {
    return new hydra.testing.TestGroup("Polymorphic sets", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("set from lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("set with repeated variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("set from two variables", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
            new hydra.core.Term.Variable(new hydra.core.Name("x")),
            new hydra.core.Term.Variable(new hydra.core.Name("y"))))))))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t0"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup setsInComplexContextsTests() {
    return new hydra.testing.TestGroup("Sets in complex contexts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("set in tuple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("context")))))))), hydra.show.Core.type(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("set in let binding", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("numbers"), new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(20))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30)))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("numbers")))))), hydra.show.Core.type(new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }

  static hydra.testing.TestGroup setsTests() {
    return new hydra.testing.TestGroup("Sets", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.Collections.monomorphicSetsTests(),
      hydra.test.checking.Collections.polymorphicSetsTests(),
      hydra.test.checking.Collections.setsInComplexContextsTests(),
      hydra.test.checking.Collections.nestedSetsTests(),
      hydra.test.checking.Collections.setsWithComplexTypesTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup setsWithComplexTypesTests() {
    return new hydra.testing.TestGroup("Sets with complex types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("set of records", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypePersonName(), java.util.Arrays.asList(
            new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
            new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Smith"))),
            new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30)))))))))))), hydra.show.Core.type(new hydra.core.Type.Set(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonName()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("set of optionals", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
            new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))),
            new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))))), hydra.show.Core.type(new hydra.core.Type.Set(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("set of maps", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<inference error>>"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.type(hydra.Scoping.typeSchemeToFType(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("key")),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))))))), hydra.show.Core.type(new hydra.core.Type.Set(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))));
  }
}
