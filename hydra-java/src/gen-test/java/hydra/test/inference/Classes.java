// Note: this is an automatically generated file. Do not edit.

package hydra.test.inference;

/**
 * Inference tests for type class constraints (ordering and equality)
 */
public interface Classes {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("Type classes", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.inference.Classes.testGroupForMonomorphicConstraints(),
      hydra.test.inference.Classes.testGroupForPrimitiveReferences(),
      hydra.test.inference.Classes.testGroupForPartialApplication(),
      hydra.test.inference.Classes.testGroupForLetBindings(),
      hydra.test.inference.Classes.testGroupForComposition(),
      hydra.test.inference.Classes.testGroupForNestedContainers(),
      hydra.test.inference.Classes.testGroupForCollectionTerms()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup testGroupForCollectionTerms() {
    return new hydra.testing.TestGroup("Collection term constraints", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Set literals", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0"))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(new hydra.testing.Tag("disabledForMinimalInference"))),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
              new hydra.core.Term.Variable(new hydra.core.Name("x")),
              new hydra.core.Term.Variable(new hydra.core.Name("y"))))))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(new hydra.testing.Tag("disabledForMinimalInference"))),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(new hydra.testing.Tag("disabledForMinimalInference"))))),
      new hydra.testing.TestGroup("Map literals", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("k"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
              new hydra.core.Term.Variable(new hydra.core.Name("k")),
              new hydra.core.Term.Variable(new hydra.core.Name("v")))))))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(
          new hydra.core.Name("t0"),
          new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t1")), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(new hydra.testing.Tag("disabledForMinimalInference"))),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(new hydra.testing.Tag("disabledForMinimalInference"))))),
      new hydra.testing.TestGroup("Collection terms with primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
              new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.negate")), new hydra.core.Term.Variable(new hydra.core.Name("x")))),
              new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(new hydra.testing.Tag("disabledForMinimalInference"))),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("k"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
              new hydra.core.Term.Variable(new hydra.core.Name("k")),
              new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.sort")), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Variable(new hydra.core.Name("k"))))))))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(new hydra.testing.Tag("disabledForMinimalInference"))))),
      new hydra.testing.TestGroup("Constraint propagation through collection elements", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("xs"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Map(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
              new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.length")), new hydra.core.Term.Variable(new hydra.core.Name("xs")))),
              new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.fromList")), new hydra.core.Term.Variable(new hydra.core.Name("xs"))))))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(new hydra.testing.Tag("disabledForMinimalInference"))),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.sort")))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.List(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(new hydra.testing.Tag("disabledForMinimalInference"))),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.fromList")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0"))))), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(new hydra.testing.Tag("disabledForMinimalInference"))),
        new hydra.testing.TestCaseWithMetadata("#4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("xs"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.fromList")), new hydra.core.Term.Variable(new hydra.core.Name("xs")))))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.Set(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(new hydra.testing.Tag("disabledForMinimalInference")))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup testGroupForComposition() {
    return new hydra.testing.TestGroup("Composition and constraint merging", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Composing constrained primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("xs"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.fromList")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.map")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("xs"))))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t0")))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("xs"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.fromList")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.map")), new hydra.core.Term.Variable(new hydra.core.Name("f")))), new hydra.core.Term.Variable(new hydra.core.Name("xs")))))))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(
          new hydra.core.Name("t0"),
          new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t1"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Composing map and sort", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
          "INFERENCE ERROR: ",
          "failed")),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("m"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.map")), new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.sort")))), new hydra.core.Term.Variable(new hydra.core.Name("m"))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(
        new hydra.core.Name("t0"),
        new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t1"))))), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(
        java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering"))))),
        java.util.Map.entry(
          new hydra.core.Name("t1"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup testGroupForLetBindings() {
    return new hydra.testing.TestGroup("Let binding constraint propagation", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Simple let-bound wrappers", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("lookup"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("k"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("m"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.lookup")), new hydra.core.Term.Variable(new hydra.core.Name("k")))), new hydra.core.Term.Variable(new hydra.core.Name("m")))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("lookup")))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(
          new hydra.core.Name("t0"),
          new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("member"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("s"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.member")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Variable(new hydra.core.Name("s")))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("member")))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("fromList"), new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.fromList")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("fromList")))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(
          new hydra.core.Name("t0"),
          new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1"))))), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Let-bound with partial instantiation", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("m"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.map")), new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.negate")))), new hydra.core.Term.Variable(new hydra.core.Name("m"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("f")))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("xs"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.fromList")), new hydra.core.Term.Variable(new hydra.core.Name("xs"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("f")))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0"))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Multiple uses of a constrained let binding", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
          "INFERENCE ERROR: ",
          "failed")),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.fromList")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)), new hydra.core.Term.Literal(new hydra.core.Literal.String_("x")))))))))))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup testGroupForMonomorphicConstraints() {
    return new hydra.testing.TestGroup("Monomorphic (constraints vanish)", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Map operations with concrete types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.fromList")), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.lookup")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("k")))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.singleton")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("k")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.insert")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("k")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.empty")))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Set operations with concrete types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.fromList")), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.member")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.singleton")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Equality operations with concrete types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.equality.equal")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.equality.compare")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.util.Comparison")), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("List operations with concrete types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
          "INFERENCE ERROR: ",
          "failed")),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.sort")), new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup testGroupForNestedContainers() {
    return new hydra.testing.TestGroup("Nested containers", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Maps of sets", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
          "INFERENCE ERROR: ",
          "failed")),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("m"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.map")), new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.fromList")))), new hydra.core.Term.Variable(new hydra.core.Name("m"))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(
        new hydra.core.Name("t0"),
        new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t1"))))), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(
        java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering"))))),
        java.util.Map.entry(
          new hydra.core.Name("t1"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Sets of sets", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
          "INFERENCE ERROR: ",
          "failed")),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("xss"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.map")), new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.fromList")))), new hydra.core.Term.Variable(new hydra.core.Name("xss"))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Set(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Set(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0")))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
        new hydra.core.Name("t0"),
        new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Map from sorted list", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
          "INFERENCE ERROR: ",
          "failed")),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("xs"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.fromList")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.map")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.singleton")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))))), new hydra.core.Term.Variable(new hydra.core.Name("xs"))))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
        new hydra.core.Name("t0"),
        new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup testGroupForPartialApplication() {
    return new hydra.testing.TestGroup("Partial application preserving constraints", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Map partial application", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("k"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.lookup")), new hydra.core.Term.Variable(new hydra.core.Name("k"))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(
          new hydra.core.Name("t0"),
          new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("k"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.singleton")), new hydra.core.Term.Variable(new hydra.core.Name("k")))), new hydra.core.Term.Variable(new hydra.core.Name("v")))))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(
          new hydra.core.Name("t0"),
          new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t1")), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Set partial application", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
          "INFERENCE ERROR: ",
          "failed")),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.member")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
        new hydra.core.Name("t0"),
        new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Equality partial application", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
          "INFERENCE ERROR: ",
          "failed")),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.equality.equal")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Variable(new hydra.core.Name("y")))))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
        new hydra.core.Name("t0"),
        new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("equality")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Partial application fixing the constrained variable", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
          "INFERENCE ERROR: ",
          "failed")),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.singleton")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("key")))), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Variable(new hydra.core.Name("t0")))))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup testGroupForPrimitiveReferences() {
    return new hydra.testing.TestGroup("Primitive references with constraints", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Map primitives (ordering on key type)", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.fromList")))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(
          new hydra.core.Name("t0"),
          new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1"))))), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.lookup")))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(
          new hydra.core.Name("t0"),
          new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.insert")))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(
          new hydra.core.Name("t0"),
          new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.map")))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(
          new hydra.core.Name("t0"),
          new hydra.core.Name("t1"),
          new hydra.core.Name("t2")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t2")), new hydra.core.Type.Variable(new hydra.core.Name("t0")))), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t2")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t2"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.empty")))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(
          new hydra.core.Name("t0"),
          new hydra.core.Name("t1")), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Set primitives (ordering on element type)", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.fromList")))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0"))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.member")))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.insert")))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0"))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.map")))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(
          new hydra.core.Name("t0"),
          new hydra.core.Name("t1")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("t1")))), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("t1"))))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(
          java.util.Map.entry(
            new hydra.core.Name("t0"),
            new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering"))))),
          java.util.Map.entry(
            new hydra.core.Name("t1"),
            new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Equality primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.equality.equal")))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("equality")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.equality.compare")))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Variable(new hydra.core.Name("hydra.util.Comparison")))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("List primitives with constraints", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.sort")))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("ordering")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.nub")))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("equality")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> hydra.lib.strings.Cat2.apply(
            "INFERENCE ERROR: ",
            "failed")),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result)))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.elem")))), hydra.show.Core.typeScheme(new hydra.core.TypeScheme(java.util.Arrays.asList(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t0")), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("t0"))), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()))))), hydra.util.Maybe.just(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          new hydra.core.Name("t0"),
          new hydra.core.TypeVariableMetadata(new java.util.TreeSet(java.util.Set.of(new hydra.core.Name("equality")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}
