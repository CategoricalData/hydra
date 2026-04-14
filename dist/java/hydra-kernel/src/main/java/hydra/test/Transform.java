// Note: this is an automatically generated file. Do not edit.

package hydra.test;

/**
 * Transform test cases for code generation, filtering to tests that can be compiled to target languages
 */
public interface Transform {
  static hydra.packaging.Namespace addGenerationPrefix(hydra.packaging.Namespace ns_) {
    return new hydra.packaging.Namespace(hydra.lib.strings.Cat2.apply(
      "generation.",
      (ns_).value));
  }

  static hydra.core.Term buildConvertCaseCall(hydra.util.CaseConvention fromConv, hydra.util.CaseConvention toConv, String input_) {
    return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.formatting.convertCase")), hydra.test.Transform.encodeCaseConvention(fromConv))), hydra.test.Transform.encodeCaseConvention(toConv))), new hydra.core.Term.Literal(new hydra.core.Literal.String_(input_))));
  }

  static hydra.core.Term buildTopologicalSortCall(java.util.List<hydra.util.Pair<Integer, java.util.List<Integer>>> adjList) {
    return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.sorting.topologicalSort")), hydra.test.Transform.encodeAdjacencyList(adjList)));
  }

  static hydra.core.Term buildTopologicalSortSCCCall(java.util.List<hydra.util.Pair<Integer, java.util.List<Integer>>> adjList) {
    return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.sorting.topologicalSortComponents")), hydra.test.Transform.encodeAdjacencyList(adjList)));
  }

  static java.util.List<hydra.testing.TestCaseWithMetadata> collectTestCases(hydra.testing.TestGroup tg) {
    return hydra.lib.lists.Concat2.apply(
      (tg).cases,
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.testing.TestGroup, java.util.List<hydra.testing.TestCaseWithMetadata>>) (sg -> hydra.test.Transform.collectTestCases(sg)),
        (tg).subgroups)));
  }

  static hydra.core.Term encodeAdjacencyList(java.util.List<hydra.util.Pair<Integer, java.util.List<Integer>>> pairs) {
    return new hydra.core.Term.List(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<Integer, java.util.List<Integer>>, hydra.core.Term>) (p -> new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(hydra.test.Transform.encodeInt(hydra.lib.pairs.First.apply(p)), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<Integer, hydra.core.Term>) (d -> hydra.test.Transform.encodeInt(d)),
        hydra.lib.pairs.Second.apply(p)))))))),
      pairs));
  }

  static hydra.core.Term encodeCaseConvention(hydra.util.CaseConvention conv) {
    return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.util.CaseConvention"), new hydra.core.Field((conv).accept(new hydra.util.CaseConvention.PartialVisitor<>() {
      @Override
      public hydra.core.Name visit(hydra.util.CaseConvention.LowerSnake ignored) {
        return new hydra.core.Name("lowerSnake");
      }

      @Override
      public hydra.core.Name visit(hydra.util.CaseConvention.UpperSnake ignored) {
        return new hydra.core.Name("upperSnake");
      }

      @Override
      public hydra.core.Name visit(hydra.util.CaseConvention.Camel ignored) {
        return new hydra.core.Name("camel");
      }

      @Override
      public hydra.core.Name visit(hydra.util.CaseConvention.Pascal ignored) {
        return new hydra.core.Name("pascal");
      }
    }), new hydra.core.Term.Unit())));
  }

  static hydra.core.Term encodeEitherListList(hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>> e) {
    return new hydra.core.Term.Either(hydra.lib.eithers.Bimap.apply(
      (java.util.function.Function<java.util.List<java.util.List<Integer>>, hydra.core.Term>) (cycles -> hydra.test.Transform.encodeListList(cycles)),
      (java.util.function.Function<java.util.List<Integer>, hydra.core.Term>) (sorted -> hydra.test.Transform.encodeIntList(sorted)),
      e));
  }

  static hydra.core.Term encodeInt(Integer n) {
    return new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(n)));
  }

  static hydra.core.Term encodeIntList(java.util.List<Integer> ints) {
    return new hydra.core.Term.List(hydra.lib.lists.Map.apply(
      (java.util.function.Function<Integer, hydra.core.Term>) (n -> hydra.test.Transform.encodeInt(n)),
      ints));
  }

  static hydra.core.Term encodeListList(java.util.List<java.util.List<Integer>> lists) {
    return new hydra.core.Term.List(hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<Integer>, hydra.core.Term>) (l -> hydra.test.Transform.encodeIntList(l)),
      lists));
  }

  static hydra.packaging.Module transformModule(hydra.packaging.Module m) {
    return new hydra.packaging.Module(hydra.test.Transform.addGenerationPrefix((m).namespace), (m).definitions, (m).termDependencies, (m).typeDependencies, (m).description);
  }

  static <T0> hydra.util.Maybe<T0> transformTestCase(T0 tcm) {
    return hydra.util.Maybe.just(tcm);
  }

  static hydra.util.Maybe<hydra.testing.TestGroup> transformToCompiledTests(hydra.testing.TestGroup tg) {
    java.util.List<hydra.testing.TestCaseWithMetadata> cases_ = (tg).cases;
    hydra.util.Maybe<String> desc = (tg).description;
    String name_ = (tg).name;
    java.util.List<hydra.testing.TestGroup> subgroups = (tg).subgroups;
    hydra.util.Lazy<java.util.List<hydra.testing.TestCaseWithMetadata>> transformedCases = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.testing.TestCaseWithMetadata, hydra.util.Maybe<hydra.testing.TestCaseWithMetadata>>) (tc -> hydra.test.Transform.transformTestCase(tc)),
      cases_)));
    hydra.util.Lazy<java.util.List<hydra.testing.TestGroup>> transformedSubgroups = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.testing.TestGroup, hydra.util.Maybe<hydra.testing.TestGroup>>) (sg -> hydra.test.Transform.transformToCompiledTests(sg)),
      subgroups)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.And.apply(
        hydra.lib.lists.Null.apply(transformedCases.get()),
        hydra.lib.lists.Null.apply(transformedSubgroups.get())),
      () -> (hydra.util.Maybe<hydra.testing.TestGroup>) (hydra.util.Maybe.<hydra.testing.TestGroup>nothing()),
      () -> hydra.util.Maybe.just(new hydra.testing.TestGroup(name_, desc, transformedSubgroups.get(), transformedCases.get())));
  }
}
