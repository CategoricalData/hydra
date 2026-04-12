// Note: this is an automatically generated file. Do not edit.

package hydra.test.lib;

/**
 * Test cases for hydra.lib.pairs primitives
 */
public interface Pairs {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("hydra.lib.pairs primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("bimap", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("transform both elements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "(",
          hydra.lib.literals.ShowInt32.apply(hydra.lib.pairs.First.apply(hydra.lib.pairs.Bimap.apply(
            (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
              x,
              2)),
            (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(5, "ab")))))),
          ", ",
          hydra.lib.literals.ShowInt32.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Bimap.apply(
            (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
              x,
              2)),
            (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(5, "ab")))))),
          ")")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "(",
          hydra.lib.literals.ShowInt32.apply(hydra.lib.pairs.First.apply((hydra.util.Pair<Integer, Integer>) ((hydra.util.Pair<Integer, Integer>) (new hydra.util.Pair<Integer, Integer>(10, 2))))),
          ", ",
          hydra.lib.literals.ShowInt32.apply(hydra.lib.pairs.Second.apply((hydra.util.Pair<Integer, Integer>) ((hydra.util.Pair<Integer, Integer>) (new hydra.util.Pair<Integer, Integer>(10, 2))))),
          ")")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("with zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "(",
          hydra.lib.literals.ShowInt32.apply(hydra.lib.pairs.First.apply(hydra.lib.pairs.Bimap.apply(
            (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
              x,
              2)),
            (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(0, "hello")))))),
          ", ",
          hydra.lib.literals.ShowInt32.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Bimap.apply(
            (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
              x,
              2)),
            (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(0, "hello")))))),
          ")")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "(",
          hydra.lib.literals.ShowInt32.apply(hydra.lib.pairs.First.apply((hydra.util.Pair<Integer, Integer>) ((hydra.util.Pair<Integer, Integer>) (new hydra.util.Pair<Integer, Integer>(0, 5))))),
          ", ",
          hydra.lib.literals.ShowInt32.apply(hydra.lib.pairs.Second.apply((hydra.util.Pair<Integer, Integer>) ((hydra.util.Pair<Integer, Integer>) (new hydra.util.Pair<Integer, Integer>(0, 5))))),
          ")")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("first", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("extract first element", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.pairs.First.apply((hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(42, "hello"))))), hydra.lib.literals.ShowInt32.apply(42))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("with zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.pairs.First.apply((hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(0, "world"))))), hydra.lib.literals.ShowInt32.apply(0))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative number", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.pairs.First.apply((hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(-5, "test"))))), hydra.lib.literals.ShowInt32.apply(-5))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("second", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("extract second element", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.pairs.Second.apply((hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(42, "hello")))), "hello")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.pairs.Second.apply((hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(0, "")))), "")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("long string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.pairs.Second.apply((hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(123, "testing")))), "testing")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}
