// Note: this is an automatically generated file. Do not edit.

package hydra.test;

/**
 * Test cases for topological sorting algorithms
 */
public interface Sorting {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("sorting", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("topological sort", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.Sorting.topologicalSort((java.util.List<hydra.util.Pair<Integer, java.util.List<Integer>>>) (java.util.Collections.<hydra.util.Pair<Integer, java.util.List<Integer>>>emptyList()))), hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>right((java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("singleton set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.Sorting.topologicalSort(java.util.Arrays.asList((hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))))), hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>right(java.util.Arrays.asList(1))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("discrete set with multiple elements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.Sorting.topologicalSort(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))))), hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>right(java.util.Arrays.asList(
            1,
            2,
            3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("linked list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.Sorting.topologicalSort(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.Arrays.asList(1)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.Arrays.asList(3)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))))), hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>right(java.util.Arrays.asList(
            1,
            3,
            2))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("binary tree", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.Sorting.topologicalSort(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.Arrays.asList(
              1,
              4)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(4, java.util.Arrays.asList(
              6,
              2)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.Arrays.asList(5)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(6, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(5, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))))), hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>right(java.util.Arrays.asList(
            5,
            1,
            2,
            6,
            4,
            3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("two trees", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.Sorting.topologicalSort(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.Arrays.asList(
              1,
              4)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(5, java.util.Arrays.asList(
              6,
              2)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.Arrays.asList(7)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(4, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(6, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(7, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))))), hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>right(java.util.Arrays.asList(
            1,
            7,
            2,
            4,
            3,
            6,
            5))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("diamond DAG", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.Sorting.topologicalSort(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.Arrays.asList(
              3,
              4)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.Arrays.asList(2)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(4, java.util.Arrays.asList(2)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.Arrays.asList(5)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(5, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))))), hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>right(java.util.Arrays.asList(
            5,
            2,
            3,
            4,
            1))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("two-node cycle", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.Sorting.topologicalSort(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.Arrays.asList(2)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.Arrays.asList(1))))))), hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>left(java.util.Arrays.asList(java.util.Arrays.asList(
            1,
            2)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cycle with incoming and outgoing edges", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.Sorting.topologicalSort(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.Arrays.asList(3)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.Arrays.asList(2)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.Arrays.asList(
              3,
              4)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(4, java.util.Arrays.asList(5)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(5, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))))), hydra.lib.eithers.Either.apply(
          (java.util.function.Function<java.util.List<java.util.List<Integer>>, String>) (cs -> hydra.lib.strings.Cat2.apply(
            "left(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
                  hydra.lib.literals.ShowInt32::apply,
                  v1)),
                cs),
              ")"))),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.lib.strings.Cat2.apply(
            "right(",
            hydra.lib.strings.Cat2.apply(
              hydra.show.Core.list(
                hydra.lib.literals.ShowInt32::apply,
                xs),
              ")"))),
          hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>left(java.util.Arrays.asList(java.util.Arrays.asList(
            2,
            3)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("topological sort SCC", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          hydra.Sorting.topologicalSortComponents((java.util.List<hydra.util.Pair<Integer, java.util.List<Integer>>>) (java.util.Collections.<hydra.util.Pair<Integer, java.util.List<Integer>>>emptyList()))), hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          (java.util.List<java.util.List<Integer>>) (java.util.Collections.<java.util.List<Integer>>emptyList())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("singleton set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          hydra.Sorting.topologicalSortComponents(java.util.Arrays.asList((hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))))), hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          java.util.Arrays.asList(java.util.Arrays.asList(1))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("discrete set with multiple elements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          hydra.Sorting.topologicalSortComponents(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))))), hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          java.util.Arrays.asList(
            java.util.Arrays.asList(1),
            java.util.Arrays.asList(2),
            java.util.Arrays.asList(3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("single two-element component #1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          hydra.Sorting.topologicalSortComponents(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.Arrays.asList(2)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))))), hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          java.util.Arrays.asList(
            java.util.Arrays.asList(2),
            java.util.Arrays.asList(1))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("single two-element component #2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          hydra.Sorting.topologicalSortComponents(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.Arrays.asList(1)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))))), hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          java.util.Arrays.asList(
            java.util.Arrays.asList(1),
            java.util.Arrays.asList(2))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("multiple-element component", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          hydra.Sorting.topologicalSortComponents(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.Arrays.asList(
              1,
              3)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.Arrays.asList(3)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))))), hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          java.util.Arrays.asList(
            java.util.Arrays.asList(3),
            java.util.Arrays.asList(1),
            java.util.Arrays.asList(2))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cycle of two nodes #1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          hydra.Sorting.topologicalSortComponents(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.Arrays.asList(2)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.Arrays.asList(1))))))), hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          java.util.Arrays.asList(java.util.Arrays.asList(
            1,
            2))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cycle of two nodes #2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          hydra.Sorting.topologicalSortComponents(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.Arrays.asList(1)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.Arrays.asList(2))))))), hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          java.util.Arrays.asList(java.util.Arrays.asList(
            1,
            2))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cycle of three nodes #1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          hydra.Sorting.topologicalSortComponents(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.Arrays.asList(2)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.Arrays.asList(3)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.Arrays.asList(1))))))), hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          java.util.Arrays.asList(java.util.Arrays.asList(
            1,
            2,
            3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cycle of three nodes #2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          hydra.Sorting.topologicalSortComponents(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.Arrays.asList(1)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.Arrays.asList(2)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.Arrays.asList(3))))))), hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          java.util.Arrays.asList(java.util.Arrays.asList(
            1,
            2,
            3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("multiple disconnected cycles", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          hydra.Sorting.topologicalSortComponents(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(200, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(100, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(300, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(10, java.util.Arrays.asList(20)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(20, java.util.Arrays.asList(10)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.Arrays.asList(2)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.Arrays.asList(3)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.Arrays.asList(1))))))), hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          java.util.Arrays.asList(
            java.util.Arrays.asList(
              1,
              2,
              3),
            java.util.Arrays.asList(
              10,
              20),
            java.util.Arrays.asList(100),
            java.util.Arrays.asList(200),
            java.util.Arrays.asList(300))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("complex cycles", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          hydra.Sorting.topologicalSortComponents(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.Arrays.asList(
              2,
              3)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.Arrays.asList(3)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.Arrays.asList(1))))))), hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          java.util.Arrays.asList(java.util.Arrays.asList(
            1,
            2,
            3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("chain of three SCCs", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          hydra.Sorting.topologicalSortComponents(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.Arrays.asList(
              2,
              10)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.Arrays.asList(3)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.Arrays.asList(1)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(10, java.util.Arrays.asList(20)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(20, java.util.Arrays.asList(
              100,
              10)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(100, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))))), hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          java.util.Arrays.asList(
            java.util.Arrays.asList(100),
            java.util.Arrays.asList(
              10,
              20),
            java.util.Arrays.asList(
              1,
              2,
              3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("SCCs with dependencies to/from non-SCC nodes", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          hydra.Sorting.topologicalSortComponents(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.Arrays.asList(
              2,
              3,
              10)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.Arrays.asList(3)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.Arrays.asList(1)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(10, java.util.Arrays.asList(
              20,
              30)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(20, java.util.Arrays.asList(30)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(30, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(100, java.util.Arrays.asList(
              200,
              2)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(200, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(300, java.util.Arrays.asList(100)))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1000, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))),
            (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2000, (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))))), hydra.show.Core.list(
          (java.util.function.Function<java.util.List<Integer>, String>) (v1 -> hydra.show.Core.list(
            hydra.lib.literals.ShowInt32::apply,
            v1)),
          java.util.Arrays.asList(
            java.util.Arrays.asList(30),
            java.util.Arrays.asList(20),
            java.util.Arrays.asList(10),
            java.util.Arrays.asList(
              1,
              2,
              3),
            java.util.Arrays.asList(200),
            java.util.Arrays.asList(100),
            java.util.Arrays.asList(300),
            java.util.Arrays.asList(1000),
            java.util.Arrays.asList(2000))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}
