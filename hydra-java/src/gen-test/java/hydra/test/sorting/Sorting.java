// Note: this is an automatically generated file. Do not edit.

package hydra.test.sorting;

/**
 * Test cases for topological sorting algorithms
 */
public interface Sorting {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("sorting", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.List.of(
      new hydra.testing.TestGroup("topological sort", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.List.<hydra.testing.TestGroup>of()), java.util.List.of(
        new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase((java.util.List<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>) (java.util.List.<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>of()), (hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) ((hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) (hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>right((java.util.List<Integer>) (java.util.List.<Integer>of())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("singleton set", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(java.util.List.of((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.List.<Integer>of()))))), (hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) ((hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) (hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>right(java.util.List.of(1)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("discrete set with multiple elements", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(3, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, (java.util.List<Integer>) (java.util.List.<Integer>of()))))), (hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) ((hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) (hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>right(java.util.List.of(
          1,
          2,
          3)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("linked list", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(3, java.util.List.of(1)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, java.util.List.of(3)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.List.<Integer>of()))))), (hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) ((hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) (hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>right(java.util.List.of(
          1,
          3,
          2)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("binary tree", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(3, java.util.List.of(
            1,
            4)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(4, java.util.List.of(
            6,
            2)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, java.util.List.of(5)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(6, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(5, (java.util.List<Integer>) (java.util.List.<Integer>of()))))), (hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) ((hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) (hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>right(java.util.List.of(
          5,
          1,
          2,
          6,
          4,
          3)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("two trees", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(3, java.util.List.of(
            1,
            4)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(5, java.util.List.of(
            6,
            2)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, java.util.List.of(7)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(4, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(6, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(7, (java.util.List<Integer>) (java.util.List.<Integer>of()))))), (hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) ((hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) (hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>right(java.util.List.of(
          1,
          7,
          2,
          4,
          3,
          6,
          5)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("diamond DAG", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, java.util.List.of(
            3,
            4)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(3, java.util.List.of(2)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(4, java.util.List.of(2)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, java.util.List.of(5)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(5, (java.util.List<Integer>) (java.util.List.<Integer>of()))))), (hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) ((hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) (hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>right(java.util.List.of(
          5,
          2,
          3,
          4,
          1)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("two-node cycle", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, java.util.List.of(2)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, java.util.List.of(1))))), (hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) ((hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) (hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>left(java.util.List.of(java.util.List.of(
          1,
          2))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("cycle with incoming and outgoing edges", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, java.util.List.of(3)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(3, java.util.List.of(2)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, java.util.List.of(
            3,
            4)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(4, java.util.List.of(5)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(5, (java.util.List<Integer>) (java.util.List.<Integer>of()))))), (hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) ((hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>) (hydra.util.Either.<java.util.List<java.util.List<Integer>>, java.util.List<Integer>>left(java.util.List.of(java.util.List.of(
          2,
          3))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())))),
      new hydra.testing.TestGroup("topological sort SCC", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.List.<hydra.testing.TestGroup>of()), java.util.List.of(
        new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase((java.util.List<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>) (java.util.List.<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>>of()), (java.util.List<java.util.List<Integer>>) (java.util.List.<java.util.List<Integer>>of()))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("singleton set", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(java.util.List.of((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.List.<Integer>of()))))), java.util.List.of(java.util.List.of(1)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("discrete set with multiple elements", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(3, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, (java.util.List<Integer>) (java.util.List.<Integer>of()))))), java.util.List.of(
          java.util.List.of(1),
          java.util.List.of(2),
          java.util.List.of(3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("single two-element component #1", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, java.util.List.of(2)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, (java.util.List<Integer>) (java.util.List.<Integer>of()))))), java.util.List.of(
          java.util.List.of(2),
          java.util.List.of(1)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("single two-element component #2", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, java.util.List.of(1)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.List.<Integer>of()))))), java.util.List.of(
          java.util.List.of(1),
          java.util.List.of(2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("multiple-element component", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, java.util.List.of(
            1,
            3)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, java.util.List.of(3)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(3, (java.util.List<Integer>) (java.util.List.<Integer>of()))))), java.util.List.of(
          java.util.List.of(3),
          java.util.List.of(1),
          java.util.List.of(2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("cycle of two nodes #1", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, java.util.List.of(2)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, java.util.List.of(1))))), java.util.List.of(java.util.List.of(
          1,
          2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("cycle of two nodes #2", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, java.util.List.of(1)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, java.util.List.of(2))))), java.util.List.of(java.util.List.of(
          1,
          2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("cycle of three nodes #1", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, java.util.List.of(2)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, java.util.List.of(3)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(3, java.util.List.of(1))))), java.util.List.of(java.util.List.of(
          1,
          2,
          3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("cycle of three nodes #2", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, java.util.List.of(1)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(3, java.util.List.of(2)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, java.util.List.of(3))))), java.util.List.of(java.util.List.of(
          1,
          2,
          3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("multiple disconnected cycles", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(200, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(100, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(300, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(10, java.util.List.of(20)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(20, java.util.List.of(10)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, java.util.List.of(2)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, java.util.List.of(3)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(3, java.util.List.of(1))))), java.util.List.of(
          java.util.List.of(
            1,
            2,
            3),
          java.util.List.of(
            10,
            20),
          java.util.List.of(100),
          java.util.List.of(200),
          java.util.List.of(300)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("complex cycles", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, java.util.List.of(
            2,
            3)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, java.util.List.of(3)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(3, java.util.List.of(1))))), java.util.List.of(java.util.List.of(
          1,
          2,
          3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("chain of three SCCs", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, java.util.List.of(
            2,
            10)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, java.util.List.of(3)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(3, java.util.List.of(1)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(10, java.util.List.of(20)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(20, java.util.List.of(
            100,
            10)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(100, (java.util.List<Integer>) (java.util.List.<Integer>of()))))), java.util.List.of(
          java.util.List.of(100),
          java.util.List.of(
            10,
            20),
          java.util.List.of(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("SCCs with dependencies to/from non-SCC nodes", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(java.util.List.of(
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1, java.util.List.of(
            2,
            3,
            10)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2, java.util.List.of(3)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(3, java.util.List.of(1)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(10, java.util.List.of(
            20,
            30)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(20, java.util.List.of(30)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(30, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(100, java.util.List.of(
            200,
            2)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(200, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(300, java.util.List.of(100)))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(1000, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
          (hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>(2000, (java.util.List<Integer>) (java.util.List.<Integer>of()))))), java.util.List.of(
          java.util.List.of(30),
          java.util.List.of(20),
          java.util.List.of(10),
          java.util.List.of(
            1,
            2,
            3),
          java.util.List.of(200),
          java.util.List.of(100),
          java.util.List.of(300),
          java.util.List.of(1000),
          java.util.List.of(2000)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.List.<hydra.testing.TestCaseWithMetadata>of()));
  }
}
