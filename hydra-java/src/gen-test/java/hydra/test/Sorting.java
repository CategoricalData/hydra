// Note: this is an automatically generated file. Do not edit.

package hydra.test;

/**
 * Test cases for topological sorting algorithms
 */
public interface Sorting {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("sorting", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), hydra.util.ConsList.of(
      new hydra.testing.TestGroup("topological sort", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase((hydra.util.ConsList<hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>>) (hydra.util.ConsList.<hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>>empty()), hydra.util.Either.<hydra.util.ConsList<hydra.util.ConsList<Integer>>, hydra.util.ConsList<Integer>>right((hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("singleton set", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(hydra.util.ConsList.of((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), hydra.util.Either.<hydra.util.ConsList<hydra.util.ConsList<Integer>>, hydra.util.ConsList<Integer>>right(hydra.util.ConsList.of(1)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("discrete set with multiple elements", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), hydra.util.Either.<hydra.util.ConsList<hydra.util.ConsList<Integer>>, hydra.util.ConsList<Integer>>right(hydra.util.ConsList.of(
          1,
          2,
          3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("linked list", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(1)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(3)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), hydra.util.Either.<hydra.util.ConsList<hydra.util.ConsList<Integer>>, hydra.util.ConsList<Integer>>right(hydra.util.ConsList.of(
          1,
          3,
          2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("binary tree", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(
            1,
            4)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(4, hydra.util.ConsList.of(
            6,
            2)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(5)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(6, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(5, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), hydra.util.Either.<hydra.util.ConsList<hydra.util.ConsList<Integer>>, hydra.util.ConsList<Integer>>right(hydra.util.ConsList.of(
          5,
          1,
          2,
          6,
          4,
          3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("two trees", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(
            1,
            4)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(5, hydra.util.ConsList.of(
            6,
            2)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(7)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(4, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(6, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(7, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), hydra.util.Either.<hydra.util.ConsList<hydra.util.ConsList<Integer>>, hydra.util.ConsList<Integer>>right(hydra.util.ConsList.of(
          1,
          7,
          2,
          4,
          3,
          6,
          5)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("diamond DAG", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(
            3,
            4)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(2)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(4, hydra.util.ConsList.of(2)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(5)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(5, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), hydra.util.Either.<hydra.util.ConsList<hydra.util.ConsList<Integer>>, hydra.util.ConsList<Integer>>right(hydra.util.ConsList.of(
          5,
          2,
          3,
          4,
          1)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("two-node cycle", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(2)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(1))))), hydra.util.Either.<hydra.util.ConsList<hydra.util.ConsList<Integer>>, hydra.util.ConsList<Integer>>left(hydra.util.ConsList.of(hydra.util.ConsList.of(
          1,
          2))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("cycle with incoming and outgoing edges", new hydra.testing.TestCase.TopologicalSort(new hydra.testing.TopologicalSortTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(3)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(2)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(
            3,
            4)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(4, hydra.util.ConsList.of(5)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(5, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), hydra.util.Either.<hydra.util.ConsList<hydra.util.ConsList<Integer>>, hydra.util.ConsList<Integer>>left(hydra.util.ConsList.of(hydra.util.ConsList.of(
          2,
          3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("topological sort SCC", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase((hydra.util.ConsList<hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>>) (hydra.util.ConsList.<hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>>empty()), (hydra.util.ConsList<hydra.util.ConsList<Integer>>) (hydra.util.ConsList.<hydra.util.ConsList<Integer>>empty()))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("singleton set", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(hydra.util.ConsList.of((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), hydra.util.ConsList.of(hydra.util.ConsList.of(1)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("discrete set with multiple elements", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), hydra.util.ConsList.of(
          hydra.util.ConsList.of(1),
          hydra.util.ConsList.of(2),
          hydra.util.ConsList.of(3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("single two-element component #1", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(2)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), hydra.util.ConsList.of(
          hydra.util.ConsList.of(2),
          hydra.util.ConsList.of(1)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("single two-element component #2", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(1)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), hydra.util.ConsList.of(
          hydra.util.ConsList.of(1),
          hydra.util.ConsList.of(2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("multiple-element component", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(
            1,
            3)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(3)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), hydra.util.ConsList.of(
          hydra.util.ConsList.of(3),
          hydra.util.ConsList.of(1),
          hydra.util.ConsList.of(2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("cycle of two nodes #1", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(2)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(1))))), hydra.util.ConsList.of(hydra.util.ConsList.of(
          1,
          2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("cycle of two nodes #2", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(1)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(2))))), hydra.util.ConsList.of(hydra.util.ConsList.of(
          1,
          2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("cycle of three nodes #1", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(2)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(3)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(1))))), hydra.util.ConsList.of(hydra.util.ConsList.of(
          1,
          2,
          3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("cycle of three nodes #2", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(1)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(2)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(3))))), hydra.util.ConsList.of(hydra.util.ConsList.of(
          1,
          2,
          3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("multiple disconnected cycles", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(200, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(100, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(300, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(10, hydra.util.ConsList.of(20)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(20, hydra.util.ConsList.of(10)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(2)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(3)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(1))))), hydra.util.ConsList.of(
          hydra.util.ConsList.of(
            1,
            2,
            3),
          hydra.util.ConsList.of(
            10,
            20),
          hydra.util.ConsList.of(100),
          hydra.util.ConsList.of(200),
          hydra.util.ConsList.of(300)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("complex cycles", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(
            2,
            3)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(3)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(1))))), hydra.util.ConsList.of(hydra.util.ConsList.of(
          1,
          2,
          3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("chain of three SCCs", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(
            2,
            10)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(3)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(1)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(10, hydra.util.ConsList.of(20)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(20, hydra.util.ConsList.of(
            100,
            10)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(100, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), hydra.util.ConsList.of(
          hydra.util.ConsList.of(100),
          hydra.util.ConsList.of(
            10,
            20),
          hydra.util.ConsList.of(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("SCCs with dependencies to/from non-SCC nodes", new hydra.testing.TestCase.TopologicalSortSCC(new hydra.testing.TopologicalSortSCCTestCase(hydra.util.ConsList.of(
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(
            2,
            3,
            10)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(3)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(1)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(10, hydra.util.ConsList.of(
            20,
            30)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(20, hydra.util.ConsList.of(30)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(30, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(100, hydra.util.ConsList.of(
            200,
            2)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(200, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(300, hydra.util.ConsList.of(100)))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1000, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
          (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2000, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), hydra.util.ConsList.of(
          hydra.util.ConsList.of(30),
          hydra.util.ConsList.of(20),
          hydra.util.ConsList.of(10),
          hydra.util.ConsList.of(
            1,
            2,
            3),
          hydra.util.ConsList.of(200),
          hydra.util.ConsList.of(100),
          hydra.util.ConsList.of(300),
          hydra.util.ConsList.of(1000),
          hydra.util.ConsList.of(2000)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty()))))), (hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>) (hydra.util.ConsList.<hydra.testing.TestCaseWithMetadata>empty()));
  }
}
