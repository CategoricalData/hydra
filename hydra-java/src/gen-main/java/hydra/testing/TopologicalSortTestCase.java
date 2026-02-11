// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which performs topological sort on a directed graph and compares the result with either an expected sorted list or expected cycles
 */
public class TopologicalSortTestCase implements Serializable, Comparable<TopologicalSortTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.TopologicalSortTestCase");
  
  public static final hydra.core.Name FIELD_NAME_ADJACENCY_LIST = new hydra.core.Name("adjacencyList");
  
  public static final hydra.core.Name FIELD_NAME_EXPECTED = new hydra.core.Name("expected");
  
  /**
   * The directed graph as an adjacency list (node to list of dependencies)
   */
  public final java.util.List<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>> adjacencyList;
  
  /**
   * The expected result: Left for cycles, Right for sorted nodes
   */
  public final hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>> expected;
  
  public TopologicalSortTestCase (java.util.List<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>> adjacencyList, hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>> expected) {
    this.adjacencyList = adjacencyList;
    this.expected = expected;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TopologicalSortTestCase)) {
      return false;
    }
    TopologicalSortTestCase o = (TopologicalSortTestCase) other;
    return java.util.Objects.equals(
      this.adjacencyList,
      o.adjacencyList) && java.util.Objects.equals(
      this.expected,
      o.expected);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(adjacencyList) + 3 * java.util.Objects.hashCode(expected);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TopologicalSortTestCase other) {
    int cmp = 0;
    cmp = Integer.compare(
      adjacencyList.hashCode(),
      other.adjacencyList.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      expected.hashCode(),
      other.expected.hashCode());
  }
  
  public TopologicalSortTestCase withAdjacencyList(java.util.List<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>> adjacencyList) {
    return new TopologicalSortTestCase(adjacencyList, expected);
  }
  
  public TopologicalSortTestCase withExpected(hydra.util.Either<java.util.List<java.util.List<Integer>>, java.util.List<Integer>> expected) {
    return new TopologicalSortTestCase(adjacencyList, expected);
  }
}
