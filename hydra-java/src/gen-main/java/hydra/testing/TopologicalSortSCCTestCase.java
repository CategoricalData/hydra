// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which performs topological sort with strongly connected component detection and compares the result with expected components
 */
public class TopologicalSortSCCTestCase implements Serializable, Comparable<TopologicalSortSCCTestCase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.testing.TopologicalSortSCCTestCase");
  
  public static final hydra.core.Name ADJACENCY_LIST = new hydra.core.Name("adjacencyList");
  
  public static final hydra.core.Name EXPECTED = new hydra.core.Name("expected");
  
  /**
   * The directed graph as an adjacency list
   */
  public final hydra.util.ConsList<hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>> adjacencyList;
  
  /**
   * The expected strongly connected components in topological order
   */
  public final hydra.util.ConsList<hydra.util.ConsList<Integer>> expected;
  
  public TopologicalSortSCCTestCase (hydra.util.ConsList<hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>> adjacencyList, hydra.util.ConsList<hydra.util.ConsList<Integer>> expected) {
    this.adjacencyList = adjacencyList;
    this.expected = expected;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TopologicalSortSCCTestCase)) {
      return false;
    }
    TopologicalSortSCCTestCase o = (TopologicalSortSCCTestCase) other;
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
  public int compareTo(TopologicalSortSCCTestCase other) {
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
  
  public TopologicalSortSCCTestCase withAdjacencyList(hydra.util.ConsList<hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>> adjacencyList) {
    return new TopologicalSortSCCTestCase(adjacencyList, expected);
  }
  
  public TopologicalSortSCCTestCase withExpected(hydra.util.ConsList<hydra.util.ConsList<Integer>> expected) {
    return new TopologicalSortSCCTestCase(adjacencyList, expected);
  }
}
