// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which performs topological sort with strongly connected component detection and compares the result with expected components
 */
public class TopologicalSortSCCTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.TopologicalSortSCCTestCase");
  
  public static final hydra.core.Name FIELD_NAME_ADJACENCY_LIST = new hydra.core.Name("adjacencyList");
  
  public static final hydra.core.Name FIELD_NAME_EXPECTED = new hydra.core.Name("expected");
  
  /**
   * The directed graph as an adjacency list
   */
  public final java.util.List<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>> adjacencyList;
  
  /**
   * The expected strongly connected components in topological order
   */
  public final java.util.List<java.util.List<Integer>> expected;
  
  public TopologicalSortSCCTestCase (java.util.List<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>> adjacencyList, java.util.List<java.util.List<Integer>> expected) {
    java.util.Objects.requireNonNull((adjacencyList));
    java.util.Objects.requireNonNull((expected));
    this.adjacencyList = adjacencyList;
    this.expected = expected;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TopologicalSortSCCTestCase)) {
      return false;
    }
    TopologicalSortSCCTestCase o = (TopologicalSortSCCTestCase) (other);
    return adjacencyList.equals(o.adjacencyList) && expected.equals(o.expected);
  }
  
  @Override
  public int hashCode() {
    return 2 * adjacencyList.hashCode() + 3 * expected.hashCode();
  }
  
  public TopologicalSortSCCTestCase withAdjacencyList(java.util.List<hydra.util.Tuple.Tuple2<Integer, java.util.List<Integer>>> adjacencyList) {
    java.util.Objects.requireNonNull((adjacencyList));
    return new TopologicalSortSCCTestCase(adjacencyList, expected);
  }
  
  public TopologicalSortSCCTestCase withExpected(java.util.List<java.util.List<Integer>> expected) {
    java.util.Objects.requireNonNull((expected));
    return new TopologicalSortSCCTestCase(adjacencyList, expected);
  }
}
