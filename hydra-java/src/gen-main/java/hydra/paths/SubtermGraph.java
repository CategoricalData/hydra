// Note: this is an automatically generated file. Do not edit.

package hydra.paths;

import java.io.Serializable;

/**
 * A graph of subterm nodes and edges, representing term access patterns
 */
public class SubtermGraph implements Serializable, Comparable<SubtermGraph> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.paths.SubtermGraph");

  public static final hydra.core.Name NODES = new hydra.core.Name("nodes");

  public static final hydra.core.Name EDGES = new hydra.core.Name("edges");

  /**
   * All nodes in the graph
   */
  public final hydra.util.ConsList<hydra.paths.SubtermNode> nodes;

  /**
   * All edges in the graph
   */
  public final hydra.util.ConsList<hydra.paths.SubtermEdge> edges;

  public SubtermGraph (hydra.util.ConsList<hydra.paths.SubtermNode> nodes, hydra.util.ConsList<hydra.paths.SubtermEdge> edges) {
    this.nodes = nodes;
    this.edges = edges;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubtermGraph)) {
      return false;
    }
    SubtermGraph o = (SubtermGraph) other;
    return java.util.Objects.equals(
      this.nodes,
      o.nodes) && java.util.Objects.equals(
      this.edges,
      o.edges);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(nodes) + 3 * java.util.Objects.hashCode(edges);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SubtermGraph other) {
    int cmp = 0;
    cmp = ((Comparable) nodes).compareTo(other.nodes);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) edges).compareTo(other.edges);
  }

  public SubtermGraph withNodes(hydra.util.ConsList<hydra.paths.SubtermNode> nodes) {
    return new SubtermGraph(nodes, edges);
  }

  public SubtermGraph withEdges(hydra.util.ConsList<hydra.paths.SubtermEdge> edges) {
    return new SubtermGraph(nodes, edges);
  }
}
