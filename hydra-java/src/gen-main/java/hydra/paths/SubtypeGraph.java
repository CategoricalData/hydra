// Note: this is an automatically generated file. Do not edit.

package hydra.paths;

import java.io.Serializable;

/**
 * A graph of subtype nodes and edges, representing type access patterns
 */
public class SubtypeGraph implements Serializable, Comparable<SubtypeGraph> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.paths.SubtypeGraph");

  public static final hydra.core.Name NODES = new hydra.core.Name("nodes");

  public static final hydra.core.Name EDGES = new hydra.core.Name("edges");

  /**
   * All nodes in the graph
   */
  public final hydra.util.ConsList<hydra.paths.SubtypeNode> nodes;

  /**
   * All edges in the graph
   */
  public final hydra.util.ConsList<hydra.paths.SubtypeEdge> edges;

  public SubtypeGraph (hydra.util.ConsList<hydra.paths.SubtypeNode> nodes, hydra.util.ConsList<hydra.paths.SubtypeEdge> edges) {
    this.nodes = nodes;
    this.edges = edges;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubtypeGraph)) {
      return false;
    }
    SubtypeGraph o = (SubtypeGraph) other;
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
  public int compareTo(SubtypeGraph other) {
    int cmp = 0;
    cmp = ((Comparable) nodes).compareTo(other.nodes);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) edges).compareTo(other.edges);
  }

  public SubtypeGraph withNodes(hydra.util.ConsList<hydra.paths.SubtypeNode> nodes) {
    return new SubtypeGraph(nodes, edges);
  }

  public SubtypeGraph withEdges(hydra.util.ConsList<hydra.paths.SubtypeEdge> edges) {
    return new SubtypeGraph(nodes, edges);
  }
}
