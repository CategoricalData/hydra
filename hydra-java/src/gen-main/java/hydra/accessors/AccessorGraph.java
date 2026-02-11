// Note: this is an automatically generated file. Do not edit.

package hydra.accessors;

import java.io.Serializable;

/**
 * A graph of accessor nodes and edges, representing term access patterns
 */
public class AccessorGraph implements Serializable, Comparable<AccessorGraph> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.accessors.AccessorGraph");
  
  public static final hydra.core.Name FIELD_NAME_NODES = new hydra.core.Name("nodes");
  
  public static final hydra.core.Name FIELD_NAME_EDGES = new hydra.core.Name("edges");
  
  /**
   * All nodes in the graph
   */
  public final java.util.List<hydra.accessors.AccessorNode> nodes;
  
  /**
   * All edges in the graph
   */
  public final java.util.List<hydra.accessors.AccessorEdge> edges;
  
  public AccessorGraph (java.util.List<hydra.accessors.AccessorNode> nodes, java.util.List<hydra.accessors.AccessorEdge> edges) {
    this.nodes = nodes;
    this.edges = edges;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AccessorGraph)) {
      return false;
    }
    AccessorGraph o = (AccessorGraph) other;
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
  public int compareTo(AccessorGraph other) {
    int cmp = 0;
    cmp = Integer.compare(
      nodes.hashCode(),
      other.nodes.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      edges.hashCode(),
      other.edges.hashCode());
  }
  
  public AccessorGraph withNodes(java.util.List<hydra.accessors.AccessorNode> nodes) {
    return new AccessorGraph(nodes, edges);
  }
  
  public AccessorGraph withEdges(java.util.List<hydra.accessors.AccessorEdge> edges) {
    return new AccessorGraph(nodes, edges);
  }
}
