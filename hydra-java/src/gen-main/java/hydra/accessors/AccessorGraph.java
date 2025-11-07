// Note: this is an automatically generated file. Do not edit.

package hydra.accessors;

import java.io.Serializable;

/**
 * A graph of accessor nodes and edges, representing term access patterns
 */
public class AccessorGraph implements Serializable {
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
    java.util.Objects.requireNonNull((nodes));
    java.util.Objects.requireNonNull((edges));
    this.nodes = nodes;
    this.edges = edges;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AccessorGraph)) {
      return false;
    }
    AccessorGraph o = (AccessorGraph) (other);
    return nodes.equals(o.nodes) && edges.equals(o.edges);
  }
  
  @Override
  public int hashCode() {
    return 2 * nodes.hashCode() + 3 * edges.hashCode();
  }
  
  public AccessorGraph withNodes(java.util.List<hydra.accessors.AccessorNode> nodes) {
    java.util.Objects.requireNonNull((nodes));
    return new AccessorGraph(nodes, edges);
  }
  
  public AccessorGraph withEdges(java.util.List<hydra.accessors.AccessorEdge> edges) {
    java.util.Objects.requireNonNull((edges));
    return new AccessorGraph(nodes, edges);
  }
}
