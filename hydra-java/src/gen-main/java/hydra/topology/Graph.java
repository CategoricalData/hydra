// Note: this is an automatically generated file. Do not edit.

package hydra.topology;

/**
 * A directed graph represented as an adjacency list mapping vertices to their outgoing neighbors
 */
public class Graph {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.topology.Graph");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.Map<hydra.topology.Vertex, java.util.List<hydra.topology.Vertex>> value;
  
  public Graph (java.util.Map<hydra.topology.Vertex, java.util.List<hydra.topology.Vertex>> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Graph)) {
      return false;
    }
    Graph o = (Graph) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
