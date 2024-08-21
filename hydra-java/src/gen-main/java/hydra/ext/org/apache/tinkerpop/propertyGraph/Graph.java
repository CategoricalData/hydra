// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.propertyGraph;

import java.io.Serializable;

/**
 * A graph; a self-contained collection of vertices and edges
 */
public class Graph<V> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/propertyGraph.Graph");
  
  public static final hydra.core.Name FIELD_NAME_VERTICES = new hydra.core.Name("vertices");
  
  public static final hydra.core.Name FIELD_NAME_EDGES = new hydra.core.Name("edges");
  
  public final java.util.Map<V, hydra.ext.org.apache.tinkerpop.propertyGraph.Vertex<V>> vertices;
  
  public final java.util.Map<V, hydra.ext.org.apache.tinkerpop.propertyGraph.Edge<V>> edges;
  
  public Graph (java.util.Map<V, hydra.ext.org.apache.tinkerpop.propertyGraph.Vertex<V>> vertices, java.util.Map<V, hydra.ext.org.apache.tinkerpop.propertyGraph.Edge<V>> edges) {
    java.util.Objects.requireNonNull((vertices));
    java.util.Objects.requireNonNull((edges));
    this.vertices = vertices;
    this.edges = edges;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Graph)) {
      return false;
    }
    Graph o = (Graph) (other);
    return vertices.equals(o.vertices) && edges.equals(o.edges);
  }
  
  @Override
  public int hashCode() {
    return 2 * vertices.hashCode() + 3 * edges.hashCode();
  }
  
  public Graph withVertices(java.util.Map<V, hydra.ext.org.apache.tinkerpop.propertyGraph.Vertex<V>> vertices) {
    java.util.Objects.requireNonNull((vertices));
    return new Graph(vertices, edges);
  }
  
  public Graph withEdges(java.util.Map<V, hydra.ext.org.apache.tinkerpop.propertyGraph.Edge<V>> edges) {
    java.util.Objects.requireNonNull((edges));
    return new Graph(vertices, edges);
  }
}