// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.propertyGraph;

import java.io.Serializable;

/**
 * A graph; a self-contained collection of vertices and edges
 */
public class Graph<V> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/propertyGraph.Graph");
  
  public final java.util.Map<V, hydra.langs.tinkerpop.propertyGraph.Vertex<V>> vertices;
  
  public final java.util.Map<V, hydra.langs.tinkerpop.propertyGraph.Edge<V>> edges;
  
  public Graph (java.util.Map<V, hydra.langs.tinkerpop.propertyGraph.Vertex<V>> vertices, java.util.Map<V, hydra.langs.tinkerpop.propertyGraph.Edge<V>> edges) {
    if (vertices == null) {
      throw new IllegalArgumentException("null value for 'vertices' argument");
    }
    if (edges == null) {
      throw new IllegalArgumentException("null value for 'edges' argument");
    }
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
  
  public Graph withVertices(java.util.Map<V, hydra.langs.tinkerpop.propertyGraph.Vertex<V>> vertices) {
    if (vertices == null) {
      throw new IllegalArgumentException("null value for 'vertices' argument");
    }
    return new Graph(vertices, edges);
  }
  
  public Graph withEdges(java.util.Map<V, hydra.langs.tinkerpop.propertyGraph.Edge<V>> edges) {
    if (edges == null) {
      throw new IllegalArgumentException("null value for 'edges' argument");
    }
    return new Graph(vertices, edges);
  }
}