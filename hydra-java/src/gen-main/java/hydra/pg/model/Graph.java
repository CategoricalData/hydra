// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * A graph; a self-contained collection of vertices and edges
 */
public class Graph<V> implements Serializable, Comparable<Graph<V>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.Graph");
  
  public static final hydra.core.Name VERTICES = new hydra.core.Name("vertices");
  
  public static final hydra.core.Name EDGES = new hydra.core.Name("edges");
  
  public final hydra.util.PersistentMap<V, hydra.pg.model.Vertex<V>> vertices;
  
  public final hydra.util.PersistentMap<V, hydra.pg.model.Edge<V>> edges;
  
  public Graph (hydra.util.PersistentMap<V, hydra.pg.model.Vertex<V>> vertices, hydra.util.PersistentMap<V, hydra.pg.model.Edge<V>> edges) {
    this.vertices = vertices;
    this.edges = edges;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Graph)) {
      return false;
    }
    Graph o = (Graph) other;
    return java.util.Objects.equals(
      this.vertices,
      o.vertices) && java.util.Objects.equals(
      this.edges,
      o.edges);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(vertices) + 3 * java.util.Objects.hashCode(edges);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Graph other) {
    int cmp = 0;
    cmp = Integer.compare(
      vertices.hashCode(),
      other.vertices.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      edges.hashCode(),
      other.edges.hashCode());
  }
  
  public Graph withVertices(hydra.util.PersistentMap<V, hydra.pg.model.Vertex<V>> vertices) {
    return new Graph(vertices, edges);
  }
  
  public Graph withEdges(hydra.util.PersistentMap<V, hydra.pg.model.Edge<V>> edges) {
    return new Graph(vertices, edges);
  }
}
