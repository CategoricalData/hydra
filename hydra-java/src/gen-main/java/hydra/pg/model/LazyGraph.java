// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * A graph which does not assume that vertex or edge ids are unique. This is useful in mappings because the id specifications for vertices and/or edges may be non-unique.
 */
public class LazyGraph<V> implements Serializable, Comparable<LazyGraph<V>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.LazyGraph");
  
  public static final hydra.core.Name VERTICES = new hydra.core.Name("vertices");
  
  public static final hydra.core.Name EDGES = new hydra.core.Name("edges");
  
  public final java.util.List<hydra.pg.model.Vertex<V>> vertices;
  
  public final java.util.List<hydra.pg.model.Edge<V>> edges;
  
  public LazyGraph (java.util.List<hydra.pg.model.Vertex<V>> vertices, java.util.List<hydra.pg.model.Edge<V>> edges) {
    this.vertices = vertices;
    this.edges = edges;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LazyGraph)) {
      return false;
    }
    LazyGraph o = (LazyGraph) other;
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
  public int compareTo(LazyGraph other) {
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
  
  public LazyGraph withVertices(java.util.List<hydra.pg.model.Vertex<V>> vertices) {
    return new LazyGraph(vertices, edges);
  }
  
  public LazyGraph withEdges(java.util.List<hydra.pg.model.Edge<V>> edges) {
    return new LazyGraph(vertices, edges);
  }
}
