// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

/**
 * A graph which does not assume that vertex or edge ids are unique. This is useful in mappings because the id specifications for vertices and/or edges may be non-unique.
 */
public class LazyGraph<V> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.model.LazyGraph");
  
  public static final hydra.core.Name FIELD_NAME_VERTICES = new hydra.core.Name("vertices");
  
  public static final hydra.core.Name FIELD_NAME_EDGES = new hydra.core.Name("edges");
  
  public final java.util.List<hydra.pg.model.Vertex<V>> vertices;
  
  public final java.util.List<hydra.pg.model.Edge<V>> edges;
  
  public LazyGraph (java.util.List<hydra.pg.model.Vertex<V>> vertices, java.util.List<hydra.pg.model.Edge<V>> edges) {
    java.util.Objects.requireNonNull((vertices));
    java.util.Objects.requireNonNull((edges));
    this.vertices = vertices;
    this.edges = edges;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LazyGraph)) {
      return false;
    }
    LazyGraph o = (LazyGraph) (other);
    return vertices.equals(o.vertices) && edges.equals(o.edges);
  }
  
  @Override
  public int hashCode() {
    return 2 * vertices.hashCode() + 3 * edges.hashCode();
  }
  
  public LazyGraph withVertices(java.util.List<hydra.pg.model.Vertex<V>> vertices) {
    java.util.Objects.requireNonNull((vertices));
    return new LazyGraph(vertices, edges);
  }
  
  public LazyGraph withEdges(java.util.List<hydra.pg.model.Edge<V>> edges) {
    java.util.Objects.requireNonNull((edges));
    return new LazyGraph(vertices, edges);
  }
}
