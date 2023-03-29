package hydra.langs.tinkerpop.propertyGraph;

/**
 * A graph; a self-contained collection of vertices and edges
 */
public class Graph<V, E, P> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/propertyGraph.Graph");
  
  public final java.util.Map<V, hydra.langs.tinkerpop.propertyGraph.Vertex<V, P>> vertices;
  
  public final java.util.Map<E, hydra.langs.tinkerpop.propertyGraph.Edge<V, E, P>> edges;
  
  public Graph (java.util.Map<V, hydra.langs.tinkerpop.propertyGraph.Vertex<V, P>> vertices, java.util.Map<E, hydra.langs.tinkerpop.propertyGraph.Edge<V, E, P>> edges) {
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
  
  public Graph withVertices(java.util.Map<V, hydra.langs.tinkerpop.propertyGraph.Vertex<V, P>> vertices) {
    return new Graph(vertices, edges);
  }
  
  public Graph withEdges(java.util.Map<E, hydra.langs.tinkerpop.propertyGraph.Edge<V, E, P>> edges) {
    return new Graph(vertices, edges);
  }
}