package hydra.ext.tinkerpop.v3;

/**
 * A graph; a self-contained collection of vertices and edges
 */
public class Graph {
  public final java.util.Set<Vertex> vertices;
  
  public final java.util.Set<Edge> edges;
  
  public Graph (java.util.Set<Vertex> vertices, java.util.Set<Edge> edges) {
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
  
  public Graph withVertices(java.util.Set<Vertex> vertices) {
    return new Graph(vertices, edges);
  }
  
  public Graph withEdges(java.util.Set<Edge> edges) {
    return new Graph(vertices, edges);
  }
}