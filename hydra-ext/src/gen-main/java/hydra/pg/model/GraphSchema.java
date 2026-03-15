// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * A graph schema; a vertex and edge types for the vertices and edges of a graph conforming to the schema
 */
public class GraphSchema<T> implements Serializable, Comparable<GraphSchema<T>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.GraphSchema");
  
  public static final hydra.core.Name VERTICES = new hydra.core.Name("vertices");
  
  public static final hydra.core.Name EDGES = new hydra.core.Name("edges");
  
  /**
   * A unique vertex type for each vertex label which may occur in a graph
   */
  public final hydra.util.PersistentMap<hydra.pg.model.VertexLabel, hydra.pg.model.VertexType<T>> vertices;
  
  /**
   * A unique edge type for each edge label which may occur in a graph
   */
  public final hydra.util.PersistentMap<hydra.pg.model.EdgeLabel, hydra.pg.model.EdgeType<T>> edges;
  
  public GraphSchema (hydra.util.PersistentMap<hydra.pg.model.VertexLabel, hydra.pg.model.VertexType<T>> vertices, hydra.util.PersistentMap<hydra.pg.model.EdgeLabel, hydra.pg.model.EdgeType<T>> edges) {
    this.vertices = vertices;
    this.edges = edges;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GraphSchema)) {
      return false;
    }
    GraphSchema o = (GraphSchema) other;
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
  public int compareTo(GraphSchema other) {
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
  
  public GraphSchema withVertices(hydra.util.PersistentMap<hydra.pg.model.VertexLabel, hydra.pg.model.VertexType<T>> vertices) {
    return new GraphSchema(vertices, edges);
  }
  
  public GraphSchema withEdges(hydra.util.PersistentMap<hydra.pg.model.EdgeLabel, hydra.pg.model.EdgeType<T>> edges) {
    return new GraphSchema(vertices, edges);
  }
}
