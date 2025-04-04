// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * A graph schema; a vertex and edge types for the vertices and edges of a graph conforming to the schema
 */
public class GraphSchema<T> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.model.GraphSchema");
  
  public static final hydra.core.Name FIELD_NAME_VERTICES = new hydra.core.Name("vertices");
  
  public static final hydra.core.Name FIELD_NAME_EDGES = new hydra.core.Name("edges");
  
  /**
   * A unique vertex type for each vertex label which may occur in a graph
   */
  public final java.util.Map<hydra.pg.model.VertexLabel, hydra.pg.model.VertexType<T>> vertices;
  
  /**
   * A unique edge type for each edge label which may occur in a graph
   */
  public final java.util.Map<hydra.pg.model.EdgeLabel, hydra.pg.model.EdgeType<T>> edges;
  
  public GraphSchema (java.util.Map<hydra.pg.model.VertexLabel, hydra.pg.model.VertexType<T>> vertices, java.util.Map<hydra.pg.model.EdgeLabel, hydra.pg.model.EdgeType<T>> edges) {
    java.util.Objects.requireNonNull((vertices));
    java.util.Objects.requireNonNull((edges));
    this.vertices = vertices;
    this.edges = edges;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GraphSchema)) {
      return false;
    }
    GraphSchema o = (GraphSchema) (other);
    return vertices.equals(o.vertices) && edges.equals(o.edges);
  }
  
  @Override
  public int hashCode() {
    return 2 * vertices.hashCode() + 3 * edges.hashCode();
  }
  
  public GraphSchema withVertices(java.util.Map<hydra.pg.model.VertexLabel, hydra.pg.model.VertexType<T>> vertices) {
    java.util.Objects.requireNonNull((vertices));
    return new GraphSchema(vertices, edges);
  }
  
  public GraphSchema withEdges(java.util.Map<hydra.pg.model.EdgeLabel, hydra.pg.model.EdgeType<T>> edges) {
    java.util.Objects.requireNonNull((edges));
    return new GraphSchema(vertices, edges);
  }
}