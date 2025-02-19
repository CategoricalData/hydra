// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * A vertex together with any outgoing and/or incoming edges; a vertex object
 */
public class VertexWithAdjacentEdges<V> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.model.VertexWithAdjacentEdges");
  
  public static final hydra.core.Name FIELD_NAME_VERTEX = new hydra.core.Name("vertex");
  
  public static final hydra.core.Name FIELD_NAME_INS = new hydra.core.Name("ins");
  
  public static final hydra.core.Name FIELD_NAME_OUTS = new hydra.core.Name("outs");
  
  /**
   * The focus vertex
   */
  public final hydra.pg.model.Vertex<V> vertex;
  
  /**
   * An adjacency list of edges in which the focus vertex is the head (in-vertex) of the edge
   */
  public final java.util.List<hydra.pg.model.AdjacentEdge<V>> ins;
  
  /**
   * An adjacency list of edges in which the focus vertex is the tail (out-vertex) of the edge
   */
  public final java.util.List<hydra.pg.model.AdjacentEdge<V>> outs;
  
  public VertexWithAdjacentEdges (hydra.pg.model.Vertex<V> vertex, java.util.List<hydra.pg.model.AdjacentEdge<V>> ins, java.util.List<hydra.pg.model.AdjacentEdge<V>> outs) {
    java.util.Objects.requireNonNull((vertex));
    java.util.Objects.requireNonNull((ins));
    java.util.Objects.requireNonNull((outs));
    this.vertex = vertex;
    this.ins = ins;
    this.outs = outs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexWithAdjacentEdges)) {
      return false;
    }
    VertexWithAdjacentEdges o = (VertexWithAdjacentEdges) (other);
    return vertex.equals(o.vertex) && ins.equals(o.ins) && outs.equals(o.outs);
  }
  
  @Override
  public int hashCode() {
    return 2 * vertex.hashCode() + 3 * ins.hashCode() + 5 * outs.hashCode();
  }
  
  public VertexWithAdjacentEdges withVertex(hydra.pg.model.Vertex<V> vertex) {
    java.util.Objects.requireNonNull((vertex));
    return new VertexWithAdjacentEdges(vertex, ins, outs);
  }
  
  public VertexWithAdjacentEdges withIns(java.util.List<hydra.pg.model.AdjacentEdge<V>> ins) {
    java.util.Objects.requireNonNull((ins));
    return new VertexWithAdjacentEdges(vertex, ins, outs);
  }
  
  public VertexWithAdjacentEdges withOuts(java.util.List<hydra.pg.model.AdjacentEdge<V>> outs) {
    java.util.Objects.requireNonNull((outs));
    return new VertexWithAdjacentEdges(vertex, ins, outs);
  }
}