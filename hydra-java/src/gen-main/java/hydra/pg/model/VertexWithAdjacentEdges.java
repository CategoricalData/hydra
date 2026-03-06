// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * A vertex together with any outgoing and/or incoming edges; a vertex object
 */
public class VertexWithAdjacentEdges<V> implements Serializable, Comparable<VertexWithAdjacentEdges<V>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.VertexWithAdjacentEdges");
  
  public static final hydra.core.Name VERTEX = new hydra.core.Name("vertex");
  
  public static final hydra.core.Name INS = new hydra.core.Name("ins");
  
  public static final hydra.core.Name OUTS = new hydra.core.Name("outs");
  
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
    this.vertex = vertex;
    this.ins = ins;
    this.outs = outs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexWithAdjacentEdges)) {
      return false;
    }
    VertexWithAdjacentEdges o = (VertexWithAdjacentEdges) other;
    return java.util.Objects.equals(
      this.vertex,
      o.vertex) && java.util.Objects.equals(
      this.ins,
      o.ins) && java.util.Objects.equals(
      this.outs,
      o.outs);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(vertex) + 3 * java.util.Objects.hashCode(ins) + 5 * java.util.Objects.hashCode(outs);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VertexWithAdjacentEdges other) {
    int cmp = 0;
    cmp = ((Comparable) vertex).compareTo(other.vertex);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      ins.hashCode(),
      other.ins.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      outs.hashCode(),
      other.outs.hashCode());
  }
  
  public VertexWithAdjacentEdges withVertex(hydra.pg.model.Vertex<V> vertex) {
    return new VertexWithAdjacentEdges(vertex, ins, outs);
  }
  
  public VertexWithAdjacentEdges withIns(java.util.List<hydra.pg.model.AdjacentEdge<V>> ins) {
    return new VertexWithAdjacentEdges(vertex, ins, outs);
  }
  
  public VertexWithAdjacentEdges withOuts(java.util.List<hydra.pg.model.AdjacentEdge<V>> outs) {
    return new VertexWithAdjacentEdges(vertex, ins, outs);
  }
}
