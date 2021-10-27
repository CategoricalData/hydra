package hydra.ext.tinkerpop.v3;

/**
 * The type of a reference to a strongly-typed element (vertex or edge) by id
 */
public abstract class IdType {
  private IdType() {}
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  /**
   * An interface for applying a function to a IdType according to its variant (subclass)
   */
  public interface Visitor<R> {
    R visit(Vertex instance) ;
    
    R visit(Edge instance) ;
  }
  
  /**
   * An interface for applying a function to a IdType according to its variant (subclass). If a visit() method for a
   * particular variant is not implemented, a default method is used instead.
   */
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IdType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    @Override
    default R visit(Vertex instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Edge instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Vertex extends IdType {
    public final hydra.ext.tinkerpop.v3.VertexType vertex;
    
    /**
     * Constructs an immutable Vertex object
     */
    public Vertex(hydra.ext.tinkerpop.v3.VertexType vertex) {
      this.vertex = vertex;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Vertex)) {
          return false;
      }
      Vertex o = (Vertex) other;
      return vertex.equals(o.vertex);
    }
    
    @Override
    public int hashCode() {
      return 2 * vertex.hashCode();
    }
  }
  
  public static final class Edge extends IdType {
    public final hydra.ext.tinkerpop.v3.EdgeType edge;
    
    /**
     * Constructs an immutable Edge object
     */
    public Edge(hydra.ext.tinkerpop.v3.EdgeType edge) {
      this.edge = edge;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Edge)) {
          return false;
      }
      Edge o = (Edge) other;
      return edge.equals(o.edge);
    }
    
    @Override
    public int hashCode() {
      return 2 * edge.hashCode();
    }
  }
}
