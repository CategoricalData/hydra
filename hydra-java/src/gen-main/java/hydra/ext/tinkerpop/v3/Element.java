package hydra.ext.tinkerpop.v3;

/**
 * Either a vertex or an edge
 */
public abstract class Element {
  private Element () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Vertex instance) ;
    
    R visit(Edge instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Element instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Vertex instance) {
      return otherwise((instance));
    }
    
    default R visit(Edge instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Vertex extends hydra.ext.tinkerpop.v3.Element {
    public final hydra.ext.tinkerpop.v3.Vertex value;
    
    public Vertex (hydra.ext.tinkerpop.v3.Vertex value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Vertex)) {
        return false;
      }
      Vertex o = (Vertex) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Edge extends hydra.ext.tinkerpop.v3.Element {
    public final hydra.ext.tinkerpop.v3.Edge value;
    
    public Edge (hydra.ext.tinkerpop.v3.Edge value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Edge)) {
        return false;
      }
      Edge o = (Edge) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}