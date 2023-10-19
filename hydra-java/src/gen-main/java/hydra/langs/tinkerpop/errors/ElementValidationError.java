package hydra.langs.tinkerpop.errors;

import java.io.Serializable;

public abstract class ElementValidationError<T, V> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/errors.ElementValidationError");
  
  private ElementValidationError () {
  
  }
  
  public abstract <R> R accept(Visitor<T, V, R> visitor) ;
  
  public interface Visitor<T, V, R> {
    R visit(Vertex<T, V> instance) ;
    
    R visit(Edge<T, V> instance) ;
  }
  
  public interface PartialVisitor<T, V, R> extends Visitor<T, V, R> {
    default R otherwise(ElementValidationError<T, V> instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Vertex<T, V> instance) {
      return otherwise((instance));
    }
    
    default R visit(Edge<T, V> instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * A vertex validation error
   */
  public static final class Vertex<T, V> extends hydra.langs.tinkerpop.errors.ElementValidationError<T, V> implements Serializable {
    /**
     * A vertex validation error
     */
    public final hydra.langs.tinkerpop.errors.VertexValidationError<T, V> value;
    
    public Vertex (hydra.langs.tinkerpop.errors.VertexValidationError<T, V> value) {
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
    public <R> R accept(Visitor<T, V, R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * An edge validation error
   */
  public static final class Edge<T, V> extends hydra.langs.tinkerpop.errors.ElementValidationError<T, V> implements Serializable {
    /**
     * An edge validation error
     */
    public final hydra.langs.tinkerpop.errors.EdgeValidationError<T, V> value;
    
    public Edge (hydra.langs.tinkerpop.errors.EdgeValidationError<T, V> value) {
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
    public <R> R accept(Visitor<T, V, R> visitor) {
      return visitor.visit(this);
    }
  }
}