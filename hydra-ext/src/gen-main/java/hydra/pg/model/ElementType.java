// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * The type of a vertex or edge
 */
public abstract class ElementType<T> implements Serializable, Comparable<ElementType<T>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.ElementType");
  
  public static final hydra.core.Name VERTEX = new hydra.core.Name("vertex");
  
  public static final hydra.core.Name EDGE = new hydra.core.Name("edge");
  
  private ElementType () {
  
  }
  
  public abstract <R> R accept(Visitor<T, R> visitor) ;
  
  public interface Visitor<T, R> {
    R visit(Vertex<T> instance) ;
    
    R visit(Edge<T> instance) ;
  }
  
  public interface PartialVisitor<T, R> extends Visitor<T, R> {
    default R otherwise(ElementType<T> instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Vertex<T> instance) {
      return otherwise(instance);
    }
    
    default R visit(Edge<T> instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Vertex<T> extends hydra.pg.model.ElementType<T> implements Serializable {
    public final hydra.pg.model.VertexType<T> value;
    
    public Vertex (hydra.pg.model.VertexType<T> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Vertex)) {
        return false;
      }
      Vertex o = (Vertex) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ElementType other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Vertex o = (Vertex) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<T, R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Edge<T> extends hydra.pg.model.ElementType<T> implements Serializable {
    public final hydra.pg.model.EdgeType<T> value;
    
    public Edge (hydra.pg.model.EdgeType<T> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Edge)) {
        return false;
      }
      Edge o = (Edge) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ElementType other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Edge o = (Edge) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<T, R> visitor) {
      return visitor.visit(this);
    }
  }
}
