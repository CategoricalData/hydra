// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * Either a vertex or an edge
 */
public abstract class Element<V> implements Serializable, Comparable<Element<V>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.Element");
  
  public static final hydra.core.Name VERTEX = new hydra.core.Name("vertex");
  
  public static final hydra.core.Name EDGE = new hydra.core.Name("edge");
  
  private Element () {
  
  }
  
  public abstract <R> R accept(Visitor<V, R> visitor) ;
  
  public interface Visitor<V, R> {
    R visit(Vertex<V> instance) ;
    
    R visit(Edge<V> instance) ;
  }
  
  public interface PartialVisitor<V, R> extends Visitor<V, R> {
    default R otherwise(Element<V> instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Vertex<V> instance) {
      return otherwise(instance);
    }
    
    default R visit(Edge<V> instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Vertex<V> extends hydra.pg.model.Element<V> implements Serializable {
    public final hydra.pg.model.Vertex<V> value;
    
    public Vertex (hydra.pg.model.Vertex<V> value) {
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
    public int compareTo(Element other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Vertex o = (Vertex) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<V, R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Edge<V> extends hydra.pg.model.Element<V> implements Serializable {
    public final hydra.pg.model.Edge<V> value;
    
    public Edge (hydra.pg.model.Edge<V> value) {
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
    public int compareTo(Element other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Edge o = (Edge) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<V, R> visitor) {
      return visitor.visit(this);
    }
  }
}
