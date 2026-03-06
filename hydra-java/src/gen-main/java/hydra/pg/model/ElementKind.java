// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * The kind of an element: vertex or edge
 */
public abstract class ElementKind implements Serializable, Comparable<ElementKind> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.ElementKind");
  
  public static final hydra.core.Name VERTEX = new hydra.core.Name("vertex");
  
  public static final hydra.core.Name EDGE = new hydra.core.Name("edge");
  
  private ElementKind () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Vertex instance) ;
    
    R visit(Edge instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ElementKind instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Vertex instance) {
      return otherwise(instance);
    }
    
    default R visit(Edge instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Vertex extends hydra.pg.model.ElementKind implements Serializable {
    public Vertex () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Vertex)) {
        return false;
      }
      Vertex o = (Vertex) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ElementKind other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Edge extends hydra.pg.model.ElementKind implements Serializable {
    public Edge () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Edge)) {
        return false;
      }
      Edge o = (Edge) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ElementKind other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
