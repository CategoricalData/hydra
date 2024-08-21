// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.propertyGraph;

import java.io.Serializable;

/**
 * Either a vertex or an edge
 */
public abstract class Element<V> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/propertyGraph.Element");
  
  public static final hydra.core.Name FIELD_NAME_VERTEX = new hydra.core.Name("vertex");
  
  public static final hydra.core.Name FIELD_NAME_EDGE = new hydra.core.Name("edge");
  
  private Element () {
  
  }
  
  public abstract <R> R accept(Visitor<V, R> visitor) ;
  
  public interface Visitor<V, R> {
    R visit(Vertex<V> instance) ;
    
    R visit(Edge<V> instance) ;
  }
  
  public interface PartialVisitor<V, R> extends Visitor<V, R> {
    default R otherwise(Element<V> instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Vertex<V> instance) {
      return otherwise((instance));
    }
    
    default R visit(Edge<V> instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Vertex<V> extends hydra.ext.org.apache.tinkerpop.propertyGraph.Element<V> implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.propertyGraph.Vertex<V> value;
    
    public Vertex (hydra.ext.org.apache.tinkerpop.propertyGraph.Vertex<V> value) {
      java.util.Objects.requireNonNull((value));
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
    public <R> R accept(Visitor<V, R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Edge<V> extends hydra.ext.org.apache.tinkerpop.propertyGraph.Element<V> implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.propertyGraph.Edge<V> value;
    
    public Edge (hydra.ext.org.apache.tinkerpop.propertyGraph.Edge<V> value) {
      java.util.Objects.requireNonNull((value));
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
    public <R> R accept(Visitor<V, R> visitor) {
      return visitor.visit(this);
    }
  }
}