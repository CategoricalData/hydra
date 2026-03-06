// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class ChainedTraversalElement implements Serializable, Comparable<ChainedTraversalElement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement");
  
  public static final hydra.core.Name METHOD = new hydra.core.Name("method");
  
  public static final hydra.core.Name SELF = new hydra.core.Name("self");
  
  private ChainedTraversalElement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Method instance) ;
    
    R visit(Self instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ChainedTraversalElement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Method instance) {
      return otherwise(instance);
    }
    
    default R visit(Self instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Method extends hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod value;
    
    public Method (hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Method)) {
        return false;
      }
      Method o = (Method) other;
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
    public int compareTo(ChainedTraversalElement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Method o = (Method) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Self extends hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalSelfMethod value;
    
    public Self (hydra.ext.org.apache.tinkerpop.gremlin.TraversalSelfMethod value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Self)) {
        return false;
      }
      Self o = (Self) other;
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
    public int compareTo(ChainedTraversalElement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Self o = (Self) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
