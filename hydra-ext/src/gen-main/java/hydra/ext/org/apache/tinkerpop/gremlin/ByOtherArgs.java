// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class ByOtherArgs implements Serializable, Comparable<ByOtherArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ByOtherArgs");
  
  public static final hydra.core.Name COMPARATOR = new hydra.core.Name("comparator");
  
  public static final hydra.core.Name OTHER = new hydra.core.Name("other");
  
  private ByOtherArgs () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Comparator instance) ;
    
    R visit(Other instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ByOtherArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Comparator instance) {
      return otherwise(instance);
    }
    
    default R visit(Other instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Comparator extends hydra.ext.org.apache.tinkerpop.gremlin.ByOtherArgs implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalComparatorArgument> value;
    
    public Comparator (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalComparatorArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Comparator)) {
        return false;
      }
      Comparator o = (Comparator) other;
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
    public int compareTo(ByOtherArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Comparator o = (Comparator) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Other extends hydra.ext.org.apache.tinkerpop.gremlin.ByOtherArgs implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal> value;
    
    public Other (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Other)) {
        return false;
      }
      Other o = (Other) other;
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
    public int compareTo(ByOtherArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Other o = (Other) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
