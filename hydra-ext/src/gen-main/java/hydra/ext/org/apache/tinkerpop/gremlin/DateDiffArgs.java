// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class DateDiffArgs implements Serializable, Comparable<DateDiffArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.DateDiffArgs");
  
  public static final hydra.core.Name TRAVERSAL = new hydra.core.Name("traversal");
  
  public static final hydra.core.Name DATE = new hydra.core.Name("date");
  
  private DateDiffArgs () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Traversal instance) ;
    
    R visit(Date instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DateDiffArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Traversal instance) {
      return otherwise(instance);
    }
    
    default R visit(Date instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Traversal extends hydra.ext.org.apache.tinkerpop.gremlin.DateDiffArgs implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value;
    
    public Traversal (hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Traversal)) {
        return false;
      }
      Traversal o = (Traversal) other;
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
    public int compareTo(DateDiffArgs other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Traversal o = (Traversal) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Date extends hydra.ext.org.apache.tinkerpop.gremlin.DateDiffArgs implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.DateArgument value;
    
    public Date (hydra.ext.org.apache.tinkerpop.gremlin.DateArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Date)) {
        return false;
      }
      Date o = (Date) other;
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
    public int compareTo(DateDiffArgs other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Date o = (Date) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
