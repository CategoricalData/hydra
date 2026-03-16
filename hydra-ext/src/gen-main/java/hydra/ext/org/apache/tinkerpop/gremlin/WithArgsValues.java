// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class WithArgsValues implements Serializable, Comparable<WithArgsValues> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.WithArgsValues");
  
  public static final hydra.core.Name WITH_OPTIONS = new hydra.core.Name("withOptions");
  
  public static final hydra.core.Name IO = new hydra.core.Name("io");
  
  public static final hydra.core.Name OBJECT = new hydra.core.Name("object");
  
  private WithArgsValues () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(WithOptions instance) ;
    
    R visit(Io instance) ;
    
    R visit(Object_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(WithArgsValues instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(WithOptions instance) {
      return otherwise(instance);
    }
    
    default R visit(Io instance) {
      return otherwise(instance);
    }
    
    default R visit(Object_ instance) {
      return otherwise(instance);
    }
  }
  
  public static final class WithOptions extends hydra.ext.org.apache.tinkerpop.gremlin.WithArgsValues implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.WithOptionsValues value;
    
    public WithOptions (hydra.ext.org.apache.tinkerpop.gremlin.WithOptionsValues value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithOptions)) {
        return false;
      }
      WithOptions o = (WithOptions) other;
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
    public int compareTo(WithArgsValues other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      WithOptions o = (WithOptions) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Io extends hydra.ext.org.apache.tinkerpop.gremlin.WithArgsValues implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.IoOptionsValues value;
    
    public Io (hydra.ext.org.apache.tinkerpop.gremlin.IoOptionsValues value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Io)) {
        return false;
      }
      Io o = (Io) other;
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
    public int compareTo(WithArgsValues other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Io o = (Io) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Object_ extends hydra.ext.org.apache.tinkerpop.gremlin.WithArgsValues implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Object_ (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Object_)) {
        return false;
      }
      Object_ o = (Object_) other;
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
    public int compareTo(WithArgsValues other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Object_ o = (Object_) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
