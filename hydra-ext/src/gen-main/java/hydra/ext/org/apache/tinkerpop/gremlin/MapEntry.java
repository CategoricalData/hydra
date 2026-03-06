// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class MapEntry implements Serializable, Comparable<MapEntry> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.MapEntry");
  
  public static final hydra.core.Name KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  private MapEntry () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Key instance) ;
    
    R visit(Value instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MapEntry instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Key instance) {
      return otherwise(instance);
    }
    
    default R visit(Value instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Key extends hydra.ext.org.apache.tinkerpop.gremlin.MapEntry implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.MapKey value;
    
    public Key (hydra.ext.org.apache.tinkerpop.gremlin.MapKey value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Key)) {
        return false;
      }
      Key o = (Key) other;
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
    public int compareTo(MapEntry other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Key o = (Key) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Value extends hydra.ext.org.apache.tinkerpop.gremlin.MapEntry implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral value;
    
    public Value (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Value)) {
        return false;
      }
      Value o = (Value) other;
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
    public int compareTo(MapEntry other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Value o = (Value) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
