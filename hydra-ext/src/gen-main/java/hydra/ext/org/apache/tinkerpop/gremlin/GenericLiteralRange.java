// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class GenericLiteralRange implements Serializable, Comparable<GenericLiteralRange> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralRange");
  
  public static final hydra.core.Name INTEGER = new hydra.core.Name("integer");
  
  public static final hydra.core.Name STRING = new hydra.core.Name("string");
  
  private GenericLiteralRange () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Integer_ instance) ;
    
    R visit(String_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GenericLiteralRange instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Integer_ instance) {
      return otherwise(instance);
    }
    
    default R visit(String_ instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Integer_ extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralRange implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.IntegerRange value;
    
    public Integer_ (hydra.ext.org.apache.tinkerpop.gremlin.IntegerRange value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integer_)) {
        return false;
      }
      Integer_ o = (Integer_) other;
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
    public int compareTo(GenericLiteralRange other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Integer_ o = (Integer_) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class String_ extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralRange implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringRange value;
    
    public String_ (hydra.ext.org.apache.tinkerpop.gremlin.StringRange value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) other;
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
    public int compareTo(GenericLiteralRange other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      String_ o = (String_) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
