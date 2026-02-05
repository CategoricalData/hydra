// Note: this is an automatically generated file. Do not edit.

package hydra.util;

import java.io.Serializable;

/**
 * Numeric precision: arbitrary precision, or precision to a specified number of bits
 */
public abstract class Precision implements Serializable, Comparable<Precision> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.util.Precision");
  
  public static final hydra.core.Name FIELD_NAME_ARBITRARY = new hydra.core.Name("arbitrary");
  
  public static final hydra.core.Name FIELD_NAME_BITS = new hydra.core.Name("bits");
  
  private Precision () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Arbitrary instance) ;
    
    R visit(Bits instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Precision instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Arbitrary instance) {
      return otherwise((instance));
    }
    
    default R visit(Bits instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Arbitrary precision
   */
  public static final class Arbitrary extends hydra.util.Precision implements Serializable {
    public Arbitrary () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Arbitrary)) {
        return false;
      }
      Arbitrary o = (Arbitrary) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Precision other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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
  
  /**
   * Precision to a specified number of bits
   */
  public static final class Bits extends hydra.util.Precision implements Serializable {
    public final Integer value;
    
    public Bits (Integer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bits)) {
        return false;
      }
      Bits o = (Bits) (other);
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
    public int compareTo(Precision other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Bits o = (Bits) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
