// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * An error of any kind, with kernel errors particularly differentiated
 */
public abstract class Error_ implements Serializable, Comparable<Error_> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.Error");
  
  public static final hydra.core.Name DECODING = new hydra.core.Name("decoding");
  
  public static final hydra.core.Name OTHER = new hydra.core.Name("other");
  
  public static final hydra.core.Name UNIFICATION = new hydra.core.Name("unification");
  
  private Error_ () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Decoding instance) ;
    
    R visit(Other instance) ;
    
    R visit(Unification instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Error_ instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Decoding instance) {
      return otherwise(instance);
    }
    
    default R visit(Other instance) {
      return otherwise(instance);
    }
    
    default R visit(Unification instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * An error that occurred during decoding of a term
   */
  public static final class Decoding extends hydra.error.Error_ implements Serializable {
    public final hydra.error.DecodingError value;
    
    public Decoding (hydra.error.DecodingError value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decoding)) {
        return false;
      }
      Decoding o = (Decoding) other;
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
    public int compareTo(Error_ other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Decoding o = (Decoding) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Any other error
   */
  public static final class Other extends hydra.error.Error_ implements Serializable {
    public final hydra.error.OtherError value;
    
    public Other (hydra.error.OtherError value) {
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
    public int compareTo(Error_ other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
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
  
  /**
   * A type unification error
   */
  public static final class Unification extends hydra.error.Error_ implements Serializable {
    public final hydra.error.UnificationError value;
    
    public Unification (hydra.error.UnificationError value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unification)) {
        return false;
      }
      Unification o = (Unification) other;
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
    public int compareTo(Error_ other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Unification o = (Unification) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
