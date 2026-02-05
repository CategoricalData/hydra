// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An operator
 */
public abstract class Operator implements Serializable, Comparable<Operator> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.Operator");
  
  public static final hydra.core.Name FIELD_NAME_BACKTICK = new hydra.core.Name("backtick");
  
  public static final hydra.core.Name FIELD_NAME_NORMAL = new hydra.core.Name("normal");
  
  private Operator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Backtick instance) ;
    
    R visit(Normal instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Operator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Backtick instance) {
      return otherwise((instance));
    }
    
    default R visit(Normal instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * A function used as an infix operator
   */
  public static final class Backtick extends hydra.ext.haskell.ast.Operator implements Serializable {
    public final hydra.ext.haskell.ast.QualifiedName value;
    
    public Backtick (hydra.ext.haskell.ast.QualifiedName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Backtick)) {
        return false;
      }
      Backtick o = (Backtick) (other);
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
    public int compareTo(Operator other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Backtick o = (Backtick) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A normal infix operator
   */
  public static final class Normal extends hydra.ext.haskell.ast.Operator implements Serializable {
    public final hydra.ext.haskell.ast.QualifiedName value;
    
    public Normal (hydra.ext.haskell.ast.QualifiedName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Normal)) {
        return false;
      }
      Normal o = (Normal) (other);
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
    public int compareTo(Operator other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Normal o = (Normal) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
