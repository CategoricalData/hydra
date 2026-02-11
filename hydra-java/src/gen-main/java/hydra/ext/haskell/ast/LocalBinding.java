// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A local binding
 */
public abstract class LocalBinding implements Serializable, Comparable<LocalBinding> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.LocalBinding");
  
  public static final hydra.core.Name FIELD_NAME_SIGNATURE = new hydra.core.Name("signature");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  private LocalBinding () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Signature instance) ;
    
    R visit(Value instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LocalBinding instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Signature instance) {
      return otherwise(instance);
    }
    
    default R visit(Value instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * A type signature
   */
  public static final class Signature extends hydra.ext.haskell.ast.LocalBinding implements Serializable {
    public final hydra.ext.haskell.ast.TypeSignature value;
    
    public Signature (hydra.ext.haskell.ast.TypeSignature value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Signature)) {
        return false;
      }
      Signature o = (Signature) other;
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
    public int compareTo(LocalBinding other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Signature o = (Signature) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A value binding
   */
  public static final class Value extends hydra.ext.haskell.ast.LocalBinding implements Serializable {
    public final hydra.ext.haskell.ast.ValueBinding value;
    
    public Value (hydra.ext.haskell.ast.ValueBinding value) {
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
    public int compareTo(LocalBinding other) {
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
