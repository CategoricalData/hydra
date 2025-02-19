// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class UnmanagedType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.UnmanagedType");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_POINTER = new hydra.core.Name("pointer");
  
  private UnmanagedType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Value instance) ;
    
    R visit(Pointer instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnmanagedType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Value instance) {
      return otherwise((instance));
    }
    
    default R visit(Pointer instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Value extends hydra.ext.csharp.syntax.UnmanagedType implements Serializable {
    public final hydra.ext.csharp.syntax.ValueType value;
    
    public Value (hydra.ext.csharp.syntax.ValueType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Value)) {
        return false;
      }
      Value o = (Value) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Pointer extends hydra.ext.csharp.syntax.UnmanagedType implements Serializable {
    public final hydra.ext.csharp.syntax.PointerType value;
    
    public Pointer (hydra.ext.csharp.syntax.PointerType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pointer)) {
        return false;
      }
      Pointer o = (Pointer) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}