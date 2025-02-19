// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class PointerType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.PointerType");
  
  public static final hydra.core.Name FIELD_NAME_VALUE_TYPE = new hydra.core.Name("valueType");
  
  public static final hydra.core.Name FIELD_NAME_POINTER_DEPTH = new hydra.core.Name("pointerDepth");
  
  private PointerType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ValueType instance) ;
    
    R visit(PointerDepth instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PointerType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ValueType instance) {
      return otherwise((instance));
    }
    
    default R visit(PointerDepth instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ValueType extends hydra.ext.csharp.syntax.PointerType implements Serializable {
    public final hydra.util.Opt<hydra.ext.csharp.syntax.ValueType> value;
    
    public ValueType (hydra.util.Opt<hydra.ext.csharp.syntax.ValueType> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ValueType)) {
        return false;
      }
      ValueType o = (ValueType) (other);
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
  
  public static final class PointerDepth extends hydra.ext.csharp.syntax.PointerType implements Serializable {
    public final Integer value;
    
    public PointerDepth (Integer value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PointerDepth)) {
        return false;
      }
      PointerDepth o = (PointerDepth) (other);
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