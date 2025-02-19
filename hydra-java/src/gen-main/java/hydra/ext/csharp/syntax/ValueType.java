// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ValueType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ValueType");
  
  public static final hydra.core.Name FIELD_NAME_NON_NULLABLE = new hydra.core.Name("nonNullable");
  
  public static final hydra.core.Name FIELD_NAME_NULLABLE = new hydra.core.Name("nullable");
  
  private ValueType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(NonNullable instance) ;
    
    R visit(Nullable instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ValueType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(NonNullable instance) {
      return otherwise((instance));
    }
    
    default R visit(Nullable instance) {
      return otherwise((instance));
    }
  }
  
  public static final class NonNullable extends hydra.ext.csharp.syntax.ValueType implements Serializable {
    public final hydra.ext.csharp.syntax.StructOrEnumType value;
    
    public NonNullable (hydra.ext.csharp.syntax.StructOrEnumType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NonNullable)) {
        return false;
      }
      NonNullable o = (NonNullable) (other);
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
  
  public static final class Nullable extends hydra.ext.csharp.syntax.ValueType implements Serializable {
    public final hydra.ext.csharp.syntax.StructOrEnumType value;
    
    public Nullable (hydra.ext.csharp.syntax.StructOrEnumType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nullable)) {
        return false;
      }
      Nullable o = (Nullable) (other);
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