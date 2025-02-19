// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class StructOrEnumType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.StructOrEnumType");
  
  public static final hydra.core.Name FIELD_NAME_STRUCT = new hydra.core.Name("struct");
  
  public static final hydra.core.Name FIELD_NAME_ENUM = new hydra.core.Name("enum");
  
  private StructOrEnumType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Struct instance) ;
    
    R visit(Enum_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StructOrEnumType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Struct instance) {
      return otherwise((instance));
    }
    
    default R visit(Enum_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Struct extends hydra.ext.csharp.syntax.StructOrEnumType implements Serializable {
    public final hydra.ext.csharp.syntax.StructType value;
    
    public Struct (hydra.ext.csharp.syntax.StructType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Struct)) {
        return false;
      }
      Struct o = (Struct) (other);
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
  
  public static final class Enum_ extends hydra.ext.csharp.syntax.StructOrEnumType implements Serializable {
    public final hydra.ext.csharp.syntax.EnumType value;
    
    public Enum_ (hydra.ext.csharp.syntax.EnumType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Enum_)) {
        return false;
      }
      Enum_ o = (Enum_) (other);
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