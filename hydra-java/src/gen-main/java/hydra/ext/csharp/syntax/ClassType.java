// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ClassType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ClassType");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT = new hydra.core.Name("object");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  private ClassType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(TypeName instance) ;
    
    R visit(Object_ instance) ;
    
    R visit(String_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(TypeName instance) {
      return otherwise((instance));
    }
    
    default R visit(Object_ instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class TypeName extends hydra.ext.csharp.syntax.ClassType implements Serializable {
    public final hydra.ext.csharp.syntax.TypeName value;
    
    public TypeName (hydra.ext.csharp.syntax.TypeName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeName)) {
        return false;
      }
      TypeName o = (TypeName) (other);
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
  
  public static final class Object_ extends hydra.ext.csharp.syntax.ClassType implements Serializable {
    public Object_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Object_)) {
        return false;
      }
      Object_ o = (Object_) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class String_ extends hydra.ext.csharp.syntax.ClassType implements Serializable {
    public String_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}