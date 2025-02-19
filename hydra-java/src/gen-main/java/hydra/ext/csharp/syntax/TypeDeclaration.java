// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class TypeDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.TypeDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name FIELD_NAME_STRUCT = new hydra.core.Name("struct");
  
  public static final hydra.core.Name FIELD_NAME_INTERFACE = new hydra.core.Name("interface");
  
  public static final hydra.core.Name FIELD_NAME_ENUM = new hydra.core.Name("enum");
  
  public static final hydra.core.Name FIELD_NAME_DELEGATE = new hydra.core.Name("delegate");
  
  private TypeDeclaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Class_ instance) ;
    
    R visit(Struct instance) ;
    
    R visit(Interface instance) ;
    
    R visit(Enum_ instance) ;
    
    R visit(Delegate instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Class_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Struct instance) {
      return otherwise((instance));
    }
    
    default R visit(Interface instance) {
      return otherwise((instance));
    }
    
    default R visit(Enum_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Delegate instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Class_ extends hydra.ext.csharp.syntax.TypeDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.ClassDeclaration value;
    
    public Class_ (hydra.ext.csharp.syntax.ClassDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Class_)) {
        return false;
      }
      Class_ o = (Class_) (other);
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
  
  public static final class Struct extends hydra.ext.csharp.syntax.TypeDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.StructDeclaration value;
    
    public Struct (hydra.ext.csharp.syntax.StructDeclaration value) {
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
  
  public static final class Interface extends hydra.ext.csharp.syntax.TypeDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.InterfaceDeclaration value;
    
    public Interface (hydra.ext.csharp.syntax.InterfaceDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Interface)) {
        return false;
      }
      Interface o = (Interface) (other);
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
  
  public static final class Enum_ extends hydra.ext.csharp.syntax.TypeDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.EnumDeclaration value;
    
    public Enum_ (hydra.ext.csharp.syntax.EnumDeclaration value) {
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
  
  public static final class Delegate extends hydra.ext.csharp.syntax.TypeDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.DelegateDeclaration value;
    
    public Delegate (hydra.ext.csharp.syntax.DelegateDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Delegate)) {
        return false;
      }
      Delegate o = (Delegate) (other);
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