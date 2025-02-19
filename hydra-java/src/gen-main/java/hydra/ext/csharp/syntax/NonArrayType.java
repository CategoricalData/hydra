// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class NonArrayType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NonArrayType");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name FIELD_NAME_INTERFACE = new hydra.core.Name("interface");
  
  public static final hydra.core.Name FIELD_NAME_DELEGATE = new hydra.core.Name("delegate");
  
  public static final hydra.core.Name FIELD_NAME_DYNAMIC = new hydra.core.Name("dynamic");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETER = new hydra.core.Name("parameter");
  
  public static final hydra.core.Name FIELD_NAME_POINTER = new hydra.core.Name("pointer");
  
  private NonArrayType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Value instance) ;
    
    R visit(Class_ instance) ;
    
    R visit(Interface instance) ;
    
    R visit(Delegate instance) ;
    
    R visit(Dynamic instance) ;
    
    R visit(Parameter instance) ;
    
    R visit(Pointer instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NonArrayType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Value instance) {
      return otherwise((instance));
    }
    
    default R visit(Class_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Interface instance) {
      return otherwise((instance));
    }
    
    default R visit(Delegate instance) {
      return otherwise((instance));
    }
    
    default R visit(Dynamic instance) {
      return otherwise((instance));
    }
    
    default R visit(Parameter instance) {
      return otherwise((instance));
    }
    
    default R visit(Pointer instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Value extends hydra.ext.csharp.syntax.NonArrayType implements Serializable {
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
  
  public static final class Class_ extends hydra.ext.csharp.syntax.NonArrayType implements Serializable {
    public final hydra.ext.csharp.syntax.ClassType value;
    
    public Class_ (hydra.ext.csharp.syntax.ClassType value) {
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
  
  public static final class Interface extends hydra.ext.csharp.syntax.NonArrayType implements Serializable {
    public final hydra.ext.csharp.syntax.InterfaceType value;
    
    public Interface (hydra.ext.csharp.syntax.InterfaceType value) {
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
  
  public static final class Delegate extends hydra.ext.csharp.syntax.NonArrayType implements Serializable {
    public final hydra.ext.csharp.syntax.DelegateType value;
    
    public Delegate (hydra.ext.csharp.syntax.DelegateType value) {
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
  
  public static final class Dynamic extends hydra.ext.csharp.syntax.NonArrayType implements Serializable {
    public Dynamic () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Dynamic)) {
        return false;
      }
      Dynamic o = (Dynamic) (other);
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
  
  public static final class Parameter extends hydra.ext.csharp.syntax.NonArrayType implements Serializable {
    public final hydra.ext.csharp.syntax.TypeParameter value;
    
    public Parameter (hydra.ext.csharp.syntax.TypeParameter value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parameter)) {
        return false;
      }
      Parameter o = (Parameter) (other);
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
  
  public static final class Pointer extends hydra.ext.csharp.syntax.NonArrayType implements Serializable {
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