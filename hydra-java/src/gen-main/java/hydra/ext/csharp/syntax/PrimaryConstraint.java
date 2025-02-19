// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class PrimaryConstraint implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.PrimaryConstraint");
  
  public static final hydra.core.Name FIELD_NAME_CLASS_TYPE = new hydra.core.Name("classType");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name FIELD_NAME_STRUCT = new hydra.core.Name("struct");
  
  public static final hydra.core.Name FIELD_NAME_UNMANAGED = new hydra.core.Name("unmanaged");
  
  private PrimaryConstraint () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ClassType instance) ;
    
    R visit(Class_ instance) ;
    
    R visit(Struct instance) ;
    
    R visit(Unmanaged instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PrimaryConstraint instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ClassType instance) {
      return otherwise((instance));
    }
    
    default R visit(Class_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Struct instance) {
      return otherwise((instance));
    }
    
    default R visit(Unmanaged instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ClassType extends hydra.ext.csharp.syntax.PrimaryConstraint implements Serializable {
    public final hydra.ext.csharp.syntax.ClassType value;
    
    public ClassType (hydra.ext.csharp.syntax.ClassType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassType)) {
        return false;
      }
      ClassType o = (ClassType) (other);
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
  
  public static final class Class_ extends hydra.ext.csharp.syntax.PrimaryConstraint implements Serializable {
    public Class_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Class_)) {
        return false;
      }
      Class_ o = (Class_) (other);
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
  
  public static final class Struct extends hydra.ext.csharp.syntax.PrimaryConstraint implements Serializable {
    public Struct () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Struct)) {
        return false;
      }
      Struct o = (Struct) (other);
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
  
  public static final class Unmanaged extends hydra.ext.csharp.syntax.PrimaryConstraint implements Serializable {
    public Unmanaged () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unmanaged)) {
        return false;
      }
      Unmanaged o = (Unmanaged) (other);
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