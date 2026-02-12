// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class ClassOrInterfaceType implements Serializable, Comparable<ClassOrInterfaceType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ClassOrInterfaceType");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name FIELD_NAME_INTERFACE = new hydra.core.Name("interface");
  
  private ClassOrInterfaceType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Class_ instance) ;
    
    R visit(Interface instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassOrInterfaceType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Class_ instance) {
      return otherwise(instance);
    }
    
    default R visit(Interface instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Class_ extends hydra.ext.java.syntax.ClassOrInterfaceType implements Serializable {
    public final hydra.ext.java.syntax.ClassType value;
    
    public Class_ (hydra.ext.java.syntax.ClassType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Class_)) {
        return false;
      }
      Class_ o = (Class_) other;
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
    public int compareTo(ClassOrInterfaceType other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Class_ o = (Class_) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Interface extends hydra.ext.java.syntax.ClassOrInterfaceType implements Serializable {
    public final hydra.ext.java.syntax.InterfaceType value;
    
    public Interface (hydra.ext.java.syntax.InterfaceType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Interface)) {
        return false;
      }
      Interface o = (Interface) other;
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
    public int compareTo(ClassOrInterfaceType other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Interface o = (Interface) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
