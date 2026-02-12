// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class ClassTypeQualifier implements Serializable, Comparable<ClassTypeQualifier> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ClassTypeQualifier");
  
  public static final hydra.core.Name FIELD_NAME_NONE = new hydra.core.Name("none");
  
  public static final hydra.core.Name FIELD_NAME_PACKAGE = new hydra.core.Name("package");
  
  public static final hydra.core.Name FIELD_NAME_PARENT = new hydra.core.Name("parent");
  
  private ClassTypeQualifier () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(None instance) ;
    
    R visit(Package_ instance) ;
    
    R visit(Parent instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassTypeQualifier instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(None instance) {
      return otherwise(instance);
    }
    
    default R visit(Package_ instance) {
      return otherwise(instance);
    }
    
    default R visit(Parent instance) {
      return otherwise(instance);
    }
  }
  
  public static final class None extends hydra.ext.java.syntax.ClassTypeQualifier implements Serializable {
    public None () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof None)) {
        return false;
      }
      None o = (None) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ClassTypeQualifier other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Package_ extends hydra.ext.java.syntax.ClassTypeQualifier implements Serializable {
    public final hydra.ext.java.syntax.PackageName value;
    
    public Package_ (hydra.ext.java.syntax.PackageName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Package_)) {
        return false;
      }
      Package_ o = (Package_) other;
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
    public int compareTo(ClassTypeQualifier other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Package_ o = (Package_) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Parent extends hydra.ext.java.syntax.ClassTypeQualifier implements Serializable {
    public final hydra.ext.java.syntax.ClassOrInterfaceType value;
    
    public Parent (hydra.ext.java.syntax.ClassOrInterfaceType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parent)) {
        return false;
      }
      Parent o = (Parent) other;
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
    public int compareTo(ClassTypeQualifier other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parent o = (Parent) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
