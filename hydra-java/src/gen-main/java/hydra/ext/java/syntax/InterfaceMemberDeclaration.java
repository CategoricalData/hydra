// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class InterfaceMemberDeclaration implements Serializable, Comparable<InterfaceMemberDeclaration> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.InterfaceMemberDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_CONSTANT = new hydra.core.Name("constant");
  
  public static final hydra.core.Name FIELD_NAME_INTERFACE_METHOD = new hydra.core.Name("interfaceMethod");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name FIELD_NAME_INTERFACE = new hydra.core.Name("interface");
  
  private InterfaceMemberDeclaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Constant instance) ;
    
    R visit(InterfaceMethod instance) ;
    
    R visit(Class_ instance) ;
    
    R visit(Interface instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InterfaceMemberDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Constant instance) {
      return otherwise(instance);
    }
    
    default R visit(InterfaceMethod instance) {
      return otherwise(instance);
    }
    
    default R visit(Class_ instance) {
      return otherwise(instance);
    }
    
    default R visit(Interface instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Constant extends hydra.ext.java.syntax.InterfaceMemberDeclaration implements Serializable {
    public final hydra.ext.java.syntax.ConstantDeclaration value;
    
    public Constant (hydra.ext.java.syntax.ConstantDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Constant)) {
        return false;
      }
      Constant o = (Constant) other;
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
    public int compareTo(InterfaceMemberDeclaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Constant o = (Constant) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class InterfaceMethod extends hydra.ext.java.syntax.InterfaceMemberDeclaration implements Serializable {
    public final hydra.ext.java.syntax.InterfaceMethodDeclaration value;
    
    public InterfaceMethod (hydra.ext.java.syntax.InterfaceMethodDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InterfaceMethod)) {
        return false;
      }
      InterfaceMethod o = (InterfaceMethod) other;
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
    public int compareTo(InterfaceMemberDeclaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InterfaceMethod o = (InterfaceMethod) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Class_ extends hydra.ext.java.syntax.InterfaceMemberDeclaration implements Serializable {
    public final hydra.ext.java.syntax.ClassDeclaration value;
    
    public Class_ (hydra.ext.java.syntax.ClassDeclaration value) {
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
    public int compareTo(InterfaceMemberDeclaration other) {
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
  
  public static final class Interface extends hydra.ext.java.syntax.InterfaceMemberDeclaration implements Serializable {
    public final hydra.ext.java.syntax.InterfaceDeclaration value;
    
    public Interface (hydra.ext.java.syntax.InterfaceDeclaration value) {
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
    public int compareTo(InterfaceMemberDeclaration other) {
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
