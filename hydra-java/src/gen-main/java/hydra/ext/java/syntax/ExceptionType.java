// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class ExceptionType implements Serializable, Comparable<ExceptionType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ExceptionType");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
  private ExceptionType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Class_ instance) ;
    
    R visit(Variable instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ExceptionType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Class_ instance) {
      return otherwise(instance);
    }
    
    default R visit(Variable instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Class_ extends hydra.ext.java.syntax.ExceptionType implements Serializable {
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
    public int compareTo(ExceptionType other) {
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
  
  public static final class Variable extends hydra.ext.java.syntax.ExceptionType implements Serializable {
    public final hydra.ext.java.syntax.TypeVariable value;
    
    public Variable (hydra.ext.java.syntax.TypeVariable value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) other;
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
    public int compareTo(ExceptionType other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Variable o = (Variable) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
