// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class ClassDeclaration implements Serializable, Comparable<ClassDeclaration> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ClassDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_NORMAL = new hydra.core.Name("normal");
  
  public static final hydra.core.Name FIELD_NAME_ENUM = new hydra.core.Name("enum");
  
  private ClassDeclaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Normal instance) ;
    
    R visit(Enum_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Normal instance) {
      return otherwise(instance);
    }
    
    default R visit(Enum_ instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Normal extends hydra.ext.java.syntax.ClassDeclaration implements Serializable {
    public final hydra.ext.java.syntax.NormalClassDeclaration value;
    
    public Normal (hydra.ext.java.syntax.NormalClassDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Normal)) {
        return false;
      }
      Normal o = (Normal) other;
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
    public int compareTo(ClassDeclaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Normal o = (Normal) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Enum_ extends hydra.ext.java.syntax.ClassDeclaration implements Serializable {
    public final hydra.ext.java.syntax.EnumDeclaration value;
    
    public Enum_ (hydra.ext.java.syntax.EnumDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Enum_)) {
        return false;
      }
      Enum_ o = (Enum_) other;
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
    public int compareTo(ClassDeclaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Enum_ o = (Enum_) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
