// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class MethodReference implements Serializable, Comparable<MethodReference> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MethodReference");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public static final hydra.core.Name FIELD_NAME_REFERENCE_TYPE = new hydra.core.Name("referenceType");
  
  public static final hydra.core.Name FIELD_NAME_SUPER = new hydra.core.Name("super");
  
  public static final hydra.core.Name FIELD_NAME_NEW = new hydra.core.Name("new");
  
  public static final hydra.core.Name FIELD_NAME_ARRAY = new hydra.core.Name("array");
  
  private MethodReference () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Expression instance) ;
    
    R visit(Primary instance) ;
    
    R visit(ReferenceType instance) ;
    
    R visit(Super instance) ;
    
    R visit(New instance) ;
    
    R visit(Array instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MethodReference instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Expression instance) {
      return otherwise(instance);
    }
    
    default R visit(Primary instance) {
      return otherwise(instance);
    }
    
    default R visit(ReferenceType instance) {
      return otherwise(instance);
    }
    
    default R visit(Super instance) {
      return otherwise(instance);
    }
    
    default R visit(New instance) {
      return otherwise(instance);
    }
    
    default R visit(Array instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Expression extends hydra.ext.java.syntax.MethodReference implements Serializable {
    public final hydra.ext.java.syntax.MethodReference_Expression value;
    
    public Expression (hydra.ext.java.syntax.MethodReference_Expression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Expression)) {
        return false;
      }
      Expression o = (Expression) other;
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
    public int compareTo(MethodReference other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Expression o = (Expression) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Primary extends hydra.ext.java.syntax.MethodReference implements Serializable {
    public final hydra.ext.java.syntax.MethodReference_Primary value;
    
    public Primary (hydra.ext.java.syntax.MethodReference_Primary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primary)) {
        return false;
      }
      Primary o = (Primary) other;
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
    public int compareTo(MethodReference other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Primary o = (Primary) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ReferenceType extends hydra.ext.java.syntax.MethodReference implements Serializable {
    public final hydra.ext.java.syntax.MethodReference_ReferenceType value;
    
    public ReferenceType (hydra.ext.java.syntax.MethodReference_ReferenceType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ReferenceType)) {
        return false;
      }
      ReferenceType o = (ReferenceType) other;
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
    public int compareTo(MethodReference other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ReferenceType o = (ReferenceType) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Super extends hydra.ext.java.syntax.MethodReference implements Serializable {
    public final hydra.ext.java.syntax.MethodReference_Super value;
    
    public Super (hydra.ext.java.syntax.MethodReference_Super value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Super)) {
        return false;
      }
      Super o = (Super) other;
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
    public int compareTo(MethodReference other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Super o = (Super) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class New extends hydra.ext.java.syntax.MethodReference implements Serializable {
    public final hydra.ext.java.syntax.MethodReference_New value;
    
    public New (hydra.ext.java.syntax.MethodReference_New value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof New)) {
        return false;
      }
      New o = (New) other;
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
    public int compareTo(MethodReference other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      New o = (New) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Array extends hydra.ext.java.syntax.MethodReference implements Serializable {
    public final hydra.ext.java.syntax.MethodReference_Array value;
    
    public Array (hydra.ext.java.syntax.MethodReference_Array value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Array)) {
        return false;
      }
      Array o = (Array) other;
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
    public int compareTo(MethodReference other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Array o = (Array) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
