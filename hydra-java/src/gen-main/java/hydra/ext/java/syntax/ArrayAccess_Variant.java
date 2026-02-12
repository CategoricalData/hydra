// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class ArrayAccess_Variant implements Serializable, Comparable<ArrayAccess_Variant> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ArrayAccess_Variant");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  private ArrayAccess_Variant () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Name instance) ;
    
    R visit(Primary instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ArrayAccess_Variant instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Name instance) {
      return otherwise(instance);
    }
    
    default R visit(Primary instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Name extends hydra.ext.java.syntax.ArrayAccess_Variant implements Serializable {
    public final hydra.ext.java.syntax.ExpressionName value;
    
    public Name (hydra.ext.java.syntax.ExpressionName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) other;
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
    public int compareTo(ArrayAccess_Variant other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Name o = (Name) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Primary extends hydra.ext.java.syntax.ArrayAccess_Variant implements Serializable {
    public final hydra.ext.java.syntax.PrimaryNoNewArray value;
    
    public Primary (hydra.ext.java.syntax.PrimaryNoNewArray value) {
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
    public int compareTo(ArrayAccess_Variant other) {
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
}
