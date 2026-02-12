// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class WildcardBounds implements Serializable, Comparable<WildcardBounds> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.WildcardBounds");
  
  public static final hydra.core.Name FIELD_NAME_EXTENDS = new hydra.core.Name("extends");
  
  public static final hydra.core.Name FIELD_NAME_SUPER = new hydra.core.Name("super");
  
  private WildcardBounds () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Extends instance) ;
    
    R visit(Super instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(WildcardBounds instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Extends instance) {
      return otherwise(instance);
    }
    
    default R visit(Super instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Extends extends hydra.ext.java.syntax.WildcardBounds implements Serializable {
    public final hydra.ext.java.syntax.ReferenceType value;
    
    public Extends (hydra.ext.java.syntax.ReferenceType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Extends)) {
        return false;
      }
      Extends o = (Extends) other;
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
    public int compareTo(WildcardBounds other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Extends o = (Extends) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Super extends hydra.ext.java.syntax.WildcardBounds implements Serializable {
    public final hydra.ext.java.syntax.ReferenceType value;
    
    public Super (hydra.ext.java.syntax.ReferenceType value) {
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
    public int compareTo(WildcardBounds other) {
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
}
