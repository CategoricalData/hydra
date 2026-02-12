// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class Assignment implements Serializable, Comparable<Assignment> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Assignment");
  
  public static final hydra.core.Name FIELD_NAME_TYPED = new hydra.core.Name("typed");
  
  public static final hydra.core.Name FIELD_NAME_UNTYPED = new hydra.core.Name("untyped");
  
  public static final hydra.core.Name FIELD_NAME_AUG = new hydra.core.Name("aug");
  
  private Assignment () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Typed instance) ;
    
    R visit(Untyped instance) ;
    
    R visit(Aug instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Assignment instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Typed instance) {
      return otherwise(instance);
    }
    
    default R visit(Untyped instance) {
      return otherwise(instance);
    }
    
    default R visit(Aug instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Typed extends hydra.ext.python.syntax.Assignment implements Serializable {
    public final hydra.ext.python.syntax.TypedAssignment value;
    
    public Typed (hydra.ext.python.syntax.TypedAssignment value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Typed)) {
        return false;
      }
      Typed o = (Typed) other;
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
    public int compareTo(Assignment other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Typed o = (Typed) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Untyped extends hydra.ext.python.syntax.Assignment implements Serializable {
    public final hydra.ext.python.syntax.UntypedAssignment value;
    
    public Untyped (hydra.ext.python.syntax.UntypedAssignment value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Untyped)) {
        return false;
      }
      Untyped o = (Untyped) other;
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
    public int compareTo(Assignment other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Untyped o = (Untyped) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Aug extends hydra.ext.python.syntax.Assignment implements Serializable {
    public final hydra.ext.python.syntax.AugAssignment value;
    
    public Aug (hydra.ext.python.syntax.AugAssignment value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Aug)) {
        return false;
      }
      Aug o = (Aug) other;
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
    public int compareTo(Assignment other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Aug o = (Aug) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
