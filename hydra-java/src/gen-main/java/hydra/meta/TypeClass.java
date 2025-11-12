// Note: this is an automatically generated file. Do not edit.

package hydra.meta;

import java.io.Serializable;

/**
 * Any of a small number of built-in type classes
 */
public abstract class TypeClass implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.meta.TypeClass");
  
  public static final hydra.core.Name FIELD_NAME_EQUALITY = new hydra.core.Name("equality");
  
  public static final hydra.core.Name FIELD_NAME_ORDERING = new hydra.core.Name("ordering");
  
  private TypeClass () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Equality instance) ;
    
    R visit(Ordering instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeClass instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Equality instance) {
      return otherwise((instance));
    }
    
    default R visit(Ordering instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Equality extends hydra.meta.TypeClass implements Serializable {
    public final Boolean value;
    
    public Equality (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Equality)) {
        return false;
      }
      Equality o = (Equality) (other);
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
  
  public static final class Ordering extends hydra.meta.TypeClass implements Serializable {
    public final Boolean value;
    
    public Ordering (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ordering)) {
        return false;
      }
      Ordering o = (Ordering) (other);
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
}
