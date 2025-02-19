// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class SimpleType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.SimpleType");
  
  public static final hydra.core.Name FIELD_NAME_NUMERIC = new hydra.core.Name("numeric");
  
  public static final hydra.core.Name FIELD_NAME_BOOL = new hydra.core.Name("bool");
  
  private SimpleType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Numeric instance) ;
    
    R visit(Bool instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SimpleType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Numeric instance) {
      return otherwise((instance));
    }
    
    default R visit(Bool instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Numeric extends hydra.ext.csharp.syntax.SimpleType implements Serializable {
    public final hydra.ext.csharp.syntax.NumericType value;
    
    public Numeric (hydra.ext.csharp.syntax.NumericType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Numeric)) {
        return false;
      }
      Numeric o = (Numeric) (other);
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
  
  public static final class Bool extends hydra.ext.csharp.syntax.SimpleType implements Serializable {
    public Bool () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bool)) {
        return false;
      }
      Bool o = (Bool) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}