// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class NumericType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.NumericType");
  
  public static final hydra.core.Name FIELD_NAME_INTEGRAL = new hydra.core.Name("integral");
  
  public static final hydra.core.Name FIELD_NAME_FLOATING_POINT = new hydra.core.Name("floatingPoint");
  
  private NumericType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Integral instance) ;
    
    R visit(FloatingPoint instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NumericType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Integral instance) {
      return otherwise((instance));
    }
    
    default R visit(FloatingPoint instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Integral extends hydra.ext.java.syntax.NumericType implements Serializable {
    public final hydra.ext.java.syntax.IntegralType value;
    
    public Integral (hydra.ext.java.syntax.IntegralType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integral)) {
        return false;
      }
      Integral o = (Integral) (other);
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
  
  public static final class FloatingPoint extends hydra.ext.java.syntax.NumericType implements Serializable {
    public final hydra.ext.java.syntax.FloatingPointType value;
    
    public FloatingPoint (hydra.ext.java.syntax.FloatingPointType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FloatingPoint)) {
        return false;
      }
      FloatingPoint o = (FloatingPoint) (other);
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
