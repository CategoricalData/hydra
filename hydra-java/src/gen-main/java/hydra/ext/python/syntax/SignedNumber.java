// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class SignedNumber implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SignedNumber");
  
  public static final hydra.core.Name FIELD_NAME_SIGN = new hydra.core.Name("sign");
  
  public static final hydra.core.Name FIELD_NAME_NUMBER = new hydra.core.Name("number");
  
  private SignedNumber () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Sign instance) ;
    
    R visit(Number_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SignedNumber instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Sign instance) {
      return otherwise((instance));
    }
    
    default R visit(Number_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Sign extends hydra.ext.python.syntax.SignedNumber implements Serializable {
    public final hydra.ext.python.syntax.PlusOrMinus value;
    
    public Sign (hydra.ext.python.syntax.PlusOrMinus value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sign)) {
        return false;
      }
      Sign o = (Sign) (other);
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
  
  public static final class Number_ extends hydra.ext.python.syntax.SignedNumber implements Serializable {
    public final hydra.ext.python.syntax.Number_ value;
    
    public Number_ (hydra.ext.python.syntax.Number_ value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Number_)) {
        return false;
      }
      Number_ o = (Number_) (other);
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