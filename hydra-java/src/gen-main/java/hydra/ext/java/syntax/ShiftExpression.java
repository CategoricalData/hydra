// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class ShiftExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ShiftExpression");
  
  public static final hydra.core.Name FIELD_NAME_UNARY = new hydra.core.Name("unary");
  
  public static final hydra.core.Name FIELD_NAME_SHIFT_LEFT = new hydra.core.Name("shiftLeft");
  
  public static final hydra.core.Name FIELD_NAME_SHIFT_RIGHT = new hydra.core.Name("shiftRight");
  
  public static final hydra.core.Name FIELD_NAME_SHIFT_RIGHT_ZERO_FILL = new hydra.core.Name("shiftRightZeroFill");
  
  private ShiftExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Unary instance) ;
    
    R visit(ShiftLeft instance) ;
    
    R visit(ShiftRight instance) ;
    
    R visit(ShiftRightZeroFill instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ShiftExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Unary instance) {
      return otherwise((instance));
    }
    
    default R visit(ShiftLeft instance) {
      return otherwise((instance));
    }
    
    default R visit(ShiftRight instance) {
      return otherwise((instance));
    }
    
    default R visit(ShiftRightZeroFill instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Unary extends hydra.ext.java.syntax.ShiftExpression implements Serializable {
    public final hydra.ext.java.syntax.AdditiveExpression value;
    
    public Unary (hydra.ext.java.syntax.AdditiveExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unary)) {
        return false;
      }
      Unary o = (Unary) (other);
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
  
  public static final class ShiftLeft extends hydra.ext.java.syntax.ShiftExpression implements Serializable {
    public final hydra.ext.java.syntax.ShiftExpression_Binary value;
    
    public ShiftLeft (hydra.ext.java.syntax.ShiftExpression_Binary value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ShiftLeft)) {
        return false;
      }
      ShiftLeft o = (ShiftLeft) (other);
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
  
  public static final class ShiftRight extends hydra.ext.java.syntax.ShiftExpression implements Serializable {
    public final hydra.ext.java.syntax.ShiftExpression_Binary value;
    
    public ShiftRight (hydra.ext.java.syntax.ShiftExpression_Binary value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ShiftRight)) {
        return false;
      }
      ShiftRight o = (ShiftRight) (other);
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
  
  public static final class ShiftRightZeroFill extends hydra.ext.java.syntax.ShiftExpression implements Serializable {
    public final hydra.ext.java.syntax.ShiftExpression_Binary value;
    
    public ShiftRightZeroFill (hydra.ext.java.syntax.ShiftExpression_Binary value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ShiftRightZeroFill)) {
        return false;
      }
      ShiftRightZeroFill o = (ShiftRightZeroFill) (other);
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